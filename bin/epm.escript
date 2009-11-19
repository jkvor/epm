#!/usr/bin/env escript

%% -----------------------------------------------------------------------------
%% main function
%% -----------------------------------------------------------------------------
main(Args) ->
	inets:start(),
	
	%% consult global .epm config file in home directory
	Home = 
		case init:get_argument(home) of
			{ok, [[H]]} -> [H];
			_ -> []
		end,
    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".epm") of
		{ok, [GlobalConfig], _} ->
			%% ensure that required fields exist in global config
			case validate_global_config(GlobalConfig) of
				true -> execute(GlobalConfig, Args);
				false -> ok
			end;
		{error, enoent} ->
			io:format("- failed to read epm global config: file does not exist~n");
		{error, Reason} ->
			io:format("- failed to read epm global config: ~p~n", [Reason])
	end.
	
%% -----------------------------------------------------------------------------
%% execute function
%% -----------------------------------------------------------------------------
execute(GlobalConfig, ["install" | Args]) ->
    CollectedArgs = collect_args(install, Args),
	[begin
		case package_info(ProjectName) of
            {error, not_found} ->
				install_package(GlobalConfig, User, ProjectName, CommandLineTags);
			{error, Reason} ->
				io:format("- there was a problem with the installed version of ~s: ~p~n", [ProjectName, Reason]),
				install_package(GlobalConfig, User, ProjectName, CommandLineTags);
            {ok, _Version} ->
				io:format("+ skipping ~s: already installed~n", [ProjectName])
        end
	end || {{User, ProjectName}, CommandLineTags} <- CollectedArgs],
    ok;

execute(_GlobalConfig, ["info" | Args]) ->
    io:format("info ~p~n", [collect_args(info, Args)]),
    ok;

execute(_GlobalConfig, ["remove" | Args]) ->
    io:format("remove ~p~n", [collect_args(remove, Args)]),
    ok;

execute(_, _) ->
    io:format("epm v0.0.1, 2009 Nick Gerakines, Jacob Vorreuter~n"),
    io:format("Usage: epm command arguments~n"),
    io:format("    install [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>] [--force]~n"),
    io:format("    remove <project>~n"),
    io:format("    info [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>]~n"),
	io:format("    test [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>]~n"),
    ok.

%% -----------------------------------------------------------------------------
%% internal functions
%% -----------------------------------------------------------------------------
collect_args(Target, Args) ->
    collect_args(Target, Args, []).

collect_args(_, [], Acc) -> lists:reverse(Acc);
	
collect_args(Target, [Arg | Rest], Acc) ->
	case parse_tag(Target, Arg) of
		undefined -> %% if not a tag then must be a project name
			{ProjectName, User} = split_package(Arg), %% split into user and project
			collect_args(Target, Rest, [{{User, ProjectName}, []} | Acc]);
		{Tag, true} -> %% tag with trailing value
			[Value | Rest1] = Rest, %% pop trailing value from front of remaining args
			[{Project, Props} | Acc1] = Acc, %% this tag applies to the last project on the stack
			collect_args(Target, Rest1, [{Project, Props ++ [{Tag, Value}]} | Acc1]);
		{Tag, false} ->	 %% tag with no trailing value
			[{Project, Props} | Acc1] = Acc, %% this tag applies to the last project on the stack
			collect_args(Target, Rest, [{Project, Props ++ [Tag]} | Acc1])
	end.

split_package(Raw) -> split_package(Raw, []).
split_package([], Package) -> {Package, none};
split_package([47 | Package], User) -> {Package, User};
split_package([A | Tail], User) -> split_package(Tail, User ++ [A]).

package_info(Package) when is_list(Package) ->
    package_info(list_to_atom(Package));
package_info(Package) ->
    case code:lib_dir(Package) of
        {error, bad_name} -> {error, not_found};
        Path ->
            case file:consult(Path ++ "/ebin/" ++ atom_to_list(Package) ++ ".app") of
                {ok, [application, Package, AppContents]} -> {ok, proplists:get_value(vsn, AppContents)};
                _ -> {error, no_app_file}
            end
    end.

%% @spec parse_tag(Target, Arg) -> {Tag, HasValue} | undefined
%%		 Target = atom()
%%		 Arg = string()
%%		 Tag = atom()
%%		 HasValue = bool()
parse_tag(install, "--tag") -> {tag, true};
parse_tag(install, "--branch") -> {branch, true};
parse_tag(install, "--sha") -> {sha, true};
parse_tag(install, "--force") -> {force, false};
parse_tag(_, _) -> undefined.

validate_global_config(GlobalConfig) ->
	validate_global_config(GlobalConfig, [git_paths, build_path, install_path]).
	
validate_global_config(_, []) -> true;
validate_global_config(GlobalConfig, [Value|Rest]) ->
	case proplists:get_value(Value, GlobalConfig) of
		undefined -> 
			io:format("- missing value in global epm config: ~p~n", [Value]), 
			false;
		_ -> 
			validate_global_config(GlobalConfig, Rest)
	end.

install_package(GlobalConfig, User, ProjectName, CommandLineTags) ->
	io:format("+ installing ~s...~n", [ProjectName]),
	checkout_package(GlobalConfig, User, ProjectName, CommandLineTags),
	ok.
	
checkout_package(GlobalConfig, User, ProjectName, CommandLineTags) ->
	Paths = proplists:get_value(git_paths, GlobalConfig),
	case search_sources_for_project(Paths, User, ProjectName, CommandLineTags) of
		undefined ->
			io:format("- failed to locate remote repo for ~s~n", [ProjectName]);
		GitUrl ->
			io:format("+ checking out ~s~n", [GitUrl])
	end,
	ok.

search_sources_for_project(GitPaths, none, ProjectName, CommandLineTags) ->	
	case githubby:repos_search({undefined, undefined}, ProjectName) of
		{struct,[{<<"repositories">>, Repos}]} ->
			Filtered = lists:filter(
				fun({struct, Props}) ->
					(proplists:get_value(<<"name">>, Props) == list_to_binary(ProjectName)) andalso
					(proplists:get_value(<<"language">>, Props) == <<"Erlang">>) andalso
					(proplists:get_value(<<"type">>, Props) == <<"repo">>)
				end, Repos),
			case Filtered of
				[{struct, Repo}|_] -> 
					search_sources_for_project(
						GitPaths, 
						binary_to_list(proplists:get_value(<<"username">>, Repo)), 
						ProjectName, 
						CommandLineTags
					);
				[] -> undefined
			end;
		_ -> undefined
	end;
	
search_sources_for_project(_GitPaths, User, ProjectName, _CommandLineTags) ->
	case githubby:repos_info({undefined, undefined}, User, ProjectName) of
		{struct,[{<<"repository">>, {struct, Repo}}]} ->
			proplists:get_value(<<"url">>, Repo);
		_ -> undefined
	end.
		