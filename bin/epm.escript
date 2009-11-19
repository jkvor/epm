#!/usr/bin/env escript

%% -----------------------------------------------------------------------------
%% main function
%% -----------------------------------------------------------------------------
main(Args) ->
	case os:find_executable("git") of
		false ->
			io:format("- failed to locate git executable~n");
		_ ->
			inets:start(),
			
			%% consult global .epm config file in home directory
			Home = 
				case init:get_argument(home) of
					{ok, [[H]]} -> [H];
					_ -> []
				end,
		    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".epm") of
				{ok, [GlobalConfig], _} ->
					execute(GlobalConfig, Args);
				{error, enoent} ->
					execute([], Args);
				{error, Reason} ->
					io:format("- failed to read epm global config: ~p~n", [Reason])
			end
	end.
	
%% -----------------------------------------------------------------------------
%% execute function
%% -----------------------------------------------------------------------------
execute(GlobalConfig, ["install" | Args]) ->
    CollectedArgs = collect_args(install, Args),
	lists:foldl(
		fun ({{User, ProjectName}, CommandLineTags}, ok) ->
				case package_info(ProjectName) of
		            {error, not_found} ->
						install_package(GlobalConfig, User, ProjectName, CommandLineTags);
					{error, Reason} ->
						io:format("- there was a problem with the installed version of ~s: ~p~n", [ProjectName, Reason]),
						install_package(GlobalConfig, User, ProjectName, CommandLineTags);
		            {ok, _Version} ->
						io:format("+ skipping ~s: already installed~n", [ProjectName]),
						ok
		        end;
			(_, _) ->
				stop
		end, ok, CollectedArgs);

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
                {ok, [{application, _, AppContents}]} -> {ok, proplists:get_value(vsn, AppContents)};
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

install_package(GlobalConfig, User, ProjectName, CommandLineTags) ->
	io:format("~s~n", [string:copies("+", 80)]),
	io:format("++~s++~n", [string:centre("installing " ++ ProjectName, 76, $ )]),
	io:format("~s~n", [string:copies("+", 80)]),
	checkout_package(GlobalConfig, User, ProjectName, CommandLineTags).
	
checkout_package(GlobalConfig, User, ProjectName, CommandLineTags) ->
	Paths = proplists:get_value(git_paths, GlobalConfig, ["git://github.com/<user>/<project>.git"]),
	case search_sources_for_project(Paths, User, ProjectName) of
		undefined ->
			io:format("- failed to locate remote repo for ~s~n", [ProjectName]),
			stop;
		GitUrl ->
			BuildPath = proplists:get_value(build_path, GlobalConfig, "."),
			case file:set_cwd(BuildPath) of
				ok ->
					delete_dir(ProjectName), %% delete dir if it already exists
					case os:cmd("git clone " ++ GitUrl ++ " " ++ ProjectName) of
						"Initialized empty Git repository" ++ _ = Result ->
							io:format("+ checking out ~s~n", [GitUrl]),
							io:format("~s~n", [Result]),
							case file:set_cwd(ProjectName) of
								ok ->
									Return = case checkout_correct_version(CommandLineTags) of
										ok ->
											case install_dependencies(GlobalConfig, ProjectName) of
												ok ->
													case build_project(GlobalConfig, ProjectName, CommandLineTags) of
														ok ->
															ok;
														_ ->
															stop
													end;
												_ ->
													stop
											end;
										_ ->
											stop
									end,
									delete_dir(ProjectName),
									Return;
								{error, _} ->
									io:format("- failed to change working directory: ~s~n", [ProjectName]),
									stop
							end;
						Other ->
							io:format("- failed to checkout ~s~n", [GitUrl]),
							io:format("~s~n", [Other]),
							stop
					end;
				{error, _} ->
					io:format("- failed to change working directory: ~s~n", [BuildPath]),
					stop
			end
	end.

search_sources_for_project(["git://github.com" ++ _ | _Tail], none, ProjectName) ->	
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
					Username = binary_to_list(proplists:get_value(<<"username">>, Repo)),
					RepoName = binary_to_list(proplists:get_value(<<"name">>, Repo)),
					"git://github.com/" ++ Username ++ "/" ++ RepoName ++ ".git";
				[] -> undefined
			end;
		_ -> undefined
	end;
	
search_sources_for_project(["git://github.com" ++ _ | _Tail], User, ProjectName) ->
	case githubby:repos_info({undefined, undefined}, User, ProjectName) of
		{struct,[{<<"repository">>, {struct, Repo}}]} ->
			Username = binary_to_list(proplists:get_value(<<"owner">>, Repo)),
			RepoName = binary_to_list(proplists:get_value(<<"name">>, Repo)),
			"git://github.com/" ++ Username ++ "/" ++ RepoName ++ ".git";
		_ -> undefined
	end;

search_sources_for_project(_, _, _) ->
	io:format("- currently github is the only supported remote repository~n").
	
checkout_correct_version([{tag, Tag}|_]) ->
	case os:cmd("git checkout -b \"" ++ Tag ++ "\" \"" ++ Tag ++ "\"") of
		"Switched to a new branch" ++ _ -> ok;
		Other ->
			io:format("- failed to switch to tag ~s: ~p~n", [Tag, Other]),
			stop
	end;
	
checkout_correct_version([{branch, Branch}|_]) ->
	case os:cmd("git checkout -b \"" ++ Branch ++ "\"") of
		"Switched to a new branch" ++ _ -> ok;
		Other ->
			io:format("- failed to switch to branch ~s: ~p~n", [Branch, Other]),
			stop
	end;
	
checkout_correct_version([{sha, Sha}|_]) ->
	case os:cmd("git checkout -b \"" ++ Sha ++ "\"") of
		"Switched to a new branch" ++ _ -> ok;
		Other ->
			io:format("- failed to switch to sha ~s: ~p~n", [Sha, Other]),
			stop
	end;
		
checkout_correct_version([_|Tail]) ->	
	checkout_correct_version(Tail);
	
checkout_correct_version(_) -> ok.
	
install_dependencies(GlobalConfig, ProjectName) ->
	case file:consult(ProjectName ++ ".epm") of
		{ok, [Config]} ->
			lists:foldl(
				fun ({Project, CommandLineTags}, ok) ->
						{ProjectName1, User} = split_package(Project),
						install_package(GlobalConfig, User, ProjectName1, CommandLineTags);
					(_, _) ->
						stop
				end, ok, proplists:get_value(deps, Config));
		{error, Reason} ->
			io:format("- failed to read ~s.epm config: ~p~n", [ProjectName, Reason]),
			stop
	end.
	
build_project(GlobalConfig, ProjectName, _CommandLineTags) ->
	case file:consult(ProjectName ++ ".epm") of
		{ok, [Config]} ->
			case proplists:get_value(install_path, GlobalConfig) of
				undefined -> ok;
				Path -> os:putenv("ERL_LIBS", Path)
			end,
			
			case proplists:get_value(prebuild_command, Config) of
				undefined -> ok;
				PrebuildCmd -> 
					io:format("+ prebuild_command: ~s~n", [PrebuildCmd]),
					io:format("~s~n", [os:cmd(PrebuildCmd)])
			end,
			
			BuildCmd = proplists:get_value(build_command, Config, "make"),
			io:format("+ build_command: ~s~n", [BuildCmd]),
			io:format("~s~n", [os:cmd(BuildCmd)]),
			
			TestCmd = proplists:get_value(test_command, Config, "make test"),
			io:format("+ test_command: ~s~n", [TestCmd]),
			io:format("~s~n", [os:cmd(TestCmd)]),
			
			InstallCmd = proplists:get_value(install_command, Config, "make install"),
			io:format("+ install_command: ~s~n", [InstallCmd]),
			io:format("~s~n", [os:cmd(InstallCmd)]),
	
			ok;
		{error, Reason} ->
			io:format("- failed to read ~s.epm config: ~p~n", [ProjectName, Reason]),
			stop
	end.
		
delete_dir(Dir) ->
	case file:list_dir(Dir) of
		{ok, Files} ->
			[begin
				case file:delete(Dir ++ "/" ++ Filename) of
					ok -> ok;
					{error, eperm} ->
						case file:del_dir(Dir ++ "/" ++ Filename) of
							ok -> ok;
							{error, eexist} ->
								delete_dir(Dir ++ "/" ++ Filename)
						end
				end
			end || Filename <- Files],
			file:del_dir(Dir);
		_ ->
			ok
	end.
	
	
	
