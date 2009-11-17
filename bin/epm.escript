#!/usr/bin/env escript

%% -----------------------------------------------------------------------------
%% main function
%% -----------------------------------------------------------------------------
main(Args) ->
	Home = 
		case init:get_argument(home) of
			{ok, [[H]]} -> [H];
			_ -> []
		end,
    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".epm") of
		{ok, Terms, _} ->
			execute(Terms, Args);
		{error, enoent} ->
			io:format("- failed to read epm global config: file does not exist~n");
		{error, Reason} ->
			io:format("- failed to read epm global config: ~p~n", [Reason])
	end.
	
%% -----------------------------------------------------------------------------
%% execute function
%% -----------------------------------------------------------------------------
execute(Terms, ["install" | Args]) ->
    CollectedArgs = collect_args(install, Args),
	[begin
		{Name, User} = split_package(Package),
		case package_info(Name) of
            {error, not_found} ->
				install_package(Name, User, Props);
			{error, Reason} ->
				io:format("- there was a problem with the installed version of ~s: ~p~n", [Name, Reason]),
				install_package(Name, User, Props);
            {ok, Version} ->
				io:format("+ skipping ~s: already installed~n", [Name])
        end
	end || {Package, Props} <- CollectedArgs],
    ok;

execute(Terms, ["info" | Args]) ->
    io:format("info ~p~n", [collect_args(info, Args)]),
    ok;

execute(Terms, ["remove" | Args]) ->
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
			collect_args(Target, Rest, [{Arg, []} | Acc]);
		{Tag, true} -> %% tag with trailing value
			[Value | Rest1] = Rest, %% pop trailing value from front of remaining args
			[{ProjectName, Props} | Acc1] = Acc, %% this tag applies to the last project on the stack
			collect_args(Target, Rest1, [{ProjectName, Props ++ [{Tag, Value}]} | Acc1]);
		{Tag, false} ->	 %% tag with no trailing value
			[{ProjectName, Props} | Acc1] = Acc, %% this tag applies to the last project on the stack
			collect_args(Target, Rest, [{ProjectName, Props ++ [Tag]} | Acc1])
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

install_package(Name, User, Props) ->
	io:format("+ installing ~s...~n", [Name]),
	
	ok.