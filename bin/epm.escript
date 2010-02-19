#!/usr/bin/env escript

%% -----------------------------------------------------------------------------
%% main function
%% -----------------------------------------------------------------------------
main(Args) ->
	case (catch main1(Args)) of
		{'EXIT', ErrorMsg} when is_list(ErrorMsg) ->
			io:format("- ~s~n", [ErrorMsg]);
		{'EXIT', Other} ->
			io:format("~p~n", [Other]);
		_ ->
			ok
	end.
	
main1(Args) ->
	%ensure_erlang_vsn(),
	ensure_git(),
	
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
	end.
	
%% -----------------------------------------------------------------------------
%% execute function
%% -----------------------------------------------------------------------------
execute(GlobalConfig, ["install" | Args]) ->
    CollectedArgs = collect_args(install, Args),
	[case package_info(ProjectName) of
        {error, not_found} ->
			install_package(GlobalConfig, User, ProjectName, CommandLineTags);
		{error, Reason} ->
			io:format("- there was a problem with the installed version of ~s: ~p~n", [ProjectName, Reason]),
			install_package(GlobalConfig, User, ProjectName, CommandLineTags);
        {ok, Version} ->
			io:format("+ skipping ~s: already installed (~p)~n", [ProjectName, Version])
    end || {{User, ProjectName}, CommandLineTags} <- CollectedArgs];

execute(_GlobalConfig, ["info" | Args]) ->
    io:format("info ~p~n", [collect_args(info, Args)]),
    ok;

execute(_GlobalConfig, ["remove" | Args]) ->
    io:format("remove ~p~n", [collect_args(remove, Args)]),
    ok;

execute(_, _) ->
    io:format("epm v0.0.1, 2010 Nick Gerakines, Jacob Vorreuter~n"),
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
	io:format("+ >> ~s~n", [ProjectName]),

	%% switch to build home dir
	set_cwd_build_home(GlobalConfig),
	
	%% clone repo
	LocalProjectDir = checkout_package(GlobalConfig, User, ProjectName),
	
	%% switch to project dir
	set_cwd_build_home(GlobalConfig),
	set_cwd(LocalProjectDir),
	
	%% ensure correct branch, tag or sha is checked out
	checkout_correct_version(CommandLineTags),
	
	%% install dependencies from .epm file
	install_dependencies(GlobalConfig, ProjectName),
	
	io:format("~s~n", [string:copies("+", 80)]),
	io:format("++~s++~n", [string:centre("installing " ++ ProjectName, 76, $ )]),
	io:format("~s~n", [string:copies("+", 80)]),
	
	%% switch to project dir
	set_cwd_build_home(GlobalConfig),
	set_cwd(LocalProjectDir),
	
	%% build/install project
	build_project(GlobalConfig, ProjectName, CommandLineTags),
	
	%% switch to build home dir and delete cloned project
	set_cwd_build_home(GlobalConfig),
	del_dir(LocalProjectDir),
	
	ok.

checkout_package(GlobalConfig, User, ProjectName) ->
	Paths = proplists:get_value(git_paths, GlobalConfig, ["git://github.com/<user>/<project>.git"]),
	{LocalProjectDir, GitUrl} = search_sources_for_project(Paths, User, ProjectName),
	del_dir(LocalProjectDir), %% delete dir if it already exists
	io:format("+ checking out ~s~n", [GitUrl]),
	case do_cmd("git clone " ++ GitUrl ++ " " ++ LocalProjectDir) of
		{0, "Initialized empty Git repository" ++ _ = Result} ->
			io:format("~s~n", [Result]),
			LocalProjectDir;
		{_, Other} ->
			io:format("~s~n", [Other]),
			exit(lists:flatten(io_lib:format("failed to checkout ~s", [GitUrl])))
	end.

search_sources_for_project(["git://github.com" ++ _ | _Tail], none, ProjectName) ->	
	Repos = 
		case githubby:repos_search({undefined, undefined}, ProjectName) of
			{struct,[{<<"repositories">>, Repos0}]} -> Repos0;
			_ -> []
		end,
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
			{Username ++ "-" ++ RepoName, "git://github.com/" ++ Username ++ "/" ++ RepoName ++ ".git"};
		[] -> 
			exit(lists:flatten(io_lib:format("failed to locate remote repo for ~s", [ProjectName])))
	end;
	
search_sources_for_project(["git://github.com" ++ _ | _Tail], User, ProjectName) ->
	case (catch githubby:repos_info({undefined, undefined}, User, ProjectName)) of
		{struct,[{<<"repository">>, {struct, Repo}}]} ->
			Username = binary_to_list(proplists:get_value(<<"owner">>, Repo)),
			RepoName = binary_to_list(proplists:get_value(<<"name">>, Repo)),
			{Username ++ "-" ++ RepoName, "git://github.com/" ++ Username ++ "/" ++ RepoName ++ ".git"};
		{'EXIT', Error} ->
			io:format("erroring in githubby~n"),
			exit({'EXIT', Error});
		_ -> 
			exit(lists:flatten(io_lib:format("failed to locate remote repo for ~s", [ProjectName])))
	end;

search_sources_for_project(_, _, _) ->
	exit(lists:flatten(io_lib:format("currently github is the only supported remote repository"))).
	
set_cwd_build_home(GlobalConfig) ->	
	set_cwd(proplists:get_value(build_path, GlobalConfig, ".")).
	
set_cwd(Dir) ->
	case file:set_cwd(Dir) of
		ok -> 
			ok;
		{error, _} ->
			exit(lists:flatten(io_lib:format("failed to change working directory: ~s", [Dir])))
	end.

checkout_correct_version([{tag, Tag}|_]) ->
	case do_cmd("git checkout -b \"" ++ Tag ++ "\" \"" ++ Tag ++ "\"") of
		{0, "Switched to a new branch" ++ _} -> 
			ok;
		{_, Other} ->
			exit(lists:flatten(io_lib:format("failed to switch to tag ~s: ~p", [Tag, Other])))
	end;
	
checkout_correct_version([{branch, Branch}|_]) ->
	case do_cmd("git checkout -b \"" ++ Branch ++ "\"") of
		{0, "Switched to a new branch" ++ _} -> 
			ok;
		{_, Other} ->
			exit(lists:flatten(io_lib:format("failed to switch to branch ~s: ~p", [Branch, Other])))
	end;
	
checkout_correct_version([{sha, Sha}|_]) ->
	case do_cmd("git checkout -b \"" ++ Sha ++ "\"") of
		{0, "Switched to a new branch" ++ _} -> 
			ok;
		{_, Other} ->
			exit(lists:flatten(io_lib:format("failed to switch to sha ~s: ~p", [Sha, Other])))
	end;
		
checkout_correct_version([_|Tail]) ->	
	checkout_correct_version(Tail);
	
checkout_correct_version(_) -> ok.
	
install_dependencies(GlobalConfig, ProjectName) ->
	Config = read_project_epm_config(ProjectName),
	[begin
		{ProjectName1, User} = split_package(Project),
		install_package(GlobalConfig, User, ProjectName1, CommandLineTags)
	 end || {Project, CommandLineTags} <- proplists:get_value(deps, Config, [])].
	
read_project_epm_config(ProjectName) ->
	case file:consult(ProjectName ++ ".epm") of
		{ok, [Config]} ->
			Config;
		{error, Reason} ->
			exit(lists:flatten(io_lib:format("failed to read ~s.epm config: ~p", [ProjectName, Reason])))
	end.
	
build_project(GlobalConfig, ProjectName, _CommandLineTags) ->
	Config = 
	    case file:consult(ProjectName ++ ".epm") of
    		{ok, [Config0]} -> Config0;
    		_ -> []
    	end,
	prebuild(Config),
	build(Config),
	%test(Config), %% TODO: add back in test step
	install(Config, proplists:get_value(install_path, GlobalConfig)).

prebuild(Config) ->
    case proplists:get_value(prebuild_command, Config) of
		undefined -> ok;
		PrebuildCmd -> 
			io:format("+ prebuild_command: ~s~n", [PrebuildCmd]),
			do_cmd(PrebuildCmd, fail)
	end.
	
build(Config) ->
    BuildCmd = proplists:get_value(build_command, Config, "make"),
	io:format("+ build_command: ~s~n", [BuildCmd]),
	do_cmd(BuildCmd, fail).
    
% test(Config) ->
%     TestCmd = proplists:get_value(test_command, Config, "make test"),
% 	io:format("+ test_command: ~s~n", [TestCmd]),
% 	{TestExitCode, TestOutput} = do_cmd(TestCmd),
% 	io:format("~s~n", [TestOutput]),
% 	TestExitCode.
    
install(Config, LibDir) ->
    InstallCmd = 
		case LibDir of
			undefined -> "";
			_ -> "ERL_LIBS=" ++ LibDir ++ " "
		end ++ proplists:get_value(install_command, Config, "make install"),
	io:format("+ install_command: ~s~n", [InstallCmd]),
	do_cmd(InstallCmd, fail).
		
del_dir(Dir) ->
	case file:list_dir(Dir) of
		{ok, Files} ->
			[begin
				case file:delete(Dir ++ "/" ++ Filename) of
					ok -> ok;
					{error, eperm} ->
						case file:del_dir(Dir ++ "/" ++ Filename) of
							ok -> ok;
							{error, eexist} ->
								del_dir(Dir ++ "/" ++ Filename)
						end
				end
			end || Filename <- Files],
			file:del_dir(Dir);
		_ ->
			ok
	end.
	
do_cmd(Cmd, fail) ->
	case do_cmd(Cmd) of
		{0, Output} ->
			io:format("~s~n", [Output]);
		{_, Output} ->
			exit(Output)
	end.
	
do_cmd(Cmd) ->
    Results = string:tokens(os:cmd(Cmd ++ "; echo $?"), "\n"),
    [ExitCode|Other] = lists:reverse(Results),
    {list_to_integer(ExitCode), string:join(lists:reverse(Other), "\n")}.
    
% ensure_erlang_vsn() ->
% 	%% greater than erts-5.7.4
% 	[Erts|_] = lists:reverse(string:tokens(code:lib_dir(erts), "/")),
% 	[_, Vsn] = string:tokens(Erts, "-"),
% 	Valid = 
% 		case [list_to_integer(X) || X <- string:tokens(Vsn, ".")] of
% 			[A,_,_] when A > 5 -> true;
% 			[5,B,_] when B > 7 -> true;
% 			[5,7,C] when C >= 4 -> true;
% 			_ -> false
% 		end,
% 	case Valid of
% 		true -> ok;
% 		false -> exit("epm requires Erlang version R13B03 or greater")
% 	end.
    	
ensure_git() ->
	case os:find_executable("git") of
		false -> exit("failed to locate git executable");
		_ -> ok
	end.	
