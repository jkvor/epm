-module(epm_core).
-export([execute/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("epm.hrl").

execute(GlobalConfig, ["install" | Args]) ->
    {Packages, Flags} = collect_args(install, Args),
	put(verbose, lists:member(verbose, Flags)),
	Deps = package_dependencies(Packages),
	{Installed, NotInstalled} = filter_installed_deps(Apps),
	case NotInstalled of
	    [] ->
	        io:format("+ nothing to do: packages and dependencies already installed~n");
	    _ ->
	        case Installed of
	            [] -> ok;
	            _ ->
	                io:format("===============================~n"),
                    io:format("Packages already installed:~n"),
                    io:format("===============================~n"),
                    [io:format("    + ~s-~s-~s (~s)~n", [U,N,V,proplists:get_value(vsn, Props)]) || {{U,N,V},Props} <- Installed]
            end,
	        io:format("===============================~n"),
	        io:format("Install the following packages?~n"),
	        io:format("===============================~n"),
            [io:format("    + ~s-~s-~s~n", [U,N,V]) || {{U,N,V},_} <- NotInstalled],
            io:format("~n([y]/n) "),
            case io:get_chars("", 1) of
                C when C == "y"; C == "\n" -> 
                    io:format("~n"),
                    [install_package(GlobalConfig, U, N, V, Props) || {{U,N,V},Props} <- NotInstalled];
                _ -> ok
            end
	end;

execute(GlobalConfig, ["remove" | Args]) ->
	{Packages, Flags} = collect_args(remove, Args),
	put(verbose, lists:member(verbose, Flags)),
	Installed0 = dict:to_list(installed_packages(Packages)),
	DependantPackages = dependant_installed_packages(proplists:get_keys(Installed0)),
    Installed = lists:usort(lists:append(Installed0, DependantPackages)),
	case Installed of
	    [] ->
	        io:format("+ nothing to remove: no matching packages installed~n");
	    _ ->
	        io:format("===============================~n"),
	        io:format("Remove the following packages?~n"),
	        io:format("===============================~n"),
            [io:format("    + ~s-~s-~s~n", [U,N,V]) || {{U,N,V},_} <- Installed],
            io:format("~n([y]/n) "),
            case io:get_chars("", 1) of
                C when C == "y"; C == "\n" -> 
                    io:format("~n"),
                    [remove_package(GlobalConfig, U, N, V, InstallDir) || {{U,N,V},{InstallDir,_Deps}} <- Installed];
                _ -> ok
            end
    end;
    
execute(GlobalConfig, ["update" | Args]) ->
    {Packages, Flags} = collect_args(update, Args),
    put(verbose, lists:member(verbose, Flags)),
    Installed = dict:to_list(installed_packages(Packages)),
    case Installed of
		[] ->
		    io:format("- nothing to update~n");
		_ ->
			io:format("===============================~n"),
			io:format("Update the following packages?~n"),
			io:format("===============================~n"),
			[io:format("    + ~s-~s-~s~n", [U,N,V]) || {{U,N,V},_} <- Installed],
            io:format("~n([y]/n) "),
            case io:get_chars("", 1) of
                C when C == "y"; C == "\n" -> 
                    io:format("~n"),
                    [update_package(GlobalConfig, U, N, V, InstallDir, Deps) || {{U,N,V},{InstallDir,Deps}} <- Installed];
                _ -> ok
            end
	end;
	
execute(_GlobalConfig, ["search" | Args]) ->
    {Packages, _Flags} = collect_args(search, Args),
    write_not_installed_package_info([{U,N,read_vsn_from_args(CommandLineArgs)} || {{U,N},CommandLineArgs} <- lists:reverse(Packages)]);
    
execute(_GlobalConfig, ["info" | Args]) ->
	{Packages, _Flags} = collect_args(info, Args),
    {Installed, NotInstalled} = lists:foldl(
		fun({{User, ProjectName}, CommandLineTags}, {TempInstalled, TempNotInstalled}) ->
			Vsn = read_vsn_from_args(CommandLineTags, undefined),
	        case local_package_info(User, ProjectName, Vsn) of
				[] ->
					{TempInstalled, [{User, ProjectName, Vsn}|TempNotInstalled]};
				List ->
					{lists:append(List, TempInstalled), TempNotInstalled}
			end	
		end, {[], []}, Packages),
	case Installed of
		[] -> ok;
		_ ->
			io:format("===============================~n"),
			io:format("INSTALLED~n"),
			io:format("===============================~n"),
			
			lists:foldl(
				fun({User, ProjectName, Vsn, InstallDir, Deps}, Count) ->
					case Count of
						0 -> ok;
						_ -> io:format("~n")
					end,
					write_installed_package_info(User, ProjectName, Vsn, InstallDir, Deps),
					Count+1
				end, 0, lists:reverse(Installed))
	end,
	
	case NotInstalled of
		[] -> ok;
		_ ->
			case Installed of
				[] -> ok;
				_ -> io:format("~n")
			end,
			write_not_installed_package_info(NotInstalled, true)
	end;
        
	
execute(_GlobalConfig, ["list" | _Args]) ->
    Installed = installed_packages(),
    case Installed of
		[] -> 
		    io:format("- no packages installed~n");
		_ ->
			io:format("===============================~n"),
			io:format("INSTALLED~n"),
			io:format("===============================~n"),

			lists:foldl(
				fun({User, ProjectName, Vsn, InstallDir, Deps}, Count) ->
					case Count of
						0 -> ok;
						_ -> io:format("~n")
					end,
					write_installed_package_info(User, ProjectName, Vsn, InstallDir, Deps),
					Count+1
				end, 0, lists:reverse(Installed))
	end;

execute(_GlobalConfig, ["latest" | _Args]) ->
	update_epm();

execute(_, _) ->
    io:format("Usage: epm commands~n~n"),
    io:format("    install [<user>/]<project> {project options}, ... {global options}~n"),
	io:format("        project options:~n"),
	io:format("             --tag <tag>~n"),
	io:format("             --branch <branch>~n"),
	io:format("             --sha <sha>~n"),
	io:format("             --with-deps (default)~n"),
	io:format("             --without-deps~n"),
	io:format("             --prebuild-command <cmd>~n"),
	io:format("             --build-command <cmd>~n"),
	io:format("             --test-command <cmd>~n"),
	io:format("        global options:~n"),
	io:format("             --verbose~n~n"),
    io:format("    remove [<user>/]<project> {project options}, ... {global options}~n"),
	io:format("        project options:~n"),
	io:format("             --tag <tag>~n"),
	io:format("             --branch <branch>~n"),
	io:format("             --sha <sha>~n"),
	io:format("             --with-deps~n"),
	io:format("             --without-deps (default)~n"),
	io:format("        global options:~n"),
	io:format("             --verbose~n~n"),
    io:format("    update [<user>/]<project> {project options}, ... {global options}~n"),
	io:format("        project options:~n"),
	io:format("             --tag <tag>~n"),
	io:format("             --branch <branch>~n"),
	io:format("             --sha <sha>~n"),
	io:format("             --with-deps~n"),
	io:format("             --without-deps (default)~n"),
	io:format("        global options:~n"),
	io:format("             --verbose~n~n"),
    io:format("    info [<user>/]<project>, ...~n~n"),
    io:format("    search <project>, ...~n~n"),
    io:format("    list~n~n"),
   	io:format("    latest~n"),
    ok.

%% -----------------------------------------------------------------------------
%% parse input args
%% -----------------------------------------------------------------------------

%% collect_args(Target, Args) -> Results
%%   Target = atom()
%%	 Args = [string()]
%%   Results = {[package(), Flags]}
%%	 Flags = [atom()]
collect_args(Target, Args) -> 
    collect_args(Target, Args, [], []).
collect_args(_, [], Packages, Flags) -> 
    {lists:reverse(Packages), lists:reverse(Flags)};
collect_args(Target, [Arg | Rest], Packages, Flags) ->
	case parse_tag(Target, Arg) of
		undefined -> %% if not a tag then must be a project name
			{ProjectName, User} = split_package(Arg), %% split into user and project
			collect_args(Target, Rest, [#package{user=User, name=ProjectName}|Packages], Flags);
		{Tag, true} -> %% tag with trailing value
			[Value | Rest1] = Rest, %% pop trailing value from front of remaining args
			[#package{args=Args}=Package|OtherPackages] = Packages, %% this tag applies to the last project on the stack
			Vsn = if
				Tag==tag; Tag==branch; Tag==sha -> Value;
				true -> Package#package.vsn
			end,
			collect_args(Target, Rest1, [Package#package{vsn=Vsn, args=Args ++ [{Tag, Value}]}|OtherPackages], Flags);
		{Tag, false} ->	 %% tag with no trailing value
			[#package{args=Args}=Package|OtherPackages] = Packages,
			collect_args(Target, Rest, [Package#package{args=Args ++ [Tag]}|OtherPackages], Flags);
		Flag ->
			collect_args(Target, Rest, Packages, [Flag|Flags])
	end.

%% @spec parse_tag(Target, Arg) -> {Tag, HasValue} | undefined
%%		 Target = atom()
%%		 Arg = string()
%%		 Tag = atom()
%%		 HasValue = bool()
parse_tag(install, "--tag") -> {tag, true};
parse_tag(install, "--branch") -> {branch, true};
parse_tag(install, "--sha") -> {sha, true};
parse_tag(install, "--prebuild-command") -> {prebuild_command, true};
parse_tag(install, "--build-command") -> {build_command, true};
parse_tag(install, "--test-command") -> {test_command, true};

parse_tag(info, "--tag") -> {tag, true};
parse_tag(info, "--branch") -> {branch, true};
parse_tag(info, "--sha") -> {sha, true};

parse_tag(_, "--with-deps") -> {with_deps, false};
parse_tag(_, "--without-deps") -> {without_deps, false};

parse_tag(_, "--verbose") -> verbose;

parse_tag(_, _) -> undefined.

split_package(Raw) -> split_package(Raw, []).
split_package([], Package) -> {Package, none};
split_package([47 | Package], User) -> {Package, User};
split_package([A | Tail], User) -> split_package(Tail, User ++ [A]).

%% -----------------------------------------------------------------------------
%% package info
%% -----------------------------------------------------------------------------	
installed_packages() ->
    [begin
        {User, ProjectName, Vsn, InstallDir, Deps}
     end || [{{User,ProjectName,Vsn},InstallDir,Deps}] <- dets:match(epm_index, '$1')].
        
installed_packages(Packages) ->
    installed_packages(Packages, dict:new()).
    
installed_packages([], Dict) ->
    Dict;
    
installed_packages([{{User, ProjectName}, CommandLineTags}|Tail], Dict) ->
    Vsn = read_vsn_from_args(CommandLineTags),
    FollowDeps = lists:member(with_deps, CommandLineTags),
    installed_packages([{User, ProjectName, Vsn, FollowDeps}|Tail], Dict);
    
installed_packages([{User, ProjectName, Vsn, FollowDeps}|Tail], Dict) ->
    Dict1 = 
        case local_package_info(User, ProjectName, Vsn) of
            [] ->
                Dict;
            List ->
                lists:foldl(
                    fun({User1, _, Vsn1, InstallDir1, Deps1}, TempDict) ->
                        TempDict1 =
                            case FollowDeps of
                                true ->
                                    lists:foldl(
                                        fun({DepUser, DepName, DepVsn}, TempDict2) ->
                                            installed_packages([{DepUser, DepName, DepVsn, FollowDeps}], TempDict2)
                                        end, TempDict, Deps1);
                                false ->
                                    TempDict
                            end,
                        dict:store({User1, ProjectName, Vsn1}, {InstallDir1, Deps1}, TempDict1)
                    end, Dict, List)
        end,
    installed_packages(Tail, Dict1).
    
dependant_installed_packages(Packages) ->
    dependant_installed_packages(Packages, []).
    
dependant_installed_packages([], Acc) -> Acc;
dependant_installed_packages([Package|Tail], Acc) ->
    Acc1 = dependant_installed_packages(Package, Acc, dets:match(epm_index, '$1')),
    dependant_installed_packages(Tail, Acc1).

dependant_installed_packages(_Package, Acc, []) -> Acc;
dependant_installed_packages({User, Name, Vsn}, Acc, [[{Installed,InstallDir,Deps}]|Tail]) ->
    Acc1 = case lists:filter(
            fun({U,N,V}) ->
                (U==User orelse U==none) andalso
                (N==Name) andalso
                (V==Vsn)
            end, Deps) of
        [] -> Acc;
        [_] -> [{Installed,{InstallDir,Deps}}|Acc]
    end,
    dependant_installed_packages({User, Name, Vsn}, Acc1, Tail).
        
installed_app_vsn(App) ->
    case dets:lookup(epm_index, App) of
        [{{_,Name,_}, InstallDir, _Deps}] -> 
            case file:consult(InstallDir ++ "/ebin/" ++ Name ++ ".app") of
                {ok, [{application,_,Props}]} ->
                    proplists:get_value(vsn, Props);
                {error, _} ->
                    undefined
            end;
        _ -> undefined
    end.
    
local_package_info(none, ProjectName, undefined) ->
    case dets:match(epm_index, {{'$1',ProjectName,'$2'},'$3','$4'}) of
        [] -> [];
        List ->
            [{User,ProjectName,Vsn,InstallDir,Deps} || [User,Vsn,InstallDir,Deps] <- List]
    end;    
local_package_info(none, ProjectName, Vsn) ->
    case dets:match(epm_index, {{'$1',ProjectName,Vsn},'$2','$3'}) of
        [] -> [];
        List ->
            [{User,ProjectName,Vsn,InstallDir,Deps} || [User,InstallDir,Deps] <- List]            
    end;
local_package_info(User, ProjectName, undefined) ->
    case dets:match(epm_index, {{User,ProjectName,'$1'},'$2','$3'}) of
        [] -> [];
        List ->
            [{User,ProjectName,Vsn,InstallDir,Deps} || [Vsn,InstallDir,Deps] <- List]
    end;
local_package_info(User, ProjectName, Vsn) ->
    case dets:match(epm_index, {{User,ProjectName,Vsn},'$1','$2'}) of
        [] -> [];
        List ->
            [{User,ProjectName,Vsn,InstallDir,Deps} || [InstallDir,Deps] <- List]
    end.

%% -----------------------------------------------------------------------------
%% Print package info
%% -----------------------------------------------------------------------------	
write_installed_package_info(User, ProjectName, Vsn, InstallDir, Deps) ->
	Repo = 
	    case repos_info(User, ProjectName) of
	        #repository{}=R0 -> R0;
	        Err -> exit(lists:flatten(io_lib:format("failed to fetch remote repo info for ~s: ~p", [ProjectName, Err])))
	    end,
    [io:format("  ~s: ~s~n", [Field, if Value==undefined -> ""; true -> Value end]) || {Field, Value} <- [
		{"name", ProjectName},
		{"owner", User},
		{"vsn", Vsn},
		{"pushed", Repo#repository.pushed},
		{"install dir", InstallDir},
		{"homepage", Repo#repository.homepage},
		{"description", Repo#repository.description}
	]],
    case Deps of
        [] -> ok;
        _ ->
            io:format("  dependencies: ~n    ~s~n", [string:join([
             case U of
				none -> lists:flatten(io_lib:format("~s/~s", [N,V]));
				_ -> lists:flatten(io_lib:format("~s/~s/~s", [U,N,V]))
             end || {U,N,V} <- Deps], "\n    ")])
    end.
    
write_not_installed_package_info(Packages) ->
	write_not_installed_package_info(Packages, false).
	
write_not_installed_package_info(Packages, IsExact) ->
    case fetch_not_installed_package_info(Packages, [], IsExact) of
        [] -> 
            io:format("- not found~n");
        Results ->
            io:format("===============================~n"),
        	io:format("AVAILABLE~n"),
        	io:format("===============================~n"),
        	[write_not_installed_package_info(User, ProjectName, Vsn, Repos) || {{User, ProjectName, Vsn}, Repos} <- Results]
    end.
    
write_not_installed_package_info(_User, _ProjectName, _Vsn, Repos) ->
	lists:foldl(
		fun(Repo, Count) ->
			Tags = repos_tags(Repo#repository.owner, Repo#repository.name),
			Branches = repos_branches(Repo#repository.owner, Repo#repository.name),
			case Count of
				0 -> ok;
				_ -> io:format("~n")
			end,
			[io:format("  ~s: ~s~n", [Field, if Value==undefined -> ""; true -> Value end]) || {Field, Value} <- [
				{"name", Repo#repository.name},
				{"owner", Repo#repository.owner},
				{"followers", Repo#repository.followers},
				{"pushed", Repo#repository.pushed},
				{"homepage", Repo#repository.homepage},
				{"description", Repo#repository.description}
			]],
			if 
			    Tags =/= [] -> 
			        io:format("  tags:~n"),
			        [io:format("    ~s~n", [K]) || {K,_V} <- Tags];
			    true -> ok
			end,
			if 
			    Branches =/= [] -> 
		            io:format("  branches:~n"),
		            [io:format("    ~s~n", [K]) || {K,_V} <- Branches];
			    true -> ok 
			end,
			Count+1
		end, 0, Repos).

fetch_not_installed_package_info([], Acc, _) -> Acc;
fetch_not_installed_package_info([{User, ProjectName, Vsn}|Tail], Acc, IsExact) ->
    Acc1 = 
        case User of
    		none -> 
    		    case repos_search(ProjectName) of
    		        [] -> 
						Acc;
    		        Repos when is_list(Repos), IsExact == true -> 
						Repos1 = [R || R <- Repos, R#repository.name==ProjectName],
						case Repos1 of
							[] -> Acc;
							_ -> [{{User, ProjectName, Vsn}, Repos1}|Acc]
						end;
					Repos when is_list(Repos) ->
						[{{User, ProjectName, Vsn}, Repos}|Acc];
    		        _ -> 
						Acc
    		    end;
    		_ -> 
    			case repos_info(User, ProjectName) of
    				#repository{}=R -> [{{User, ProjectName, Vsn}, [R]}|Acc];
    				_ -> Acc
    			end
    	end,
    fetch_not_installed_package_info(Tail, Acc1, IsExact).
	
%% -----------------------------------------------------------------------------
%% INSTALL
%% -----------------------------------------------------------------------------
install_package(GlobalConfig, User, ProjectName, Vsn, Props) ->
	%% switch to build home dir
	epm_util:set_cwd_build_home(GlobalConfig),
	
	%% download correct version of package
	LocalProjectDir = download_package(User, ProjectName, Vsn),
	
	%% switch to project dir
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:set_cwd(LocalProjectDir),
	
	%% build/install project
	InstallDir = build_project(GlobalConfig, ProjectName, Props),
	
	%% switch to build home dir and delete cloned project
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:del_dir(LocalProjectDir),
	
	Deps = proplists:get_value(deps, Props),
	dets:insert(epm_index, {{User, ProjectName, Vsn}, InstallDir, Deps}),
	
	ok.

%% -----------------------------------------------------------------------------
%% REMOVE
%% -----------------------------------------------------------------------------
remove_package(_GlobalConfig, User, ProjectName, Vsn, InstallDir) ->
    io:format("+ removing package ~s-~s-~s from ~s~n", [User, ProjectName, Vsn, InstallDir]),
    RemoveCmd = "rm -rf " ++ InstallDir,
    epm_util:print_cmd_output("~s~n", [RemoveCmd]),
    epm_util:do_cmd(RemoveCmd, fail),
    dets:delete(epm_index, {User,ProjectName,Vsn}).

%% -----------------------------------------------------------------------------
%% UPDATE
%% ----------------------------------------------------------------------------- 
update_package(GlobalConfig, User, ProjectName, Vsn, _InstallDir, _Deps) ->
    %% switch to build home dir
	epm_util:set_cwd_build_home(GlobalConfig),
	
	%% download correct version of package
	LocalProjectDir = download_package(User, ProjectName, Vsn),
	
	%% switch to project dir
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:set_cwd(LocalProjectDir),
	
	%% build/install project
	_InstallDir = build_project(GlobalConfig, ProjectName, []),
	
	%% switch to build home dir and delete cloned project
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:del_dir(LocalProjectDir).
       
read_vsn_from_args(Args) ->
    read_vsn_from_args(Args, "master").
    
read_vsn_from_args([{tag, Tag}|_], _) -> Tag;
read_vsn_from_args([{branch, Branch}|_], _) -> Branch;
read_vsn_from_args([{sha, Sha}|_], _) -> Sha;
read_vsn_from_args([_|Tail], Default) -> read_vsn_from_args(Tail, Default);
read_vsn_from_args([], Default) -> Default.
		
build_project(GlobalConfig, ProjectName, Props) ->
	Config = 
	    case file:consult(ProjectName ++ ".epm") of
    		{ok, [Config0]} -> Config0;
    		_ -> []
    	end,
	UserSuppliedPrebuildCommand = proplists:get_value(prebuild_command, Props),
	UserSuppliedBuildCommand = proplists:get_value(build_command, Props),
	UserSuppliedTestCommand = proplists:get_value(test_command, Props),
	prebuild(ProjectName, Config, UserSuppliedPrebuildCommand),
	build(ProjectName, Config, UserSuppliedBuildCommand),
	test(ProjectName, Config, UserSuppliedTestCommand),
	install(ProjectName, Config, proplists:get_value(install_dir, GlobalConfig)).

prebuild(ProjectName, Config, undefined) ->
    case proplists:get_value(prebuild_command, Config) of
		undefined -> ok;
		PrebuildCmd -> prebuild1(ProjectName, PrebuildCmd)
	end;
prebuild(ProjectName, _Config, PrebuildCmd) ->
	prebuild1(ProjectName, PrebuildCmd).
	
prebuild1(ProjectName, PrebuildCmd) ->
	io:format("+ running ~s prebuild command~n", [ProjectName]),
	epm_util:print_cmd_output("~s~n", [PrebuildCmd]),
	epm_util:do_cmd(PrebuildCmd, fail).
	
build(ProjectName, Config, undefined) ->
    BuildCmd = proplists:get_value(build_command, Config, "make"),
	build1(ProjectName, BuildCmd);
build(ProjectName, _Config, BuildCmd) ->
	build1(ProjectName, BuildCmd).	

build1(ProjectName, BuildCmd) ->
	io:format("+ running ~s build command~n", [ProjectName]),
	epm_util:print_cmd_output("~s~n", [BuildCmd]),
	epm_util:do_cmd(BuildCmd, fail).

test(ProjectName, Config, undefined) ->
    case proplists:get_value(test_command, Config) of
		undefined -> ok;
		TestCmd -> test1(ProjectName, TestCmd)
	end;
test(ProjectName, _Config, TestCmd) ->
	test1(ProjectName, TestCmd).
	
test1(ProjectName, TestCmd) ->
	io:format("+ running ~s test command~n", [ProjectName]),
	epm_util:print_cmd_output("~s~n", [TestCmd]),
	epm_util:do_cmd(TestCmd, fail).
	
install(ProjectName, Config, undefined) ->
	install(ProjectName, Config, code:lib_dir());

install(ProjectName, _Config, LibDir) ->
	Vsn = 
	    case file:consult("ebin/" ++ ProjectName ++ ".app") of
    		{ok,[{application,_,Props}]} ->
    			proplists:get_value(vsn, Props);
    		_ ->
    			undefined
    	end,
    Dir = 
        case Vsn of
            undefined -> LibDir ++ "/" ++ ProjectName;
            _ -> LibDir ++ "/" ++ ProjectName ++ "-" ++ Vsn
        end,
	InstallCmd = "mkdir -p " ++ Dir ++ "; cp -R ./* " ++ Dir,
	io:format("+ running ~s install command~n", [ProjectName]),
	epm_util:print_cmd_output("~s~n", [InstallCmd]),
	epm_util:do_cmd(InstallCmd, fail),
	Ebin = Dir ++ "/ebin",
	case code:add_pathz(Ebin) of
	    true ->
	        ok;
	    Err ->
	        exit(lists:flatten(io_lib:format("failed to add path for ~s (~s): ~p", [ProjectName, Ebin, Err])))
	end,
	Dir.

%% -----------------------------------------------------------------------------
%% Replace epm script with most recent
%% -----------------------------------------------------------------------------
update_epm() ->
	File = 
		case os:find_executable("epm") of
			false ->
				case filelib:is_regular("epm") of
					true -> "./epm";
					fasle -> exit("failed to find epm executable to replace")
				end;
			F -> F
		end,
	Url = "http://github.com/JacobVorreuter/epm/raw/master/epm",
	case http:request(get, {Url, [{"User-Agent", "EPM"}, {"Host", "github.com"}]}, [{timeout, 6000}], [{body_format, binary}]) of
		{ok, {{_, 200, _}, _, Body}} ->
			case file:write_file(File, Body) of
				ok ->
					io:format("+ updated epm (~s) to latest version~n", [File]);
				{error, Reason} ->
					exit(lists:flatten(io_lib:format("failed to overwrite epm executable ~s: ~p~n", [File, Reason])))
			end;
		_ ->
			exit("failed to download latest version of epm")
	end.
	
%% -----------------------------------------------------------------------------
%% Compile list of dependencies
%% -----------------------------------------------------------------------------
package_dependencies(Packages) ->
	G = digraph:new(),
    AppInfo = package_dependencies1(Packages, G, undefined, dict:new()),
    Deps = digraph_utils:topsort(G),
    digraph:delete(G),
    [{Dep, dict:fetch(Dep, AppInfo)} || Dep <- Deps].


%% returns [{{User,Name,Vsn},[{deps, Deps}]}]
gather_remote_deps(Packages) ->
    G = digraph:new(),
    AppInfo = gather_remote_deps(Packages, G, undefined, dict:new()),
    Deps = digraph_utils:topsort(G),
    digraph:delete(G),
    [{Dep, dict:fetch(Dep, AppInfo)} || Dep <- Deps].
   
gather_remote_deps([], _, _, Dict) -> Dict;
gather_remote_deps([Package|Tail], G, Parent, Dict) ->
	RepoPlugins = proplists:get_value(repo_plugins, GlobalConfig, [github_api]),
    Repo = retrieve_remote_repo(RepoPlugins, Package#package.user, Package#package.name),
    WithoutDeps = lists:member(without_deps, Package#package.args),
	Key = {Repo#repository.owner, Repo#repository.name, Package#package.vsn},
    
    digraph:add_vertex(G, Key),
    
    case Parent of
        undefined -> ok;
        {_, ParentProjectName, _} ->
            digraph:add_edge(G, Parent, Key),
            case digraph_utils:is_acyclic(G) of
                true ->
                    ok;
                false ->
                    ?EXIT("circular dependency detected: ~s <--> ~s", [ParentProjectName, Repo#repository.name])
            end
    end,
    
    {Deps, Dict1} = 
        case WithoutDeps of
            true ->
                {[], Dict};
            false ->
                lists:mapfoldl(
                    fun({Dep, Args}, TempDict) -> 
                        {DepName, DepUser} = split_package(Dep),
                        DepVsn = read_vsn_from_args(Args),
                        TempDict1 = gather_remote_deps([{{DepUser, DepName}, Args}], G, Key, TempDict),
                        {{DepUser, DepName, DepVsn}, TempDict1} 
                    end, Dict, package_deps(Repo#repository.owner, Repo#repository.name, Vsn))
        end,
    
    Dict2 = dict:store(Key, [{deps, Deps}|CommandLineTags], Dict1),
    gather_remote_deps(Tail, G, Parent, Dict2).

filter_installed_deps(Apps) ->
    filter_installed_deps(Apps, [], []).
    
filter_installed_deps([], Installed, NotInstalled) ->
    {lists:reverse(Installed), NotInstalled};
    
filter_installed_deps([{App,Info}|Tail], Installed, NotInstalled) ->
    case installed_app_vsn(App) of
        undefined -> filter_installed_deps(Tail, Installed, [{App,Info}|NotInstalled]);
        AppVsn -> filter_installed_deps(Tail, [{App,[{vsn, AppVsn}|Info]}|Installed], NotInstalled)
    end.

retrieve_remote_repo(RepoPlugins, none, ProjectName) ->	
    case search(ProjectName) of
        [] ->
            ?EXIT("failed to locate remote repo for ~s", [ProjectName]);
        Repos when is_list(Repos) ->
            case lists:filter(fun(R1) -> R1#repository.name==ProjectName end, Repos) of
				[R0|_] -> R0;
				[] ->
					?EXIT("failed to locate remote repo for ~s", [ProjectName])
			end;
        Err ->
            ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
    end;

retrieve_remote_repo(User, ProjectName) ->
	case info(User, ProjectName) of
		#repository{}=Repo ->
			Repo;
		Err -> 
			?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
	end.