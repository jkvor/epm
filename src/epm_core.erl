-module(epm_core).
-export([execute/2]).

-include("epm.hrl").

-define(DEFAULT_API_MODULES, [github_api]).

execute(GlobalConfig0, ["install" | Args]) ->
    {GlobalConfig, Packages, Flags} = collect_args(install, Args, GlobalConfig0),
	put(verbose, lists:member(verbose, Flags)),
	Deps = package_dependencies(GlobalConfig, Packages),
	{Installed, NotInstalled} = filter_installed_packages(Deps),
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
                    [begin
                        io:format("    + ~s-~s-~s (~s)~n", [U,N,V,AppVsn])
                     end || #package{user=U, name=N, vsn=V, app_vsn=AppVsn} <- Installed]
            end,
	        io:format("===============================~n"),
	        io:format("Install the following packages?~n"),
	        io:format("===============================~n"),
            [io:format("    + ~s-~s-~s~n", [U,N,V]) || #package{user=U, name=N, vsn=V} <- NotInstalled],
            io:format("~n([y]/n) "),
            case io:get_chars("", 1) of
                C when C == "y"; C == "\n" -> 
                    io:format("~n"),
                    [install_package(GlobalConfig, Package) || Package <- NotInstalled];
                _ -> ok
            end
	end;

execute(GlobalConfig0, ["remove" | Args]) ->
	{GlobalConfig, Packages, Flags} = collect_args(remove, Args, GlobalConfig0),
	put(verbose, lists:member(verbose, Flags)),
	Installed = installed_packages(Packages),
    case Installed of
        [] ->
            io:format("+ nothing to remove: no matching packages installed~n");
        _ ->
            io:format("===============================~n"),
            io:format("Remove the following packages?~n"),
            io:format("===============================~n"),
            [io:format("    + ~s-~s-~s~n", [U,N,V]) || #package{user=U, name=N, vsn=V} <- Installed],
            io:format("~n([y]/n) "),
            case io:get_chars("", 1) of
                C when C == "y"; C == "\n" -> 
                    io:format("~n"),
                    [remove_package(GlobalConfig, Package) || Package <- Installed];
                _ -> ok
            end
        end;

execute(GlobalConfig0, ["update" | Args]) ->
    {GlobalConfig, Packages, Flags} = collect_args(update, Args, GlobalConfig0),
    put(verbose, lists:member(verbose, Flags)),
    Installed = installed_packages(Packages),
    case Installed of
		[] ->
		    io:format("- nothing to update~n");
		_ ->
			io:format("===============================~n"),
			io:format("Update the following packages?~n"),
			io:format("===============================~n"),
			[io:format("    + ~s-~s-~s~n", [U,N,V]) || #package{user=U, name=N, vsn=V} <- Installed],
            io:format("~n([y]/n) "),
            case io:get_chars("", 1) of
                C when C == "y"; C == "\n" -> 
                    io:format("~n"),
                    [update_package(GlobalConfig, Package) || Package <- Installed];
                _ -> ok
            end
	end;
	
execute(GlobalConfig0, ["info" | Args]) ->
	{GlobalConfig, Packages, _Flags} = collect_args(info, Args, GlobalConfig0),
	{Installed, NotInstalled} = filter_installed_packages(Packages),
    case Installed of
		[] -> ok;
		_ ->
			io:format("===============================~n"),
			io:format("INSTALLED~n"),
			io:format("===============================~n"),
	
			lists:foldl(
				fun(Package, Count) ->
					case Count of
						0 -> ok;
						_ -> io:format("~n")
					end,
					write_installed_package_info(Package),
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
       			write_not_installed_package_info(GlobalConfig, NotInstalled, true)
       	end;
	
execute(GlobalConfig0, ["search" | Args]) ->
    {GlobalConfig, Packages, _Flags} = collect_args(search, Args, GlobalConfig0),
    write_not_installed_package_info(GlobalConfig, lists:reverse(Packages));
        
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
				fun(Package, Count) ->
					case Count of
						0 -> ok;
						_ -> io:format("~n")
					end,
					write_installed_package_info(Package),
					Count+1
				end, 0, lists:reverse(Installed))
	end;

execute(_GlobalConfig, ["latest" | _Args]) ->
	update_epm();
	
execute(GlobalConfig0, ["config" | Args]) ->
	{GlobalConfig, _Packages, Flags} = collect_args(config, Args, GlobalConfig0),
	case Flags of
		[] ->
			print_config_values(GlobalConfig);
		[get] ->
			print_config_values(GlobalConfig);
		_ ->
			Config2 = lists:foldl(
				fun(Flag, Config) ->
					case Flag of
						{set, [K,V]} ->
							K1 = list_to_atom(K), 
							[{K1, V}|proplists:delete(K1, Config)];
						{remove, K} ->
							K1 = list_to_atom(K),
							proplists:delete(K1, Config);
						_ ->
							Config
					end
				end, GlobalConfig, Flags),
			write_config_file(Config2)
	end;
		
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
	io:format("             --verbose~n"),
	io:format("             --config-set <key> <value>~n~n"),
    io:format("    remove [<user>/]<project> {project options}, ... {global options}~n"),
	io:format("        project options:~n"),
	io:format("             --tag <tag>~n"),
	io:format("             --branch <branch>~n"),
	io:format("             --sha <sha>~n"),
	io:format("        global options:~n"),
	io:format("             --verbose~n~n"),
	io:format("             --config-set <key> <value>~n~n"),	
    io:format("    update [<user>/]<project> {project options}, ... {global options}~n"),
	io:format("        project options:~n"),
	io:format("             --tag <tag>~n"),
	io:format("             --branch <branch>~n"),
	io:format("             --sha <sha>~n"),
	io:format("             --with-deps~n"),
	io:format("             --without-deps (default)~n"),
	io:format("        global options:~n"),
	io:format("             --verbose~n~n"),
	io:format("             --config-set <key> <value>~n~n"),
    io:format("    info [<user>/]<project>, ... {global options}~n"),
	io:format("        global options:~n"),
	io:format("             --config-set <key> <value>~n~n"),	
    io:format("    search <project>, ... {global options}~n"),
	io:format("        global options:~n"),
	io:format("             --config-set <key> <value>~n~n"),	
    io:format("    list~n~n"),
   	io:format("    latest~n~n"),
   	io:format("    config {options}~n"),
	io:format("        options:~n"),
	io:format("             --get (default)~n"),
	io:format("             --set <key> <value>~n"),
	io:format("             --remove <key>~n"),
    ok.

%% -----------------------------------------------------------------------------
%% parse input args
%% -----------------------------------------------------------------------------

%% collect_args(Target, Args, GlobalConfig) -> Results
%%   Target = atom()
%%	 Args = [string()]
%%`  GlobalConfig = list()
%%   Results = {[package(), Flags]}
%%	 Flags = [atom()]
collect_args(Target, Args, GlobalConfig) -> 
    collect_args(Target, Args, GlobalConfig, [], []).
collect_args(_, [], GlobalConfig, Packages, Flags) -> 
    {GlobalConfig, lists:reverse(Packages), lists:reverse(Flags)};
collect_args(Target, [Arg | Rest], GlobalConfig, Packages, Flags) ->
	case parse_tag(Target, Arg) of
		undefined -> %% if not a tag then must be a project name
			{ProjectName, User} = split_package(Arg), %% split into user and project
			collect_args(Target, Rest, GlobalConfig, [#package{user=User, name=ProjectName}|Packages], Flags);
		{Type, Tag, 0} ->	 %% tag with no trailing value
			case Type of
				project ->
					[#package{args=Args}=Package|OtherPackages] = Packages,
					collect_args(Target, Rest, GlobalConfig, [Package#package{args=Args ++ [Tag]}|OtherPackages], Flags);
				global ->
					collect_args(Target, Rest, GlobalConfig, Packages, [Tag|Flags])
			end;
		{Type, Tag, NumVals} when is_integer(NumVals) -> %% tag with trailing value(s)
			if
				length(Rest) < NumVals ->
					exit("poorly formatted command");
				true -> ok
			end,
			{Vals, Rest1} = lists:split(NumVals, Rest),
			Vals1 = 
				case Vals of
					[V] -> V;
					_ -> Vals
				end,
			case Type of
				project ->
					[#package{args=Args}=Package|OtherPackages] = Packages, %% this tag applies to the last project on the stack
					Vsn = if
						Tag==tag; Tag==branch; Tag==sha -> Vals1;
						true -> Package#package.vsn
					end,
					collect_args(Target, Rest1, GlobalConfig, [Package#package{vsn=Vsn, args=Args ++ [{Tag, Vals1}]}|OtherPackages], Flags);
				global ->
					GlobalConfig1 = 
						if
							Tag == config_set ->
								[K,V1] = Vals1,
								K1 = list_to_atom(K),
								[case K1 of
									repo_plugins -> {K1, epm_util:eval(V1++".")};
									_ -> {K1, V1}
								 end |proplists:delete(K1, GlobalConfig)];
							true -> 
								GlobalConfig
						end,
					collect_args(Target, Rest1, GlobalConfig1, Packages, [{Tag, Vals1}|Flags])
			end	
	end.

%% @spec parse_tag(Target, Arg) -> {Tag, HasValue} | undefined
%%		 Target = atom()
%%		 Arg = string()
%%		 Tag = atom()
%%		 HasValue = bool()
parse_tag(install, "--tag") -> {project, tag, 1};
parse_tag(install, "--branch") -> {project, branch, 1};
parse_tag(install, "--sha") -> {project, sha, 1};
parse_tag(install, "--prebuild-command") -> {project, prebuild_command, 1};
parse_tag(install, "--build-command") -> {project, build_command, 1};
parse_tag(install, "--test-command") -> {project, test_command, 1};

parse_tag(info, "--tag") -> {project, tag, 1};
parse_tag(info, "--branch") -> {project, branch, 1};
parse_tag(info, "--sha") -> {project, sha, 1};

parse_tag(_, "--with-deps") -> {project, with_deps, 0};
parse_tag(_, "--without-deps") -> {project, without_deps, 0};

parse_tag(config, "--get") -> {global, get, 0};
parse_tag(config, "--set") -> {global, set, 2};
parse_tag(config, "--remove") -> {global, remove, 1};
parse_tag(_, "--verbose") -> {global, verbose, 0};
parse_tag(_, "--config-set") -> {global, config_set, 2};

parse_tag(_, _) -> undefined.

split_package(Raw) -> split_package(Raw, []).
split_package([], Package) -> {Package, none};
split_package([47 | Package], User) -> {Package, User};
split_package([A | Tail], User) -> split_package(Tail, User ++ [A]).

%% -----------------------------------------------------------------------------
%% package info
%% -----------------------------------------------------------------------------	
local_package_info(#package{user=none, name=ProjectName, vsn=undefined}) ->
    case dets:match(epm_index, {{'_',ProjectName,'_'},'$1'}) of
        [] -> [];
        List -> [Package || [Package] <- List]
    end;    
local_package_info(#package{user=none, name=ProjectName, vsn=Vsn}) ->
    case dets:match(epm_index, {{'_',ProjectName,Vsn},'$1'}) of
        [] -> [];
        List -> [Package || [Package] <- List]
    end;
local_package_info(#package{user=User, name=ProjectName, vsn=undefined}) ->
    case dets:match(epm_index, {{User,ProjectName,'_'},'$1'}) of
        [] -> [];
        List -> [Package || [Package] <- List]
    end;
local_package_info(#package{user=User, name=ProjectName, vsn=Vsn}) ->
    case dets:match(epm_index, {{User,ProjectName,Vsn},'$1'}) of
        [] -> [];
        List -> [Package || [Package] <- List]
    end.
    
installed_packages() ->
    [Package || [{_,Package}] <- dets:match(epm_index, '$1')].

installed_packages(Packages) ->
    [V || {_K,V} <- dict:to_list(installed_packages1(Packages, dict:new()))].
    
installed_packages1([], Dict) ->
    Dict;
    
installed_packages1([Package|Tail], Dict) ->
    Dict1 = 
        case local_package_info(Package) of
            [] ->
                Dict;
            List ->
                lists:foldl(
                    fun(InstalledPackage, TempDict) ->
                        TempDict1 = dict:store({
                            InstalledPackage#package.user, 
                            InstalledPackage#package.name, 
                            InstalledPackage#package.vsn }, InstalledPackage, TempDict),
                        DependantPackages = dependant_installed_packages(InstalledPackage),    
                        installed_packages1(DependantPackages, TempDict1)
                    end, Dict, List)
        end,
   installed_packages1(Tail, Dict1). 
    
dependant_installed_packages(Package) ->
    dependant_installed_packages(Package, [], dets:match(epm_index, '$1')).

dependant_installed_packages(_Package, Acc, []) -> Acc;
dependant_installed_packages(#package{user=User,name=Name,vsn=Vsn}=Package, Acc, [[{_,#package{deps=Deps}=InstalledPackage}]|Tail]) ->
    Acc1 = case lists:filter(
            fun({U,N,V}) ->
                (U==User orelse U==none) andalso
                (N==Name) andalso
                (V==Vsn)
            end, Deps) of
        [] -> Acc;
        [_] -> [InstalledPackage|Acc]
    end,
    dependant_installed_packages(Package, Acc1, Tail).
     
%% -----------------------------------------------------------------------------
%% Print package info
%% -----------------------------------------------------------------------------	
write_installed_package_info(Package) ->
	Repo = Package#package.repo,
    [io:format("  ~s: ~s~n", [Field, if Value==undefined -> ""; true -> Value end]) || {Field, Value} <- [
		{"name", Repo#repository.name},
		{"owner", Repo#repository.owner},
		{"vsn", Package#package.vsn},
		{"pushed", Repo#repository.pushed},
		{"install dir", Package#package.install_dir},
		{"homepage", Repo#repository.homepage},
		{"description", Repo#repository.description}
	]],
    case Package#package.deps of
        [] -> ok;
        Deps ->
            io:format("  dependencies: ~n    ~s~n", [string:join([
             case U of
				none -> lists:flatten(io_lib:format("~s/~s", [N,V]));
				_ -> lists:flatten(io_lib:format("~s/~s/~s", [U,N,V]))
             end || {U,N,V} <- Deps], "\n    ")])
    end.

write_not_installed_package_info(GlobalConfig, Packages) ->
	write_not_installed_package_info(GlobalConfig, Packages, false).
	
write_not_installed_package_info(GlobalConfig, Packages, IsExact) ->
    RepoPlugins = proplists:get_value(repo_plugins, GlobalConfig, ?DEFAULT_API_MODULES),
	write_not_installed_package_info1(Packages, RepoPlugins, IsExact).
	
write_not_installed_package_info1(Packages, RepoPlugins, IsExact) ->
    case fetch_not_installed_package_info(Packages, RepoPlugins, [], IsExact) of
        [] -> 
            io:format("- not found~n");
        Repos ->
            io:format("===============================~n"),
        	io:format("AVAILABLE~n"),
        	io:format("===============================~n"),
        	lists:foldl(
				fun(Repo, Count) ->
					Tags = apply(Repo#repository.api_module, tags, [Repo#repository.owner, Repo#repository.name]),
					Branches = apply(Repo#repository.api_module, branches, [Repo#repository.owner, Repo#repository.name]),
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
						{"description", Repo#repository.description},
						{"repo plugin", atom_to_list(Repo#repository.api_module)}
					]],
					if 
					    Tags =/= [] -> 
					        io:format("  tags:~n"),
					        [io:format("    ~s~n", [Tag]) || Tag <- Tags];
					    true -> ok
					end,
					if 
					    Branches =/= [] -> 
				            io:format("  branches:~n"),
				            [io:format("    ~s~n", [Branch]) || Branch <- Branches];
					    true -> ok 
					end,
					Count+1
				end, 0, Repos)
    end.

fetch_not_installed_package_info([], _, Acc, _) -> Acc;
fetch_not_installed_package_info([#package{user=User,name=ProjectName}|Tail], RepoPlugins, Acc, IsExact) ->
    Repos = retrieve_remote_repos(RepoPlugins, User, ProjectName, IsExact),
    fetch_not_installed_package_info(Tail, RepoPlugins, lists:append(Acc, Repos), IsExact).
		           
%% -----------------------------------------------------------------------------
%% INSTALL
%% -----------------------------------------------------------------------------
install_package(GlobalConfig, Package) ->
    Repo = Package#package.repo,
    User = Repo#repository.owner, 
    Name = Repo#repository.name, 
    Vsn = Package#package.vsn,
	%% switch to build home dir
	epm_util:set_cwd_build_home(GlobalConfig),
	
	%% download correct version of package
	LocalProjectDir = apply(Repo#repository.api_module, download_package, [Repo, Vsn]),
	
	%% switch to project dir
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:set_cwd(LocalProjectDir),
	
	%% build/install project
	InstallDir = build_project(GlobalConfig, Package),
	
	%% switch to build home dir and delete cloned project
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:del_dir(LocalProjectDir),
	
	dets:insert(epm_index, {{User, Name, Vsn}, Package#package{install_dir=InstallDir}}),
	
	ok.

%% -----------------------------------------------------------------------------
%% REMOVE
%% -----------------------------------------------------------------------------
remove_package(_GlobalConfig, #package{user=User, name=Name, vsn=Vsn, install_dir=InstallDir}) ->
    io:format("+ removing package ~s-~s-~s from ~s~n", [User, Name, Vsn, InstallDir]),
    RemoveCmd = "rm -rf " ++ InstallDir,
    epm_util:print_cmd_output("~s~n", [RemoveCmd]),
    epm_util:do_cmd(RemoveCmd, fail),
    dets:delete(epm_index, {User,Name,Vsn}).

%% -----------------------------------------------------------------------------
%% UPDATE
%% -----------------------------------------------------------------------------
update_package(GlobalConfig, Package) ->
	Repo = Package#package.repo,
    Vsn = Package#package.vsn,
	%% switch to build home dir
	epm_util:set_cwd_build_home(GlobalConfig),
	
	%% download correct version of package
	LocalProjectDir = apply(Repo#repository.api_module, download_package, [Repo, Vsn]),
	
	%% switch to project dir
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:set_cwd(LocalProjectDir),
	
	%% build/install project
	_InstallDir = build_project(GlobalConfig, Package),
	
	%% switch to build home dir and delete cloned project
	epm_util:set_cwd_build_home(GlobalConfig),
	epm_util:del_dir(LocalProjectDir).

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
	%% application:get_env(epm, )
	Fork = proplists:get_value(epm_fork, application:get_all_env(epm), "JacobVorreuter"),
	Url = "http://github.com/" ++ Fork ++ "/epm/raw/master/epm",
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
%% Global Config
%% -----------------------------------------------------------------------------    		
print_config_values(GlobalConfig) ->
	[io:format("~p\t\t~p~n", [K,V]) || {K,V} <- GlobalConfig].
	
write_config_file(GlobalConfig) ->
	FileLoc = get(global_config),
	case file:open(FileLoc, [write]) of
		{ok, IoDevice} ->
			io:format(IoDevice, "[~n", []),
			lists:foldl(
				fun({Key, Val}, Count) ->
					if
						Count == 0 -> ok;
						true ->
							io:format(IoDevice, ",~n", [])
					end,
					case {Key,Val} of
						{repo_plugins, [C|_]} when is_integer(C) ->
							io:format(IoDevice, "  {~p, ~s}", [Key, Val]);
						_ ->
							io:format(IoDevice, "  {~p, ~p}", [Key, Val])
					end,
					Count+1
				end, 0, GlobalConfig),
			io:format(IoDevice, "~n].~n", []),
			io:format("+ updated .epm config~n");
		{error, Reason} ->
			?EXIT("failed to update .epm config (~s): ~p", [FileLoc, Reason])
	end.

%% -----------------------------------------------------------------------------
%% Read vsn
%% -----------------------------------------------------------------------------    
read_vsn_from_args([{tag, Tag}|_], _) -> Tag;
read_vsn_from_args([{branch, Branch}|_], _) -> Branch;
read_vsn_from_args([{sha, Sha}|_], _) -> Sha;
read_vsn_from_args([_|Tail], Default) -> read_vsn_from_args(Tail, Default);
read_vsn_from_args([], Default) -> Default.
		
build_project(GlobalConfig, Package) ->
    ProjectName = (Package#package.repo)#repository.name,
    Props = Package#package.args,
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
	case proplists:get_value(build_command, Config) of
		undefined ->
			case filelib:is_regular("Makefile") of
				true ->
					build1(ProjectName, "make");
				false ->
					case os:find_executable("rebar") of
						false ->
							exit("failed to build package: No Makefile and rebar not installed");
						RebarExec ->
							io:format("+ compiling with rebar...~n"),
							build1(ProjectName, RebarExec ++ " compile")
					end
			end;
		Cmd ->
			build1(ProjectName, Cmd)
	end;
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
%% Compile list of dependencies
%% -----------------------------------------------------------------------------
package_dependencies(GlobalConfig, Packages) ->
    RepoPlugins = proplists:get_value(repo_plugins, GlobalConfig, ?DEFAULT_API_MODULES),
	G = digraph:new(),
    UpdatedPackages = package_dependencies1(Packages, RepoPlugins, G, undefined, dict:new()),
    Deps = digraph_utils:topsort(G),
    digraph:delete(G),
    [dict:fetch(Dep, UpdatedPackages) || Dep <- Deps].
   
package_dependencies1([], _, _, _, Dict) -> Dict;
package_dependencies1([Package|Tail], RepoPlugins, G, Parent, Dict) ->
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
    
	PkgVsn =
		case Package#package.vsn of
			undefined -> apply(Repo#repository.api_module, default_vsn, []);	
			_ -> Package#package.vsn
		end,
		
    {Deps, Dict1} = 
        case WithoutDeps of
            true ->
                {[], Dict};
            false ->
                Deps0 = apply(Repo#repository.api_module, package_deps, [Repo#repository.owner, Repo#repository.name, PkgVsn]),
                lists:mapfoldl(
                    fun({Dep, Args}, TempDict) -> 
                        {DepName, DepUser} = split_package(Dep),
                        DepVsn = read_vsn_from_args(Args, apply(Repo#repository.api_module, default_vsn, [])),
                        Package0 = #package{
                            user = DepUser,
                            name = DepName,
                            vsn = DepVsn,
                            args = Args
                        },
                        TempDict1 = package_dependencies1([Package0], RepoPlugins, G, Key, TempDict),
                        {{DepUser, DepName, DepVsn}, TempDict1} 
                    end, Dict, Deps0)
        end,
    
    Package1 = Package#package{
        user = Repo#repository.owner, 
        name = Repo#repository.name,
		vsn  = PkgVsn,
        deps = Deps,
        repo = Repo
    },
    package_dependencies1(Tail, RepoPlugins, G, Parent, dict:store(Key, Package1, Dict1)).

filter_installed_packages(Packages) ->
    filter_installed_packages(Packages, [], []).
    
filter_installed_packages([], Installed, NotInstalled) ->
    {lists:reverse(Installed), NotInstalled};
    
filter_installed_packages([Package|Tail], Installed, NotInstalled) ->
	case local_package_info(Package) of
        [] -> filter_installed_packages(Tail, Installed, [Package|NotInstalled]);
        [P|_] -> filter_installed_packages(Tail, [P|Installed], NotInstalled)
    end.

retrieve_remote_repo([], _, ProjectName) ->
    ?EXIT("failed to locate remote repo for ~s", [ProjectName]);
    
retrieve_remote_repo([Module|Tail], none, ProjectName) ->	
    case apply(Module, search, [ProjectName]) of
        [] ->
            retrieve_remote_repo(Tail, none, ProjectName);
        Repos when is_list(Repos) ->
            case lists:filter(fun(R1) -> R1#repository.name==ProjectName end, Repos) of
				[R0|_] -> R0;
				[] -> retrieve_remote_repo(Tail, none, ProjectName)
			end;
        Err ->
            ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
    end;

retrieve_remote_repo([Module|Tail], User, ProjectName) ->
	case apply(Module, info, [User, ProjectName]) of
		Repo when is_record(Repo, repository) -> 
		    Repo;
		undefined -> 
		    retrieve_remote_repo(Tail, User, ProjectName); 
		Err -> 
			?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
	end.
	
retrieve_remote_repos(Modules, User, ProjectName, IsExact) ->
    retrieve_remote_repos(Modules, User, ProjectName, IsExact, []).
    
retrieve_remote_repos([], _, _, _, Acc) -> Acc;
    
retrieve_remote_repos([Module|Tail], none, ProjectName, IsExact, Acc) ->	
    case apply(Module, search, [ProjectName]) of
        [] ->
            retrieve_remote_repos(Tail, none, ProjectName, IsExact, Acc);        
        Repos when is_list(Repos), IsExact==true ->
            case lists:filter(fun(R1) -> R1#repository.name==ProjectName end, Repos) of
				[] -> retrieve_remote_repos(Tail, none, ProjectName, IsExact, Acc);
				R0s -> retrieve_remote_repos(Tail, none, ProjectName, IsExact, lists:append(Acc, R0s))
			end;
        Repos when is_list(Repos) ->
            retrieve_remote_repos(Tail, none, ProjectName, IsExact, lists:append(Acc, Repos));
        Err ->
            ?EXIT("failed to locate remote repos for ~s: ~p", [ProjectName, Err])
    end;

retrieve_remote_repos([Module|Tail], User, ProjectName, IsExact, Acc) ->
	case apply(Module, info, [User, ProjectName]) of
		Repo when is_record(Repo, repository) -> 
		    retrieve_remote_repos(Tail, User, ProjectName, IsExact, Acc ++ [Repo]);
		undefined -> 
		    retrieve_remote_repos(Tail, User, ProjectName, IsExact, Acc);
		Err -> 
			?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
	end.
