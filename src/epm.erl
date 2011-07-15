-module(epm).
-export([main/1]).
-include("epm.hrl").

main(Args) ->
	put(vsn, "0.1.1"),
	
    io:format("epm v~s, 2010~n~n", [get(vsn)]),
    
    inets:start(),
	lists:map(fun application:start/1, [crypto, public_key, ssl, epm]),
    inets:start(httpc, [{profile, epm}]),

	case (catch main1(Args)) of
		{'EXIT', ErrorMsg} when is_list(ErrorMsg) ->
			io:format("- ~s~n", [ErrorMsg]);
		{'EXIT', Other} ->
			io:format("~p~n", [Other]);
		_ ->
			ok
	end,
	
	dets:close(epm_index),
	io:format("~n").
	
main1(Args) ->	
	Home = epm_util:home_dir(),
	EpmHome = epm_util:epm_home_dir(Home),
	epm_util:open_dets_table(Home, EpmHome),
	
	%% consult global .epm config file in home directory
    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".epm") of
		{ok, [GlobalConfig], FileLoc} ->
			
			put(global_config, FileLoc),
		    
			case proplists:get_value(install_dir, GlobalConfig) of
		        undefined -> 
					io:format("################ Warning ################~n"),
					io:format("You have not specified a value for ~n"),
					io:format("install_dir in your .epm config file. The~n"),
					io:format("current working directory will be used.~n~n"),
					io:format("run `epm config --set install_dir <path>`~n"),
					io:format("#########################################~n~n"),
					ok;
		        InstallDir -> epm_util:add_to_path(InstallDir)
		    end,
			
			setup_connectivity(GlobalConfig),
			
			epm_core:execute(GlobalConfig, Args);
		{ok, [], FileLoc} ->
			put(global_config, FileLoc),
			epm_core:execute([], Args);
		{error, enoent} ->
			file:write_file(filename:join([Home, ".epm"]), <<>>),
			put(global_config, filename:join([Home, ".epm"])),
			epm_core:execute([], Args);
		{error, Reason} ->
			?EXIT("failed to read epm global config: ~p", [Reason])
	end.

setup_connectivity(GlobalConfig) ->
    case proplists:get_value(proxy_host, GlobalConfig) of
        undefined -> ok;
        Host ->
            Port = proplists:get_value(proxy_port, GlobalConfig, "8080"),
            epm_util:set_http_proxy(Host, Port)
    end,
    Timeout = proplists:get_value(net_timeout, GlobalConfig, 6000),
    epm_util:set_net_timeout(Timeout).
