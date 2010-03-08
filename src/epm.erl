-module(epm).
-export([main/1]).
-include("epm.hrl").

main(Args) ->
    io:format("epm v0.1.0, 2010~n~n"),
    
    inets:start(),

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
	
	%% consult global .epm config file in home directory
    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".epm") of
		{ok, [GlobalConfig], FileLoc} ->
			EpmHome = epm_util:epm_home_dir(Home),
			
			epm_util:open_dets_table(Home, EpmHome),
			
			put(global_config, FileLoc),
		    
			case proplists:get_value(install_dir, GlobalConfig) of
		        undefined -> ok;
		        InstallDir -> epm_util:add_to_path(InstallDir)
		    end,
			
			epm_core:execute(GlobalConfig, Args);
		{error, enoent} ->
			put(global_config, filename:join([Home, ".epm"])),
			epm_core:execute([], Args);
		{error, Reason} ->
			?EXIT("failed to read epm global config: ~p", [Reason])
	end.