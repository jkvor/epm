-module(epm).
-export([main/1]).
-include("epm.hrl").

main(Args) ->
    io:format("epm v0.1.0, 2010~n~n"),
    
    %% Make sure the caller is running w/ r13b03
    case erlang:system_info(version) < "5.7.4" of
        true ->
            io:format("EPM requires at least erts 5.7.4; this VM is using ~s\n", [erlang:system_info(version)]),
            halt(1);
        false ->
            ok
    end,

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
	Home = home_dir(),
	EpmHome = epm_home_dir(Home),
	
	open_dets_table(EpmHome),
		
	%% consult global .epm config file in home directory
    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".epm") of
		{ok, [GlobalConfig], _} ->
		    case proplists:get_value(install_dir, GlobalConfig) of
		        undefined -> ok;
		        InstallDir -> epm_util:add_to_path(InstallDir)
		    end,
			epm_core:execute(GlobalConfig, Args);
		{error, enoent} ->
			epm_core:execute([], Args);
		{error, Reason} ->
			?EXIT("failed to read epm global config: ~p", [Reason])
	end.
	
home_dir() ->
    case init:get_argument(home) of
		{ok, [[H]]} -> [H];
		_ -> []
	end.
	
epm_home_dir(Home) ->
    EPM = filename:join([Home, "epm"]),
    case filelib:is_dir(EPM) of
        true -> EPM;
        false ->
            case file:make_dir(EPM) of
                ok -> EPM;
                {error, Reason} ->
                    ?EXIT("failed to create epm home directory (~s): ~p", [EPM, Reason])
            end
    end.
    
open_dets_table(EpmHome) ->
    File = filename:join([EpmHome, "epm_index"]),
    case dets:open_file(epm_index, [{type, set}, {file, File}]) of
	    {ok, _} -> ok;
	    {error, {file_error,_,eacces}} ->
            ?EXIT("insufficient access to epm index file: ~s", [File]);
	    {error, Reason} ->
	        ?EXIT("failed to open epm index file (~s): ~p", [File, Reason])
	end.