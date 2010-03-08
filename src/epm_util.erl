-module(epm_util).
-compile(export_all).
-include("epm.hrl").

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
    
open_dets_table(Home, EpmHome) ->
    File = filename:join([EpmHome, "epm_index"]),

	%% TODO: delete this later
	Insert = 
		case filelib:is_regular(filename:join([Home, "epm_index"])) of
			true ->
				case dets:open_file(epm_index, [{type, set}, {file, filename:join([Home, "epm_index"])}]) of
					{ok, _} ->
						Rows = dets:match(epm_index, '$1'),
						dets:close(epm_index),
						
						[{{User,Name,Vsn}, #package{
							user = User,
							name = Name,
							vsn = Vsn,
							install_dir = InstallDir,
							deps = Deps,
							repo = github_api:info(User, Name)
						 }} || [{{User,Name,Vsn},InstallDir,Deps}] <- Rows];
					_ -> []
				end;
			false -> []
		end,

    case dets:open_file(epm_index, [{type, set}, {file, File}]) of
	    {ok, _} -> 
			%% TODO: delete this later
			[dets:insert(epm_index, I) || I <- Insert],
			file:delete(filename:join([Home, "epm_index"])),
			ok;
	    {error, {file_error,_,eacces}} ->
            ?EXIT("insufficient access to epm index file: ~s", [File]);
	    {error, Reason} ->
	        ?EXIT("failed to open epm index file (~s): ~p", [File, Reason])
	end.
	
eval(Str) ->
    case erl_scan:string(Str) of
        {ok,Tokens,_} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok,Forms} ->
                    case erl_eval:exprs(Forms,[]) of
                        {value, Terms, _} -> Terms;
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

add_to_path(InstallDir) ->
	case file:list_dir(InstallDir) of
		{ok, Files} ->
		    [begin
		        File = filename:join([InstallDir, File0]),
		        case filelib:is_dir(File) of
		            true ->
		                Ebin = filename:join([File, "ebin"]),
		                case filelib:wildcard(Ebin ++ "/*.app") of
		                    [App|_] ->
		                        case file:consult(App) of
		                            {ok, _} ->
		                                code:add_pathz(Ebin);
		                            _ ->
		                                ok
		                        end;
		                    _ ->
		                        ok
		                end;
		            false -> ok
		        end
		     end || File0 <- Files];
		_ ->
		    ok
	end.
	    
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
	
rn_dir(OldName, NewName) ->
    case file:rename(OldName, NewName) of
        ok -> ok;
        {error, Reason} ->
            exit(lists:flatten(io_lib:format("failed to rename ~s to ~s: ~p", [OldName, NewName, Reason])))
    end.
	
do_cmd(Cmd, fail) ->
	case do_cmd(Cmd) of
		{0, ""} ->
			ok;
		{0, Output} ->
			print_cmd_output("~s~n", [Output]);
		{_, Output} ->
			exit(Output)
	end.
	
do_cmd(Cmd) ->
    Results = string:tokens(os:cmd(Cmd ++ "; echo $?"), "\n"),
    [ExitCode|Other] = lists:reverse(Results),
    {list_to_integer(ExitCode), string:join(lists:reverse(Other), "\n")}.
    
print_cmd_output(Format, Args) ->
	case get(verbose) of
		undefined -> print_cmd_output(Format, Args, false);
		Verbose -> print_cmd_output(Format, Args, Verbose)
	end.
	
print_cmd_output(_, _, false) -> ok; %% do not print verbose output
print_cmd_output(Format, Args, true) ->
	Str = lists:flatten(io_lib:format("    " ++ Format, Args)),
	Output0 = re:replace(Str, "\n", "\n    ", [global, {return, list}]),
	Output = re:replace(Output0, "\~", "", [global, {return, list}]),
	io:format(string:substr(Output, 1, length(Output)-4), []).

set_cwd_build_home(GlobalConfig) ->	
	set_cwd(proplists:get_value(build_dir, GlobalConfig, ".")).
	
set_cwd(Dir) ->
	case file:set_cwd(Dir) of
		ok -> 
			ok;
		{error, _} ->
			exit(lists:flatten(io_lib:format("failed to change working directory: ~s", [Dir])))
	end.