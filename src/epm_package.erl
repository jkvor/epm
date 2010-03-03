-module(epm_package).
-export([download_tarball/2]).

epm_package:download_tarball(Repo, Url) ->
	LocalProjectDir = Repo#repository.owner ++ "-" ++ Repo#repository.name,
	io:format("+ downloading ~s~n", [Url]),
    case http:request(get, {Url, [{"User-Agent", "EPM"}]}, [{timeout, 6000}], [{body_format, binary}]) of
        {ok,{{_,200,_},_,Bin}} ->
            case erl_tar:table({binary, Bin}, [compressed]) of
                {ok,[_,TarName|_]} ->
                    epm_util:del_dir(TarName),
                    epm_util:del_dir(LocalProjectDir),
                    case erl_tar:extract({binary, Bin}, [compressed]) of
                        ok -> 
                            epm_util:rn_dir(TarName, LocalProjectDir),
                            LocalProjectDir;
                        {error, Reason} ->
                            ?EXIT("failed to extract ~s tarball: ~p", [Repo#repository.name, Reason])
                    end;
                {error, Reason1} ->
                    ?EXIT("failed to extract ~s tarball: ~p", [Repo#repository.name, Reason1])
            end;
        {ok, {{_,404,_},_,_}} ->
            ?EXIT("remote project does not exist: ~s/~s/~s", [Repo#repository.owner, Repo#repository.name, Vsn]);
        Error ->
            io:format("~p~n", [Error]),
            ?EXIT("failed to download ~s tarball", [Repo#repository.name])
    end.