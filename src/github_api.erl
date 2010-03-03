-module(github_api).
-behaviour(api_behavior).

-export([package_deps/1, search/1, info/1]).

-include("epm.hrl").

package_deps(#project{user=User, name=Name, vsn=Vsn}) ->
    if
        User == undefined -> ?EXIT("get_package_deps/1 user cannot be undefined");
        true -> ok
    end,
    if
        Name == undefined -> ?EXIT("get_package_deps/1 name cannot be undefined");
        true -> ok
    end,
    if
        Vsn == undefined -> ?EXIT("get_package_deps/1 vsn cannot be undefined");
        true -> ok
    end,
    Url = lists:flatten(io_lib:format("http://github.com/~s/~s/raw/~s/~s.epm", [User, Name, Vsn, Name])),
    case request_as_str(Url) of
        Body when is_list(Body) -> proplists:get_value(deps, epm_util:eval(Body), []);
        _ -> []
    end.
 
search(ProjectName) ->
    case git_request_as_xml("http://github.com/api/v2/xml/repos/search/" ++ ProjectName) of
		#xmlElement{name=repositories, content=Repos} ->
			Found = lists:reverse(lists:foldl(
				fun (#xmlElement{name=repository}=Repo, Acc) ->
						case {xmerl_xpath:string("/repository/name/text()", Repo),
							  xmerl_xpath:string("/repository/language/text()", Repo),
							  xmerl_xpath:string("/repository/type/text()", Repo)} of
							{[#xmlText{value=Name}], [#xmlText{value="Erlang"}], [#xmlText{value="repo"}]} ->
								[#repository{
									name = Name,
            						owner = repo_xml_field("username", Repo),
            						description = repo_xml_field("description", Repo),
            						homepage = repo_xml_field("homepage", Repo),
            						followers = repo_xml_field("followers", Repo),
            						pushed = repo_xml_field("pushed", Repo)
								}|Acc];
							_ ->
								Acc
						end;
					(_, Acc) -> Acc
			end, [], Repos)),
			case repos_info("epm", ProjectName) of
			    #repository{}=R0 ->
			        case lists:filter(
			                fun(R1) ->
			                    R1#repository.name==R0#repository.name andalso R1#repository.owner==R0#repository.owner
			                end, Found) of
			            [] -> [R0|Found];
			            _ -> Found
			        end;
			    _ ->
			        Found
			end;
		Err -> 
		    Err
	end.
	
info(Project) when is_record(Project, project) -> 
	case git_request_as_xml("http://github.com/api/v2/xml/repos/show/" ++ User ++ "/" ++ ProjectName) of
		#xmlElement{name=repository}=Repo ->
			case xmerl_xpath:string("/repository/name/text()", Repo) of
				[#xmlText{value=Name}] ->					
					#repository{
						name = Name,
						owner = repo_xml_field("owner", Repo),
						description = repo_xml_field("description", Repo),
						homepage = repo_xml_field("homepage", Repo),
						followers = repo_xml_field("followers", Repo),
						pushed = repo_xml_field("pushed", Repo)
					};
				_ ->
					undefined
			end;
		Err ->
			Err
	end.
	
repo_xml_field(FieldName, Repo) ->
    case xmerl_xpath:string("/repository/" ++ FieldName ++ "/text()", Repo) of
		[#xmlText{value=Value}] -> Value;
		_ -> undefined
	end.

repos_tags(User, ProjectName) ->
    Url = "http://github.com/api/v2/yaml/repos/show/" ++ User ++ "/" ++ ProjectName ++ "/tags",
    case git_request_as_str(Url) of
        "--- \ntags: {}\n\n" -> [];
        "--- \ntags: \n" ++ Body -> 
            [begin
                [K,V] = string:tokens(Token, ":"),
                {string:strip(K), string:strip(V)} 
             end || Token <- string:tokens(Body, "\n")];
        _ -> []
    end.
	
repos_branches(User, ProjectName) ->
    Url = "http://github.com/api/v2/yaml/repos/show/" ++ User ++ "/" ++ ProjectName ++ "/branches",
    case git_request_as_str(Url) of
        "--- \nbranches: {}\n\n" -> [];
        "--- \nbranches: \n" ++ Body -> 
            [begin
                [K,V] = string:tokens(Token, ":"),
                {string:strip(K), string:strip(V)} 
             end || Token <- string:tokens(Body, "\n")];
		_ -> []
	end.
		
git_request_as_xml(Url) ->
    case git_request_as_str(Url) of
        Body when is_list(Body) ->
            case xmerl_scan:string(Body) of
                {XmlElement, _} ->
                    XmlElement;
                _ ->
                    poorly_formatted_xml
            end;
        Err ->
            Err
    end.
    
request_as_str(Url) ->
    case http:request(get, {Url, [{"User-Agent", "EPM"}, {"Host", "github.com"}]}, [{timeout, 6000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
	        Body;
	    {ok, {{_, 403, _}, _, _}} ->
	        not_found;
        {ok, {{_, 404, _}, _, _}} ->
	        not_found;
	    {error, Reason} ->
	        io:format("timeout? ~p~n", [Reason]),
	        Reason
	end.

download_package(User, ProjectName, Vsn) ->
    Repo = retrieve_remote_repo(User, ProjectName),
    LocalProjectDir = Repo#repository.owner ++ "-" ++ Repo#repository.name,
    Url = lists:flatten(io_lib:format("http://github.com/~s/~s/tarball/~s", [Repo#repository.owner, Repo#repository.name, Vsn])),
    io:format("+ downloading ~s~n", [Url]),
    case http:request(get, {Url, [{"User-Agent", "EPM"}, {"Host", "github.com"}]}, [{timeout, 6000}], [{body_format, binary}]) of
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
                            exit(lists:flatten(io_lib:format("failed to extract ~s tarball: ~p", [ProjectName, Reason])))
                    end;
                {error, Reason1} ->
                    exit(lists:flatten(io_lib:format("failed to extract ~s tarball: ~p", [ProjectName, Reason1])))
            end;
        {ok, {{_,404,_},_,_}} ->
            exit(lists:flatten(io_lib:format("remote project does not exist: ~s/~s/~s", [Repo#repository.owner, Repo#repository.name, Vsn])));
        Error ->
            io:format("~p~n", [Error]),
            exit(lists:flatten(io_lib:format("failed to download ~s tarball", [ProjectName])))
    end.