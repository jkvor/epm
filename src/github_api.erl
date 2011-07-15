-module(github_api).
-behaviour(api_behavior).

-export([package_deps/3, search/1, info/2, tags/2, branches/2, download_package/2, default_vsn/0]).

-include_lib("xmerl/include/xmerl.hrl").
-include("epm.hrl").
-define(HOST, "github.com").

package_deps(User, ProjectName, Vsn) ->
    if
        User == undefined -> ?EXIT("get_package_deps/3 user cannot be undefined",[]);
        true -> ok
    end,
    if
        ProjectName == undefined -> ?EXIT("get_package_deps/3 name cannot be undefined",[]);
        true -> ok
    end,
    if
        Vsn == undefined -> ?EXIT("get_package_deps/3 vsn cannot be undefined",[]);
        true -> ok
    end,
    Url = lists:flatten(io_lib:format("https://github.com/~s/~s/raw/~s/~s.epm", [User, ProjectName, Vsn, ProjectName])),
    case epm_util:request_as_str(Url, ?HOST) of
        Body when is_list(Body) -> proplists:get_value(deps, epm_util:eval(Body), []);
        _ -> []
    end.
 
search(ProjectName) ->
    case request_as_xml("https://github.com/api/v2/xml/repos/search/" ++ ProjectName) of
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
            						pushed = repo_xml_field("pushed", Repo),
            						api_module = ?MODULE
								}|Acc];
							_ ->
								Acc
						end;
					(_, Acc) -> Acc
			end, [], Repos)),
			case info("epm", ProjectName) of
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
	
info(User, ProjectName) -> 
	case request_as_xml("https://github.com/api/v2/xml/repos/show/" ++ User ++ "/" ++ ProjectName) of
		#xmlElement{name=repository}=Repo ->
			case xmerl_xpath:string("/repository/name/text()", Repo) of
				[#xmlText{value=Name}] ->					
					#repository{
						name = Name,
						owner = repo_xml_field("owner", Repo),
						description = repo_xml_field("description", Repo),
						homepage = repo_xml_field("homepage", Repo),
						followers = repo_xml_field("followers", Repo),
						pushed = repo_xml_field("pushed", Repo),
						api_module = ?MODULE
					};
				_ ->
					undefined
			end;
		Err ->
			Err
	end.

tags(User, ProjectName) ->
    Url = "https://github.com/api/v2/yaml/repos/show/" ++ User ++ "/" ++ ProjectName ++ "/tags",
    case epm_util:request_as_str(Url, ?HOST) of
        "--- \ntags: {}\n\n" -> [];
        "--- \ntags: \n" ++ Body -> 
            [begin
                [K,_V] = string:tokens(Token, ":"),
                string:strip(K)
             end || Token <- string:tokens(Body, "\n")];
        _ -> []
    end.

branches(User, ProjectName) ->
    Url = "https://github.com/api/v2/yaml/repos/show/" ++ User ++ "/" ++ ProjectName ++ "/branches",
    case epm_util:request_as_str(Url, ?HOST) of
        "--- \nbranches: {}\n\n" -> [];
        "--- \nbranches: \n" ++ Body -> 
            [begin
                [K,_V] = string:tokens(Token, ":"),
                string:strip(K)
             end || Token <- string:tokens(Body, "\n")];
		_ -> []
	end.

download_package(Repo, Vsn) ->
    Url = lists:flatten(io_lib:format("https://github.com/~s/~s/tarball/~s", [Repo#repository.owner, Repo#repository.name, Vsn])),
	epm_package:download_tarball(Repo, Url).
			
default_vsn() -> "master".

repo_xml_field(FieldName, Repo) ->
    case xmerl_xpath:string("/repository/" ++ FieldName ++ "/text()", Repo) of
		[#xmlText{value=Value}] -> Value;
		_ -> undefined
	end.
		
request_as_xml(Url) ->
    case epm_util:request_as_str(Url, ?HOST) of
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
