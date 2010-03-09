%% OMFG, this API sucks. 
%%
%% explain to me how this is even remotely useful:
%%	jacobvorreuter$ curl "http://api.bitbucket.org/1.0/repositories/?name=rebar"
%%	{
%%	    "count": 12, 
%%	    "query": "rebar", 
%%	    "repositories": [
%%	        {
%%	            "website": "", 
%%	            "slug": "rebar", 
%%	            "name": "rebar", 
%%	            "followers_count": 0, 
%%	            "description": ""
%%	        }, 
%%	        {
%%	            "website": null, 
%%	            "slug": "rebar", 
%%	            "name": "rebar", 
%%	            "followers_count": 0, 
%%	            "description": ""
%%	        },
%%			...
%%		]
%%	}
%%
%% WHERE IS THE REPO OWNER'S NAME?!?!?!?!? Now I'm forced to scrape their fucking html...
%%
-module(bitbucket_api).
-behaviour(api_behavior).

-export([package_deps/3, search/1, info/2, tags/2, branches/2, download_package/2, default_vsn/0]).

-include_lib("xmerl/include/xmerl.hrl").
-include("epm.hrl").

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
    Url = lists:flatten(io_lib:format("http://bitbucket.org/~s/~s/raw/~s/~s.epm", [User, ProjectName, Vsn, ProjectName])),
    case request_as_str(Url) of
        Body when is_list(Body) -> proplists:get_value(deps, epm_util:eval(Body), []);
        _ -> []
    end.
 
search(ProjectName) ->
    case request_as_xml("http://bitbucket.org/repo/all/?name=" ++ ProjectName) of
		{html,_,_}=Html ->
			case search_xml_for_repos(Html) of
				undefined -> [];
				Repos -> extract_repo_info_from_html(Repos)
			end;
		Err ->
			Err
	end.
	
info(User, ProjectName) -> 
	case search(ProjectName) of
		[#repository{}|_]=Repos ->
			case lists:filter(
				fun(Repo) ->
					Repo#repository.owner==User andalso
					Repo#repository.name==ProjectName
				end, Repos) of
				[R|_] -> R;
				_ -> undefined
			end;
		[] -> 
			undefined;
		Err ->
			Err
	end.
	
tags(User, ProjectName) ->
    Xml = request_as_xml(lists:flatten(io_lib:format("http://api.bitbucket.org/1.0/repositories/~s/~s/tags/?format=xml", [User, ProjectName]))),
	case Xml of
		{ehtml,[],[{'?xml',_,[{response,[],Tags}]}]} ->
			[atom_to_list(Tag) || {Tag,_,_} <- Tags];
		_ ->
			[]
	end.

branches(User, ProjectName) ->
    Xml = request_as_xml(lists:flatten(io_lib:format("http://api.bitbucket.org/1.0/repositories/~s/~s/branches/?format=xml", [User, ProjectName]))),
    case Xml of
		{ehtml,[],[{'?xml',_,[{response,[],Branches}]}]} ->
			[atom_to_list(Branch) || {Branch,_,_} <- Branches];
		_ ->
			[]
	end.

download_package(Repo, Vsn) ->
    Url = lists:flatten(io_lib:format("http://bitbucket.org/~s/~s/get/~s.tar.gz", [Repo#repository.owner, Repo#repository.name, Vsn])),
	epm_package:download_tarball(Repo, Url).
					
default_vsn() -> "tip".

request_as_xml(Url) ->
    case request_as_str(Url) of
        Body when is_list(Body) ->
            case yaws_html:h2e(Body) of
				{ehtml,[],[_,Xml]} -> Xml;
				_ -> poorly_formatted_xml
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
	    {ok, _} ->
	        request_failed;
	    {error, Reason} ->
	        io:format("timeout? ~p~n", [Reason]),
	        Reason
	end.
	
%% fake xpaths

search_xml_for_repos({'div',[{class,"repos-all"}|_],Repos}) -> 
	Repos;
search_xml_for_repos({_,_,Children}) ->
	search_xml_for_repos(Children);	
search_xml_for_repos([Child|Tail]) when not is_integer(Child) ->
	case search_xml_for_repos(Child) of
		undefined ->
			search_xml_for_repos(Tail);
		Repos ->
			Repos
	end;
search_xml_for_repos(_) -> undefined.

extract_repo_info_from_html(Repos) ->
	lists:foldl(
		fun({'div',_,Props}, Acc) ->
			case Props of
				[{'div',_,_},{span,[],[{a,_,User},_,{a,_,RepoName}]},{br,_},Desc|_] ->
					Desc1 = string:strip(re:replace(Desc, "[\\t\\n]", "", [global, {return, list}])),
					Desc2 = 
						case Desc1 of
							"Clone URL" ++ _ -> undefined;
							_ -> Desc1
						end,
					[#repository{
						owner = User,
						name = RepoName,
						description = Desc2,
						api_module = ?MODULE
					}|Acc];
				_ ->
					Acc
			end
		end, [], lists:reverse(Repos)).
		