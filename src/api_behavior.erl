-module(api_behavior).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
	{package_deps, 3}, 
	{search, 1}, 
	{info, 2}, 
	{tags, 2}, 
	{branches, 2},
	{download_package, 2}];
behaviour_info(_) -> undefined.

%% package_deps(User, ProjectName, Vsn) -> Deps
%%  User = string()
%%  ProjectName = string()
%%  Vsn = string()
%%  Deps = [{Project, Args}]
%%  Project = string()
%%  Args = list()

%% search(ProjectName) -> Results
%%  ProjectName = string()
%%  Results = [repository()]

%% info(User, ProjectName) -> Result
%%  User = string()
%%  ProjectName = string()
%%  Result = undefined | repository()

%% tags(User, ProjectName) -> Result
%%  User = string()
%%  ProjectName = string()
%%  Result = [Tag]
%%  Tag = string()

%% branches(User, ProjectName) -> Result
%%  User = string()
%%  ProjectName = string()
%%  Result = [Branch]
%%  Branch = string()

%% download_package(User, ProjectName, Vsn) -> Result
%%  User = string()
%%  ProjectName = string()
%%  Vsn = string()
%%  Result = 