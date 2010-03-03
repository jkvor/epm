-module(api_behavior).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{package_deps, 1}, {search, 1}, {info, 1}];
behaviour_info(_) -> undefined.

%% package_deps(Project) -> Deps
%%  Project = project()
%%  Deps = [{Project, Args}]
%%  Project = string()
%%  Args = list()

%% search(ProjectName) -> Results
%%  ProjectName = string()
%%  Results = [repository()]

%% info(Project) -> Result
%%  Project = project()
%%  Result = undefined | repository()