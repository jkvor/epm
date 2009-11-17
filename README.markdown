### epm _IS_
* an Erlang package manager meant to have _minimal_ impact on projects
* a simple and easy deployment and dependency solution

### epm _IS NOT_
* meant to handle non-Erlang dependencies

### USAGE

__Install__ a given project  
	epm install [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>] [--force]

__Test__ a given project  
	epm test [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>]

__List dependencies__ of a project  
	epm list_deps [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>]

__Info__ regarding project details  
	epm info [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>]

__Remove__ a given project  
	epm remove <project>

__Release__ package created for a project and its dependencies according to the OTP release spec  
	epm release [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>]

### Global epm config

* git_paths -- List of git repository base paths to search against
* build_path -- Where to check-out and build packages
* install_path -- Where to install
* exclude -- List of apps to ignore dependancies for, optional but nice

~/.epm
	[
		{git_paths, ["git://github.com/<user>/<project>.git"]},
		{build_path, "/tmp"},
		{install_path, "/usr/local/lib"},
		{exclude, [mochiweb]}
	]

### Application epm config

Each application in the epm dependency hierarchy must include an epm app config specifying the following:
* deps -- List of application dependancies described as {app_name, <user>/<project>/[<tag>][<branch>][<sha>]}
* prebuild_command (optional) -- the command to run before build
* build_command (optional) -- the command to run to build
* install_command (optional) -- the command to run after build, to install
* test_command (optional) -- the command to run after build, to test
* stable (optional) -- the last stable sha for this tree

myapp/myapp.epm
	[
		deps, [
			{mochiweb, "2.0"},
			{log_roller, "da5a7738c913383cbd06ca3a0139e6eaab03030f"},
			{etap, "master"}
		]
	]

### epm Application Specification
* conforms to the OTP application spec
* includes an epm app config