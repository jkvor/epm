### epm _IS_
* an Erlang package manager meant to have _minimal_ impact on projects
* a simple and easy dependency tracker

### epm _IS NOT_
* a packaging and deployment tool
* meant to handle non-Erlang dependencies

### USAGE

__Install__ a given project  
	epm install [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>] [--force] [--verbose]
	epm install JacobVorreuter/excavator
	epm install JacobVorreuter/excavator --tag 0.3
	epm install JacobVorreuter/excavator --branch scheduler
	epm install JacobVorreuter/excavator --sha d3dc9a345000b73401b214da55f6e6af3629d39a

__Info__ regarding project details  
	epm info [<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>] [--verbose]
	epm info ngerakines/etap

__Remove__ a given project  
	epm remove <project> [--verbose]
	epm remove etap

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
	].

### Application epm config

Each application in the epm dependency hierarchy must include an epm app config specifying the following:
* deps (optional) -- List of application dependancies described as {app_name, <user>/<project>/[<tag>][<branch>][<sha>]}
* prebuild_command (optional) -- the command to run before build (skipped by default)
* build_command (optional) -- the command to run to build (defaults to "make")
* install_command (optional) -- the command to run after build, to install (defaults to "make install")
* test_command (optional) -- the command to run after build, to test (defaults to "make test")
* stable (optional) -- the last stable sha for this tree

myapp/myapp.epm
	[
		{deps, [
			{"ngerakines/mochiweb", [{tag, "1.0"}]},
			{"jacobvorreuter/log_roller", [{sha, "da5a7738c913383cbd06ca3a0139e6eaab03030f"}]},
			{"ngerakines/etap", [{tag, "0.3.4"}]}
		]},
		{build_command, "make"},
		{install_command, "make install"},
		{test_command, "make test"}
	].
