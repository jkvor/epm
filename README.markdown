### epm _IS_
* an Erlang package manager meant to have _minimal_ impact on projects
* a simple and easy dependency tracker

### epm _IS NOT_
* a packaging and deployment tool
* meant to handle non-Erlang dependencies

### USAGE

__Install__
	epm install [[<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>],] [--verbose]

__Info__
	epm info [[<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>],] [--verbose]

__Remove__ a given project  
	epm remove [<project>,] [--verbose]

### Global epm config values (optional)

* build_path -- where to download and build packages (default is current working dir)
* install_path -- where to install (default is code:lib_dir())
* exclude -- list of apps to ignore dependencies for (default is [])

~/.epm
	[
		{build_path, "/tmp"},
		{install_path, "/usr/local/lib"},
		{exclude, ["mochiweb"]}
	].

### Application epm config

Each application in the epm dependency hierarchy may include an epm app config specifying the following:
* deps (optional) -- List of application dependancies

myapp/myapp.epm
	[
		{deps, [
			{"ngerakines/mochiweb", [{tag, "1.0"}]},
			{"jacobvorreuter/log_roller", [{sha, "da5a7738c913383cbd06ca3a0139e6eaab03030f"}]},
			{"ngerakines/etap", [{tag, "0.3.4"}]}
		]}
	].
