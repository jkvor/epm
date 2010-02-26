### epm _IS_
* an Erlang package manager meant to have _minimal_ impact on projects
* a simple and easy dependency tracker

### epm _IS NOT_
* a packaging and deployment tool
* meant to handle non-Erlang dependencies

### Everything comes from GitHub

epm installs apps from GitHub and nowhere else (perhaps other sources will be added in the future).

### Sometimes GitHub sucks

epm uses the GitHub API extensively. Often times requests to the API timeout or fail. This sucks, but what can you do?

### Put a .epm config file in your home directory (optional)

this allows customization such as:

* build_path -- where to download and build packages (default is current working dir)
* install_path -- where to install (default is code:lib_dir())
* exclude -- list of apps to ignore dependencies for (default is [])

example ~/.epm file:

	[
		{build_path, "/tmp"},
		{install_path, "/Users/jvorreuter/dev"},
		{exclude, ["mochiweb"]}
	].
	
### How do apps specify their dependencies?
	
Each application in the epm dependency hierarchy may include an epm app config specifying its dependencies

look at this one: <http://github.com/JacobVorreuter/excavator/blob/master/excavator.epm>

myapp/myapp.epm
	[
		{deps, [
			{"clones/mochiweb", []},
			{"jacobvorreuter/log_roller", [{sha, "da5a7738c913383cbd06ca3a0139e6eaab03030f"}]},
			{"ngerakines/etap", [{tag, "0.3.4"}]}
		]}
	].
	
If this file is not present, it is assumed that the app has no dependencies
	
### Usage
    install [[<user>/]<project> [--tag <tag>] [--branch <branch>] [--sha <sha>],] [--verbose]
    remove [<project>,] [--verbose]
    update [<project>,] [--with-deps]
    info [<project>,] [--verbose]
    list [--verbose]

### Do it

search for an Erlang app  

	jvorreuter$ ./epm search excavator
	epm v0.1.0, 2010

	===============================
	AVAILABLE
	===============================
	  name: excavator
	  owner: JacobVorreuter
	  followers: 7
	  homepage: 
	  description: An Erlang application for ingesting data from various sources (APIs, data feeds, web content, etc)
	  tags:
	    "0.3"
	  branches:
	    master
	    scheduler

install that app (its dependencies will be installed too)  

	jvorreuter$ ./epm install excavator
	epm v0.1.0, 2010

	===============================
	Install the following packages?
	===============================
	    + clones-mochiweb-master
	    + JacobVorreuter-mochiweb_server_behavior-master
	    + epm-etap-master
	    + JacobVorreuter-dynamic_compile-master
	    + JacobVorreuter-mochixpath-master
	    + JacobVorreuter-excavator-master
	[y/n] y

	+ downloading http://github.com/clones/mochiweb/tarball/master
	+ running mochiweb build command
	+ running mochiweb install command
	+ downloading http://github.com/JacobVorreuter/mochiweb_server_behavior/tarball/master
	+ running mochiweb_server_behavior build command
	+ running mochiweb_server_behavior install command
	+ downloading http://github.com/epm/etap/tarball/master
	+ running etap build command
	+ running etap install command
	+ downloading http://github.com/JacobVorreuter/dynamic_compile/tarball/master
	+ running dynamic_compile build command
	+ running dynamic_compile install command
	+ downloading http://github.com/JacobVorreuter/mochixpath/tarball/master
	+ running mochixpath build command
	+ running mochixpath install command
	+ downloading http://github.com/JacobVorreuter/excavator/tarball/master
	+ running excavator build command
	+ running excavator install command

get some info about that app you just installed  

	jvorreuter$ ./epm info excavator
	epm v0.1.0, 2010

	===============================
	INSTALLED
	===============================
	  name: excavator
	  owner: JacobVorreuter
	  vsn: master
	  install dir: /Users/jvorreuter/dev/excavator-0.3
	  homepage: 
	  description: An Erlang application for ingesting data from various sources (APIs, data feeds, web content, etc)
	  dependencies: 
	    clones/mochiweb/master
	    mochixpath/master
	    dynamic_compile/master
	    epm/etap/master
	    mochiweb_server_behavior/master

how 'bout a list of all apps I've installed?  

	jvorreuter$ ./epm list
	epm v0.1.0, 2010

	===============================
	INSTALLED
	===============================
	  name: excavator
	  owner: JacobVorreuter
	  vsn: master
	  install dir: /Users/jvorreuter/dev/excavator-0.3
	  homepage: 
	  description: An Erlang application for ingesting data from various sources (APIs, data feeds, web content, etc)
	  dependencies: 
	    clones/mochiweb/master
	    mochixpath/master
	    dynamic_compile/master
	    epm/etap/master
	    mochiweb_server_behavior/master

	  name: mochixpath
	  owner: JacobVorreuter
	  vsn: master
	  install dir: /Users/jvorreuter/dev/mochixpath-0.1
	  homepage: 
	  description: Mochiweb html parser xpath extension

	  name: dynamic_compile
	  owner: JacobVorreuter
	  vsn: master
	  install dir: /Users/jvorreuter/dev/dynamic_compile-0.1
	  homepage: 
	  description: compile and load erlang modules from string input

	  name: mochiweb_server_behavior
	  owner: JacobVorreuter
	  vsn: master
	  install dir: /Users/jvorreuter/dev/mochiweb_server_behavior-0.1
	  homepage: 
	  description: Erlang behavior for a simple mochiweb web server
	  dependencies: 
	    clones/mochiweb/master

	  name: etap
	  owner: epm
	  vsn: master
	  install dir: /Users/jvorreuter/dev/etap-0.3.4
	  homepage: 
	  description: etap is a simple erlang testing library that provides TAP compliant output.

	  name: mochiweb
	  owner: clones
	  vsn: master
	  install dir: /Users/jvorreuter/dev/mochiweb-0.01
	  homepage: http://code.google.com/p/mochiweb/
	  description: mochiweb clone

what have I done? I must remove that terrible app and all of its dependencies  

	jvorreuter$ ./epm remove excavator
	epm v0.1.0, 2010

	===============================
	Remove the following packages?
	===============================
	    + clones-mochiweb-master
	    + JacobVorreuter-excavator-master
	    + JacobVorreuter-dynamic_compile-master
	    + JacobVorreuter-mochiweb_server_behavior-master
	    + JacobVorreuter-mochixpath-master
	    + epm-etap-master
	[y/n] y

	+ removing package clones-mochiweb-master from /Users/jvorreuter/dev/mochiweb-0.01
	+ removing package JacobVorreuter-excavator-master from /Users/jvorreuter/dev/excavator-0.3
	+ removing package JacobVorreuter-dynamic_compile-master from /Users/jvorreuter/dev/dynamic_compile-0.1
	+ removing package JacobVorreuter-mochiweb_server_behavior-master from /Users/jvorreuter/dev/mochiweb_server_behavior-0.1
	+ removing package JacobVorreuter-mochixpath-master from /Users/jvorreuter/dev/mochixpath-0.1
	+ removing package epm-etap-master from /Users/jvorreuter/dev/etap-0.3.4
	