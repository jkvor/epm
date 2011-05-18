### epm _IS_
* an Erlang package manager meant to have _minimal_ impact on projects
* a simple and easy dependency tracker

### epm _IS NOT_
* a packaging and deployment tool
* meant to handle non-Erlang dependencies

### Install

	curl "https://github.com/JacobVorreuter/epm/raw/master/epm" > epm
	chmod +x epm
	sudo mv epm /usr/local/bin/
	
	epm config --set build_dir "/tmp"
	epm config --set install_dir "/Users/jvorreuter/erl_libs"
	
	
### Read the blog post

<http://www.jkvor.com/erlang-package-manager>

### Usage

	install [<user>/]<project> {project options}, ... {global options}
        project options:
             --tag <tag>
             --branch <branch>
             --sha <sha>
             --with-deps (default)
             --without-deps
             --prebuild-command <cmd>
             --build-command <cmd>
             --test-command <cmd>
        global options:
             --verbose
             --config-set <key> <value>

    remove [<user>/]<project> {project options}, ... {global options}
        project options:
             --tag <tag>
             --branch <branch>
             --sha <sha>
        global options:
             --verbose

             --config-set <key> <value>

    update [<user>/]<project> {project options}, ... {global options}
        project options:
             --tag <tag>
             --branch <branch>
             --sha <sha>
             --with-deps
             --without-deps (default)
        global options:
             --verbose

             --config-set <key> <value>

    info [<user>/]<project>, ... {global options}
        global options:
             --config-set <key> <value>

    search <project>, ... {global options}
        global options:
             --config-set <key> <value>

    list

    latest

    config {options}
        options:
             --get (default)
             --set <key> <value>
             --remove <key>
	
### Do it

tell epm where to install packages

	jvorreuter$ epm config --set install_dir /Users/jvorreuter/dev
	epm v0.1.1, 2010

	+ updated .epm config
	
search for an Erlang app  

	jvorreuter$ ./epm search excavator
	epm v0.1.1, 2010

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

install that app that you wanted (its dependencies will be installed too)  

	jvorreuter$ ./epm install excavator
	epm v0.1.1, 2010

	===============================
	Install the following packages?
	===============================
	    + clones-mochiweb-master
	    + JacobVorreuter-mochiweb_server_behavior-master
	    + epm-etap-master
	    + JacobVorreuter-dynamic_compile-master
	    + JacobVorreuter-mochixpath-master
	    + JacobVorreuter-excavator-master

	([y]/n) y

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
	epm v0.1.1, 2010

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
	epm v0.1.1, 2010

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
	  homepage: http://yummymeatwhiz.com
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

what have I done? I must remove that terrible app. Its dependencies can stay though

	jvorreuter$ ./epm remove excavator
	epm v0.1.1, 2010

	===============================
	Remove the following packages?
	===============================
	    + JacobVorreuter-excavator-master

	([y]/n) y

	+ removing package JacobVorreuter-excavator-master from /Users/jvorreuter/dev/excavator-0.3
	