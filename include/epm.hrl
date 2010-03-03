-define(EXIT(Format, Args), exit(lists:flatten(io_lib:format(Format, Args)))).

-record(repository, {name, owner, description, homepage, followers, pushed, api_module}).
-record(package, {user, name, vsn="master", app_vsn, install_dir, deps=[], args=[], repo=#repository{}}).