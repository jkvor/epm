-define(EXIT(Format, Args), exit(lists:flatten(io_lib:format(Format, Args)))).

-record(project, {user, name, vsn}).
-record(repository, {name, owner, description, homepage, followers, pushed}).