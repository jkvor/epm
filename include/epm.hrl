-define(EXIT(Format, Args), exit(lists:flatten(io_lib:format(Format, Args)))).

-record(package, {user, name, vsn, args=[]}).
-record(repository, {name, owner, description, homepage, followers, pushed}).