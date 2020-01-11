
-module(things).
-export([bla/0]).

bla() -> io:fwrite("hello, world\n").
