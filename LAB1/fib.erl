-module(fib).
-export([fib_p/1, fib_g/1, tail_fib/1]).
%Calculating fibonacci numbers using argument comparison with pattern
fib_p(N) ->
if
    N == 0 -> 0;
    N == 1 -> 1;
    N >= 2 -> fib_p(N-1) + fib_p(N-2)
end.

%Calculating fibonacci numbers using guards
fib_g(N) when N == 0 -> 0;
fib_g(N) when N == 1 -> 1;
fib_g(N) when N >=2 -> fib_g(N-1) + fib_g(N-2).

%Calculating fibonacci numbers using tail recursion
tail_fib(N) -> tail_fib_helper(N, 0, 1).
tail_fib_helper(0, Res, _) -> Res;
tail_fib_helper(1, _, Res) -> Res;
tail_fib_helper(N, Prev, Res) when N >= 2 -> tail_fib_helper(N-1, Res, Res+Prev).