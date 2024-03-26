-module(proc_sieve).

-export([generate/1]).
-export([proc_sieve_func/2]).
-export([gen_print/1]).

proc_sieve_func(0, InvalidPid) ->
    receive 
        N -> proc_sieve_func(N, InvalidPid)
    after 1000 ->
        io:format("time out in N=0~n")
    end; 

proc_sieve_func(N, Pid) when is_pid(Pid)-> % 6. all others to get prime
receive
    {done, Return} ->         % 6.2. when done
        Pid ! {done, self()}, % 6.2. send done down and wait
        receive               % 6.3. as received a list 
            ResultsList ->    % 6.3. concatenate it with our number and send up
                Return ! [N] ++ ResultsList
        end;
    Num when Num rem N == 0 ->    % 6.1. as we get non-prine number 
        proc_sieve_func(N, Pid);  % 6.1. just go check other numbers
    Num when Num rem N /= 0 ->    % 6.1. otherwise
        Pid ! Num,                % 6.1. send our prime number to an upper
        proc_sieve_func(N, Pid)   % 6.1. anyway go check other numbers

end;
proc_sieve_func(N, Anything) -> % 2.3. first one to get a prime (anything in pid)
receive
    {done, ReqPid} ->             % 5. as we get done-flag
        ReqPid ! [N];             % 5.1 just send our number as a list
    Num when Num rem N == 0 ->    % 3. as we get non-prine number 
        proc_sieve_func(N, Anything);  % 3. just go check other numbers
    Num when Num rem N /= 0 ->    % 3.1. otherwise start a new process
        Pid = spawn(proc_sieve, proc_sieve_func, [0, anything]),
        Pid ! Num,                % 3.1. and send that prime number to it
        proc_sieve_func(N, Pid)   % 3.2. anyway go check other numbers
after 5000 ->
        io:format("time out N = ~p~n", [N]),
        {fail, timeout}
end.

sieve() -> % 1.2. spawn a new process with function, 0 and no pid
    spawn(proc_sieve, proc_sieve_func, [0, anything]).

generate(MaxN) when MaxN > 1 -> % 1. exception if MaxN < 2
    Pid = sieve(),              % 1.1. start a new process with no parameters
    nextNumber(2, MaxN, Pid).   % 1.3. let's send numbers, starting from 2

nextNumber(Stopword, Stopword, To) -> % 4. when we get to the stopword
    To ! {done, self()},              % 4.1. send a done-flag and initial pid
    receive                           % 4.2. wait for message with result 
        Result -> Result              % 4.3. and do nothing (should be printed)
    end;

nextNumber(N, Stopword, To) ->       % 2. 
    To ! N,                          % 2.1. Send the passed number to process
    nextNumber(N + 1, Stopword, To). % 2.2. Send the next number to process

gen_print(MaxN) ->
    List = generate(MaxN),
    lists:foreach(fun(T) -> 
                         io:format("~w, ", [T]) 
                  end,
                  List).