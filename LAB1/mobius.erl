-module(mobius).
-export([is_prime/1, prime_factors/1, find_square_multiples/2]).
%mobius
is_prime(1) -> false; % 1(?????? ??????)
is_prime(N) when N >= 2 -> is_prime(N, 2). % ???? >= 2, ????????? ????? ????????

% ??????????????? ???????, ??????????? ????? ???????? ????? N ? ????????? [2, sqrt(N)]
is_prime(N, Divisor) when Divisor * Divisor > N -> true; is_prime(N, Divisor) -> 
    if
        N rem Divisor == 0 -> false; %% ???? ????? ??????? ?? ????????, ??? ?? ???????? ???????
        true -> is_prime(N, Divisor + 1) %??????????? Divisor ? ?????????? ?????
    end.

% ???????, ??????????? ?????? ??????? ????????? ????? N
prime_factors(N) -> prime_factors(N, 2, []).

% ??????????????? ???????, ????? ??????? ????????? ????? N ? ????????? [2, N]
prime_factors(N, Divisor, Result) when Divisor > N -> Result;
prime_factors(N, Divisor, Result) ->
    case is_prime(Divisor) of
        true -> case is_prime(N) of
                    true -> [N | Result]; %???? N ???????, ?? ????????? ??? ? ??????
                    false ->
                        if 
                            N rem Divisor == 0 -> %???? N ??????? ?? Divisor
                                prime_factors(N div Divisor, Divisor, [Divisor | Result]); % ????????? Divisor ? ?????? ? ?????????? ??????????? ?????
                            true -> prime_factors(N, Divisor + 1, Result) %? ????????? ?????? ??????????? Divisor ? ?????????? ????? 
                        end 
                end;
        false -> prime_factors(N, Divisor + 1, Result) 
    end.

% ???????, ????????????, ???????? ?? ???????? N ??????? ??????? ?????? ? ????????
is_square_multiple(N) -> 
    SortedPrimeFactors = lists:sort(prime_factors(N)), %% ????????????? ?????? ??????? ?????????? N
    length(SortedPrimeFactors) > sets:size(sets:from_list(SortedPrimeFactors)). 

% Вспомогательная функция Current - текущее число последовательности,
% First - первое число искомой последовательности, SeqLen - ее длина.
find_square_multiples(Count, MaxN) -> find_square_multiples(Count, MaxN, 2, 2, 0).

find_square_multiples(Count, _, _, First, SeqLen) when SeqLen == Count -> First;
find_square_multiples(_, MaxN, Current, _, _) when Current > MaxN -> fail; % Si el numero actual excede MaxN (y SeqLen != Count), falla
find_square_multiples(Count, MaxN, Current, First, SeqLen) ->
    case is_square_multiple(Current) of
        true -> find_square_multiples(Count, MaxN, Current+1, First, SeqLen+1);
        false -> find_square_multiples(Count, MaxN, Current+1, Current+1, 0) %????????????? ????? ? ????? ??????????????????
    end.
