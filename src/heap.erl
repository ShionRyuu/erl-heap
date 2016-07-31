%% -------------------------------------------------------------------
%% erl-heap: heap implementation in Erlang
%%
%% Copyright (c) 2016 Shion Ryuu (shionryuu@outlook.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(heap).
-author("Ryuu").

%% API
-export([
    new/0,
    new/1,
    new/2,
    size/1,
    is_empty/1,
    is_in/2,
    insert/3,
    delete/2,
    to_list/1,
    sort/1,
    top/1,
    pop/1,
    cmp/3
]).

-compile({no_auto_import, [size/1]}).

-define(heap, #{
    size => 0,            %% size
    mode => min,          %% min/max
    cmp => fun def_cmp/2, %% comp fun
    heap => dict:new(),   %% index/v
    keys => dict:new(),   %% key/index
    index => dict:new()   %% index/key
}).

-define(VALID_MOD(M), (M =:= min orelse M =:= max)).
-define(VALID_CMP(C), (is_function(C, 2) orelse is_tuple(C))).

%% @doc new heap
new() ->
    ?heap.

new(M) ->
    new(M, fun def_cmp/2).

new(M, C) when ?VALID_MOD(M) andalso ?VALID_CMP(C) ->
    ?heap#{mode => M, cmp => C}.

%% @doc heap size
size(#{size := S}) ->
    S.

%% @doc is heap empty
is_empty(H) ->
    size(H) =:= 0.

%% @doc is in heap
is_in(K, H) ->
    #{keys := Ks} = H,
    dict:is_key(K, Ks).

%% @doc insert or update element
insert(H, K, V) ->
    case is_in(K, H) of
        true -> update_(H, K, V);
        _ -> insert_(H, K, V)
    end.

update_(H, K, V) ->
    #{heap := L, keys := Ks, cmp := C, mode := M} = H,
    I = dict:fetch(K, Ks),
    V0 = dict:fetch(I, L),
    case cmp(C, V, V0) of
        0 ->
            H#{heap => dict:store(I, V, L)};
        R when (M =:= min andalso R =:= -1) orelse
            (M =:= max andalso R =:= 1) ->
            shift_up(H, I, K, V);
        _ ->
            shift_down(H, I, K, V)
    end.

insert_(H, K, V) ->
    #{size := S} = H,
    shift_up(H#{size => S + 1}, S + 1, K, V).

%% @doc delete element
delete(H, K) ->
    #{size := S, heap := L, keys := Ks, index := Ix} = H,
    case is_in(K, H) of
        true when S =:= 1 ->
            H#{size => 0, heap => dict:new(),
                keys => dict:new(), rank => dict:new()};
        true when S > 1 ->
            I = dict:fetch(K, Ks),
            Il = S,
            Vl = dict:fetch(Il, L),
            Kl = dict:fetch(Il, Ix),
            NH = store_elem(erase_elem(H#{size => S - 1}, Il, K), I, Kl, Vl),
            shift_down(NH, I, Kl, Vl);
        _ ->
            H
    end.

%% @doc top element
top(#{size := S}) when S =< 0 ->
    undefined;
top(H) ->
    #{heap := L, index := Ix} = H,
    {dict:fetch(1, Ix), dict:fetch(1, L)}.

%% @doc pop top element
pop(#{size := S} = H) when S =< 0 ->
    H;
pop(H) ->
    #{index := Ix} = H,
    delete(H, dict:fetch(1, Ix)).

%% @doc shift up
shift_up(H, I, K, V) when I =:= 1 ->
    store_elem(H, I, K, V);
shift_up(H, I, K, V) ->
    #{heap := L, index := Ix, cmp := C, mode := M} = H,
    Ip = parent(I),
    Vp = dict:fetch(Ip, L),
    Kp = dict:fetch(Ip, Ix),
    case cmp(C, V, Vp) of
        R when (M =:= min andalso R =:= -1) orelse
            (M =:= max andalso R =:= 1) ->
            shift_up(store_elem(H, I, Kp, Vp), Ip, K, V);
        _ ->
            store_elem(H, I, K, V)
    end.

%% @doc shift down
shift_down(H, I, K, V) ->
    Il = down_index(I, left(I), H),
    Ir = down_index(Il, right(I), H),
    #{heap := L, index := Ix} = H,
    case Ir =:= I of
        true ->
            shift_up(H, I, K, V);
            % store_elem(H, I, K, V);
        _ ->
            Vr = dict:fetch(Ir, L),
            Kr = dict:fetch(Ir, Ix),
            shift_down(store_elem(store_elem(H, I, Kr, Vr), Ir, K, V), Ir, K, V)
    end.

down_index(Ia, Ib, #{size := S}) when Ib > S ->
    Ia;
down_index(Ia, Ib, H) ->
    #{heap := L, cmp := C, mode := M} = H,
    Va = dict:fetch(Ia, L),
    Vb = dict:fetch(Ib, L),
    case cmp(C, Va, Vb) of
        R when (M =:= min andalso R =:= -1) orelse
            (M =:= max andalso R =:= 1) -> Ia;
        _ -> Ib
    end.

store_elem(H, I, K, V) ->
    #{heap := L, keys := Ks, index := Ix} = H,
    H#{heap => dict:store(I, V, L), keys => dict:store(K, I, Ks),
        index => dict:store(I, K, Ix)}.

erase_elem(H, I, K) ->
    #{heap := L, keys := Ks, index := Ix} = H,
    H#{heap => dict:erase(I, L), keys => dict:erase(K, Ks),
        index => dict:erase(I, Ix)}.

%% @doc compare elements
cmp(H, A, B) when is_map(H) ->
    #{cmp := C} = H,
    cmp(C, A, B);
cmp(Fun, A, B) when is_function(Fun, 2) ->
    Fun(A, B);
cmp({M, F}, A, B) when is_atom(M), is_atom(F) ->
    M:F(A, B).

%% @doc
to_list(H) ->
    to_list(H, 1, []).

to_list(#{size := S}, I, Acc) when I > S ->
    lists:reverse(Acc);
to_list(H, I, Acc) ->
    #{heap := L} = H,
    to_list(H, I + 1, [dict:fetch(I, L) | Acc]).

%% @doc sort heap elements
sort(H) ->
    sort(H, []).

sort(#{size := S}, Acc) when S =:= 0 ->
    lists:reverse(Acc);
sort(H, Acc) ->
    {K, V} = top(H),
    sort(pop(H), [{K, V} | Acc]).

%% @doc left index
left(I) ->
    2 * I.

%% @doc right index
right(I) ->
    2 * I + 1.

%% @doc parent index
parent(I) ->
    I div 2.

%% @doc default compare function
def_cmp(A, B) when A =:= B ->
    0;
def_cmp(A, B) when A > B ->
    1;
def_cmp(A, B) when A < B ->
    -1.

%% -----------------------------------------------------------------------------
%% Test
%% -----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

heap_size_test() ->
    H = heap:new(),
    H1 = heap:insert(H, "2", {4, 4}),
    H2 = heap:insert(H1, "1", {3, 3}),
    H3 = heap:insert(H2, "1", {2, 2}),
    H4 = heap:insert(H3, "3", {5, 5}),
    H5 = heap:insert(H4, "5", {6, 6}),
    H6 = heap:insert(H5, "6", {7, 7}),
    ?assertEqual(heap:size(H6), 5),
    H7 = heap:insert(H6, "8", {1, 1}),
    ?assertEqual(heap:size(H7), 6).

heap_pop_top_test() ->
    H = heap:new(min),
    ?assertEqual(heap:pop(H), H),
    ?assertEqual(heap:top(H), undefined),
    H1 = heap:insert(H, 1, 5),
    ?assertEqual(heap:top(H1), {1, 5}),
    H2 = heap:pop(H1),
    ?assertEqual(heap:top(H2), undefined),
    ?assertEqual(heap:size(H2), 0),
    ?assert(heap:is_empty(H)),
    ok.

heap_sort_test() ->
    H = heap:new(min),
    H1 = heap:insert(H, "2", {4, 4}),
    ?assertEqual(heap:sort(H1), [{"2", {4, 4}}]),
    H2 = heap:insert(H1, "1", {3, 3}),
    ?assertEqual(heap:sort(H2), [{"1", {3, 3}}, {"2", {4, 4}}]),
    H3 = heap:insert(H2, "1", {2, 2}),
    ?assertEqual(heap:sort(H3), [{"1", {2, 2}}, {"2", {4, 4}}]),
    H4 = heap:insert(H3, "3", {5, 5}),
    ?assertEqual(heap:sort(H4), [{"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}]),
    H5 = heap:insert(H4, "5", {6, 6}),
    ?assertEqual(heap:sort(H5), [{"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}, {"5", {6, 6}}]),
    H6 = heap:insert(H5, "6", {7, 7}),
    ?assertEqual(heap:sort(H6), [{"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}, {"5", {6, 6}}, {"6", {7, 7}}]),
    H7 = heap:insert(H6, "8", {1, 1}),
    ?assertEqual(heap:sort(H7), [{"8", {1, 1}}, {"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}, {"5", {6, 6}}, {"6", {7, 7}}]).

heap_insert_delete_test() ->
    H = heap:new(min),
    H1 = heap:insert(H, 1, 5),
    %% update
    H2 = heap:insert(H, 1, 15), 
    ?assertEqual(heap:size(H2), 1),
    H3 = heap:delete(H2, 12),
    ?assertEqual(H2, H3),
    H4 = heap:delete(H1, 1),
    ?assert(heap:is_empty(H4)),
    ?assertNot(heap:is_in(1, H4)),
    H5 = heap:delete(H4, 1),
    ?assertEqual(H4, H5),
    ?assert(heap:is_empty(H5)).

heap_mode_test() ->
    H0 = heap:new(min),
    H1 = heap:insert(H0, 2, 2),
    H2 = heap:insert(H1, 1, 1),
    ?assertEqual(heap:top(H2), {1, 1}),
    H3 = heap:insert(H2, 3, 3),
    ?assertEqual(heap:top(H3), {1, 1}),

    H10 = heap:new(max),
    H11 = heap:insert(H10, 2, 2),
    H12 = heap:insert(H11, 1, 1),
    ?assertEqual(heap:top(H12), {2, 2}),
    H13 = heap:insert(H12, 3, 3),
    ?assertEqual(heap:top(H13), {3, 3}),
    ok.

-endif.