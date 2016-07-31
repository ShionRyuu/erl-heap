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
-module(sized_heap).
-author("Ryuu").

%% API
-export([
    new/0,
    new/1,
    new/2,
    new/3,
    cap/1,
    size/1,
    is_empty/1,
    is_full/1,
    is_in/2,
    insert/3,
    delete/2,
    to_list/1,
    sort/1,
    top/1,
    pop/1
]).

-compile({no_auto_import, [size/1]}).

-define(heap, #{
    cap => infinity,           %% capacity
    mode => min,               %% min/max
    min_heap => heap:new(min), %% min heap
    max_heap => heap:new(max)  %% max heap
}).

-define(VALID_MOD(M), (M =:= min orelse M =:= max)).
-define(VALID_CAP(C), (is_integer(C) orelse C =:= infinity)).
-define(VALID_CMP(C), (is_function(C, 2) orelse is_tuple(C))).

%% @doc new heap
new() ->
    ?heap.

new(M) when ?VALID_MOD(M) ->
    ?heap#{mode => M}.

new(M, C) when ?VALID_MOD(M) andalso ?VALID_CAP(C) ->
    ?heap#{mode => M, cap => C}.

new(M, Cap, Cmp) when ?VALID_MOD(M) andalso ?VALID_CAP(Cap) andalso ?VALID_CMP(Cmp) ->
    ?heap#{mode => M, cap => Cap,
        min_heap => heap:new(min, Cmp),
        max_heap => heap:new(max, Cmp)}.

%% @doc heap size
cap(H) ->
    #{cap := C} = H,
    C.

%% @doc heap size
size(H) ->
    #{min_heap := IH} = H,
    heap:size(IH).

%% @doc is heap empty
is_empty(H) ->
    size(H) =:= 0.

%% @doc is heap empty
is_full(H) ->
    size(H) >= cap(H).

%% @doc is in heap
is_in(K, H) ->
    #{min_heap := IH} = H,
    heap:is_in(K, IH).

%% @doc insert or update element
insert(H, K, V) ->
    #{mode := M} = H,
    #{min_heap := IH, max_heap := XH} = H,
    case (not is_full(H)) orelse
        heap:is_in(K, IH) of
        true ->
            H#{min_heap => heap:insert(IH, K, V),
                max_heap := heap:insert(XH, K, V)};
        _ when M =:= min -> %% min
            {Kx, Vx} = heap:top(XH),
            case heap:cmp(XH, V, Vx) of
                -1 ->
                    H#{min_heap => heap:insert(heap:delete(IH, Kx), K, V),
                        max_heap := heap:insert(heap:delete(XH, Kx), K, V)};
                _ -> H
            end;
        _ -> %% max
            {Ki, Vi} = heap:top(IH),
            case heap:cmp(XH, V, Vi) of
                1 ->
                    H#{min_heap => heap:insert(heap:delete(IH, Ki), K, V),
                        max_heap := heap:insert(heap:delete(XH, Ki), K, V)};
                _ -> H
            end
    end.

%% @doc delete element
delete(H, K) ->
    #{min_heap := IH, max_heap := XH} = H,
    H#{min_heap => heap:delete(IH, K), max_heap := heap:delete(XH, K)}.

%% @doc
to_list(H) ->
    #{mode := M, min_heap := IH, max_heap := XH} = H,
    case M of
        min -> heap:to_list(IH);
        _ -> heap:to_list(XH)
    end.

%% @doc sort heap elements
sort(H) ->
    #{mode := M, min_heap := IH, max_heap := XH} = H,
    case M of
        min -> heap:sort(IH);
        _ -> heap:sort(XH)
    end.

%% @doc top element
top(H) ->
    #{mode := M, min_heap := IH, max_heap := XH} = H,
    case M of
        min -> heap:top(IH);
        _ -> heap:top(XH)
    end.

%% @doc pop top element
pop(H) ->
    case is_empty(H) of
        true -> H;
        _ -> pop2(H)
    end.

pop2(H) ->
    #{mode := M, min_heap := IH, max_heap := XH} = H,
    case M of
        min ->
            {K, _} = heap:top(IH),
            H#{min_heap => heap:pop(IH), max_heap => heap:delete(XH, K)};
        _ ->
            {K, _} = heap:top(XH),
            H#{min_heap => heap:delete(IH, K), max_heap => heap:pop(XH)}
    end.

%% -----------------------------------------------------------------------------
%% Test
%% -----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

heap_cap_size_test() ->
    Cap = 3,
    H = sized_heap:new(min, Cap),
    ?assert(sized_heap:is_empty(H)),
    H1 = sized_heap:insert(H, 1, 5),
    ?assertEqual(sized_heap:size(H1), 1),
    H2 = sized_heap:insert(H1, 2, 2),
    ?assertEqual(sized_heap:size(H2), 2),
    H3 = sized_heap:insert(H2, 5, 11),
    ?assertEqual(sized_heap:size(H3), 3),
    ?assert(sized_heap:is_full(H3)),
    H4 = sized_heap:insert(H3, 6, 4),
    ?assertEqual(sized_heap:size(H4), 3),
    ?assert(sized_heap:is_full(H4)),
    ok.

heap_pop_top_test() ->
    Cap = 3,
    H = sized_heap:new(min, Cap),
    ?assertEqual(sized_heap:pop(H), H),
    ?assertEqual(sized_heap:top(H), undefined),
    H1 = sized_heap:insert(H, 1, 5),
    ?assertEqual(sized_heap:top(H1), {1, 5}),
    H2 = sized_heap:pop(H1),
    ?assertEqual(sized_heap:top(H2), undefined),
    ?assertEqual(sized_heap:size(H2), 0),
    ?assert(sized_heap:is_empty(H)),
    ok.

heap_sort_test() ->
    Cap = 3,
    H = sized_heap:new(min, Cap),
    H1 = sized_heap:insert(H, "2", {4, 4}),
    ?assertEqual(sized_heap:sort(H1), [{"2", {4, 4}}]),
    H2 = sized_heap:insert(H1, "1", {3, 3}),
    ?assertEqual(sized_heap:sort(H2), [{"1", {3, 3}}, {"2", {4, 4}}]),
    H3 = sized_heap:insert(H2, "1", {2, 2}),
    ?assertEqual(sized_heap:sort(H3), [{"1", {2, 2}}, {"2", {4, 4}}]),
    H4 = sized_heap:insert(H3, "3", {5, 5}),
    ?assertEqual(sized_heap:sort(H4), [{"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}]),
    H5 = sized_heap:insert(H4, "5", {6, 6}),
    ?assertEqual(sized_heap:sort(H5), [{"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}]),
    H6 = sized_heap:insert(H5, "6", {7, 7}),
    ?assertEqual(sized_heap:sort(H6), [{"1", {2, 2}}, {"2", {4, 4}}, {"3", {5, 5}}]),
    H7 = sized_heap:insert(H6, "8", {1, 1}),
    ?assertEqual(sized_heap:sort(H7), [{"8", {1, 1}}, {"1", {2, 2}}, {"2", {4, 4}}]),
    ?assertEqual(sized_heap:size(H7), Cap).

heap_insert_delete_test() ->
    Cap = 3,
    H = sized_heap:new(min, Cap),
    H1 = sized_heap:insert(H, 1, 5),
    %% update
    H2 = sized_heap:insert(H, 1, 15), 
    ?assertEqual(sized_heap:size(H2), 1),
    H3 = sized_heap:delete(H2, 12),
    ?assertEqual(H2, H3),
    H4 = sized_heap:delete(H1, 1),
    ?assert(sized_heap:is_empty(H4)),
    ?assertNot(sized_heap:is_in(1, H4)),
    H5 = sized_heap:delete(H4, 1),
    ?assertEqual(H4, H5),
    ?assert(sized_heap:is_empty(H5)).

heap_mode_test() ->
    Cap = 3,

    H0 = sized_heap:new(min, Cap),
    H1 = sized_heap:insert(H0, 2, 2),
    H2 = sized_heap:insert(H1, 1, 1),
    ?assertEqual(sized_heap:top(H2), {1, 1}),
    H3 = sized_heap:insert(H2, 3, 3),
    ?assertEqual(sized_heap:top(H3), {1, 1}),

    H10 = sized_heap:new(max, Cap),
    H11 = sized_heap:insert(H10, 2, 2),
    H12 = sized_heap:insert(H11, 1, 1),
    ?assertEqual(sized_heap:top(H12), {2, 2}),
    H13 = sized_heap:insert(H12, 3, 3),
    ?assertEqual(sized_heap:top(H13), {3, 3}),
    ok.

-endif.