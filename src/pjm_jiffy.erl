-module(pjm_jiffy).

-export([from_json/2, to_json/1]).
-export([term_to_json/1]).

-type value() :: object() | array() | string() | number() | null() | boolean().
-type key() :: binary() | atom().
-type kv() :: { key() | value() }.
-type object() :: {[kv()]}.
-type array() :: [value()].
-type string() :: binary().
-type number() :: number().
-type boolean() :: true | false.
-type null() :: null.

-export_type([value/0]).
-export_type([key/0, kv/0]).
-export_type([object/0, array/0, string/0, number/0, boolean/0, null/0]).

-spec from_json(object(), module() | pjm:model()) -> pjm:model().
from_json({Object}, Module) when is_atom(Module) ->
    from_json({Object}, Module:new());
from_json({Object}, Model) ->
    pjm:set(Object, Model).

-spec to_json(pjm:model()) -> object().
to_json(Model) ->
    {
      pjm:fold(
        fun to_json_acc/3,
        [],
        Model
       )
    }.

to_json_acc(_K, undefined, List) ->
    List;
to_json_acc(_K, {regexp, _, _}, List) ->
    %% ignore bson regexp
    List;
to_json_acc(_K, {bin, _, _}, List) ->
    %% ignore bson bin types
    List;
to_json_acc(K, Value, List) ->
    [{K, term_to_json(Value)}|List].

term_to_json({pjm, _, _} = Model) ->
    to_json(Model);
term_to_json([]) -> [];
term_to_json({}) -> {[]};
term_to_json([{Key, _Value}|_Rest] = List) when is_atom(Key) orelse is_binary(Key) ->
    {lists:map(fun({K, V}) -> {K, term_to_json(V)} end, List)};
term_to_json(List) when is_list(List) ->
    lists:map(fun term_to_json/1, List);
term_to_json({List}) when is_list(List) ->
    term_to_json(List);
term_to_json({<<_:96>> = Id}) ->
    %% display bson id as hex string
    << << (integer_to_binary(Bits, 16))/binary >> || << Bits:4 >> <= Id >>;
term_to_json(Term) -> Term.
