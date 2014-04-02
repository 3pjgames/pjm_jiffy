-module(pjm_jiffy_custom_to_json_tests).
-compile([{parse_transform, pjm_parse_trans}]).

-include("./test_helpers.hrl").

-export([to_json/1]).

-pjm([{stores_in, users},
      {to_json, to_json},
      {fields, [
                {login, binary, <<"test">>},
                {age, integer}
               ]}]).

to_json(Model) ->
    { fold(
        fun(login, V, Acc) ->
                [{login, pjm_jiffy:term_to_json(V)}|Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        Model
       ) }.

to_json_test() ->
    M = pjm_jiffy:from_json(
          {[{login, <<"ian">>}, {age, 30}]},
          ?MODULE
         ),
    ?assertEqual({[{login, <<"ian">>}]}, pjm_jiffy:to_json(M)).
