-module(pjm_jiffy_tests).
-compile([{parse_transform, pjm_parse_trans}]).

-include("./test_helpers.hrl").

-pjm([{stores_in, users},
      {fields, [
                {login, binary, <<"test">>},
                {embed, pjm_jiffy_tests}
               ]}]).

from_json_test() ->
    M = pjm_jiffy:from_json(
          {[{login, <<"ian">>}, {age, 30}]},
          ?MODULE
         ),
    ?assertEqual([<<"ian">>, 30], get([login, age], M)).

from_json_nest_test() ->
    M = pjm_jiffy:from_json(
          {[{login, <<"ian">>}, {age, 30}, {embed, {[{login, "yincan"}]}}]},
          ?MODULE
         ),
    ?assertEqual(<<"yincan">>, get(login, get(embed, M))).

to_json_nest_test() ->
    M = pjm_jiffy:from_json(
          {[{login, <<"ian">>}, {age, 30}, {embed, {[{login, "yincan"}]}}]},
          ?MODULE
         ),
    ?assertEqual({[{age, 30}, {embed, {[{login, <<"yincan">>}]}}, {login, <<"ian">>}]}, pjm_jiffy:to_json(M)).
