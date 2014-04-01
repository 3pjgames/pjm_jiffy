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

term_to_json_test_() ->
    [
     ?_assertEqual(<<"533A39F6973FF85C86000001">>, pjm_jiffy:term_to_json({<<83,58,57,246,151,63,248,92,134,0,0,1>>})),
     ?_assertEqual({[{foo, 1}]}, pjm_jiffy:term_to_json([{foo, 1}]))
    ].
