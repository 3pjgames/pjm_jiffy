-module(pjm_jiffy_tests).
-compile([{parse_transform, pjm_parse_trans}]).

-include("./test_helpers.hrl").

-pjm_stores_in(users).
-pjm_fields([
             {login, binary, <<"test">>},
             {embed, pjm_jiffy_tests}
            ]).

from_json_test() ->
    M = pjm_jiffy:from_json(
          {[{login, <<"ian">>}, {age, 30}]},
          ?MODULE
         ),
    ?assertEqual([<<"ian">>, 30], get([login, age], M)).
