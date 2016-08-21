-module (hunter_utils).

-export ([get_time_delta/0, milliseconds_now/0]).


get_time_delta() ->
    LastTime = ets:first(last_time),
    MillisecondsNow = hunter_utils:milliseconds_now(),
    ets:delete_all_objects(last_time),
    ets:insert(last_time, {MillisecondsNow}),

    (MillisecondsNow - LastTime) / 1000.

milliseconds_now() ->
    {Mg, S, Mc} = erlang:timestamp(),
    (Mg * 1000000000) + (S * 1000) + (Mc div 1000).