
-ifdef(DEBUG_PRINT).

-define(debug(FMT, ARGS), io:format(FMT, ARGS)).
-define(debug(FMT), io:format(FMT, [])).

-endif.

-define(error(FMT, ARGS), io:format(FMT, ARGS)).
-define(info(FMT, ARGS), io:format(FMT, ARGS)).
-define(error(FMT), io:format(FMT, [])).
-define(info(FMT), io:format(FMT, [])).

-ifndef(DEBUG_PRINT).
-define(debug(FMT, ARGS), ok).
-define(debug(FMT), ok).
-endif.
