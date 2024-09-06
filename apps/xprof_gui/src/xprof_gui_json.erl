-module(xprof_gui_json).

-export([encode/1]).

-ifndef(XPROF_JSON_LIB).
-ifdef(OTP_RELEASE).
-if(OTP_RELEASE >= 27).
%% OTP 27+
-define(XPROF_JSON_LIB, json).
-else.
%% OTP 21-26
-define(XPROF_JSON_LIB, jsone).
-endif.
-else.
%% OTP 18-20
-define(XPROF_JSON_LIB, jsone).
-endif.
-endif.

-ifndef(XPROF_JSON_ENC_FUN).
-define(XPROF_JSON_ENC_FUN, encode).
-endif.

-spec encode(term()) -> binary().
encode(Data) ->
    iolist_to_binary(?XPROF_JSON_LIB:?XPROF_JSON_ENC_FUN(Data)).
