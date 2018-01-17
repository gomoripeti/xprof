%%%
%%% @doc Module to parse and format expressions in Erlang syntax
%%%
-module(xprof_core_erlang_syntax).

-behaviour(xprof_core_language).

-export([parse_query/1,
         parse_incomplete_query/1,
         parse_match_spec/1,
         hidden_function/1,
         fmt_mfa/3,
         fmt_mod_and_delim/1,
         fmt_mod/1,
         fmt_fun_and_arity/2,
         fmt_fun/1,
         fmt_exception/2,
         fmt_term/1]).

%% @doc Parse a query string that represents either an xprof-flavoured
%% match-spec fun or an extended xprof query in Erlang syntax.
%% The `mfa' key has special handling.
%%
%% FIXME: the current logic is the following:
%% - the query should be fully parsable except the value of mfa key
%% which may be parsable if it is in the module-function-arity form,
%% but not in case of an xprof-flavoured match-spec fun
%% - to address this it is mandatory to have the mfa key as the last one,
%% if present
%% - if there is a parsing error, assume that it is because this last mfa value
%% and try to remove the problematic part from the end until the query is
%% parsable (it will be parsable when only mod:fun(args) is left
%% without guards and body)
%% - if this way the query is parsed we need to verify that the parsing error
%% was because of the mfa key and not because of other parsing error
%% - in either way we figure out where the mfa value as string starts in the
%% original query and return the mfa value as string, so that the old parser
%% (`parse_match_spec') can do its job on it.
%%
%% Current logic puts curly-brackets around the params and parses the whole
%% query as a record.
%% Alternatively it would be good to also be able to parse an incomplete
%% query to support autocomplete (suggesting keys and value type hinst).
%% For this it might be better to parse the params as a list of comma separated
%% expression (matching '=' op) one by one.
-spec parse_query(string()) -> {ok, xprof_core:cmd(),
                                [{mfa, string()} |
                                 {atom(), erl_parse:abstract_expr()}]}
                                   | {error, Reason :: any()}.
parse_query("#" ++ _ = Query) ->
    %% extended query
    try
        tokens_query(Query)
    catch
        throw:Error ->
            Error
    end;
parse_query(Query) when is_list(Query) ->
    {ok, funlatency, [{mfa, Query}]}.

-spec parse_incomplete_query(binary()) ->
    {ok, Cmd, Params}
  | {incomplete_cmd, CmdPrefix}
  | {incomplete_key, KeyPrefix, Cmd, ParamsSoFar}
  | {incomplete_value, Key, ValuePrefix, Cmd, ParamsSoFar}
  when
      Cmd :: xprof_core:cmd(),
      CmdPrefix :: atom(), %% binary()/string() ???
      Params :: xprof_core:params(),
      ParamsSoFar :: xprof_core:params(),
      Key :: atom(),
      KeyPrefix :: atom(), %% binary()/string() ???
      ValuePrefix :: string(). %% binary() ???
%% throw:{error, Reason :: term()}
parse_incomplete_query(Query) ->
    {ok, Tokens, Rest} = tokens_query(Query, incomplete),
    case parse_query_tokens(Tokens, cmd, undefined, []) of
        %{ok, Cmd, [{LastKey, LastValue}|Params]} when Rest =/= [] ->
        %    {incomplete_value, LastKey, {LastValue, Rest}, Cmd, Params};
        {ok, Cmd, Params} ->
            {incomplete_key, Rest, Cmd, Params};
        {more, cmd, _, _} ->
            {incomplete_cmd, Rest};
        {more, key, Cmd, Params} ->
            {incomplete_key, Rest, Cmd, Params};
        {more, {eq, Key}, Cmd, Params} ->
            {incomplete_key, {Key, Rest}, Cmd, Params};
        {more, {value, Key, ValueTokens}, Cmd, Params} ->
            {incomplete_value, Key, {ValueTokens, Rest}, Cmd, Params};
        {more, {value, Key}, Cmd, Params} ->
            {incomplete_value, Key, Rest, Cmd, Params};
        {more, comma, Cmd, Params} when Rest =:= [] ->
            {ok, Cmd, Params};
        {more, comma, Cmd, [{LastKey, LastValue}|Params]} ->
            %% Rest does not start with a comma, hence the value is parsable but incomplete
            {incomplete_value, LastKey, {LastValue, Rest}, Cmd, Params};
        {unexpected, _Token, _State} = Un ->
            {error, Un}
    end.

tokens_query(Str, error) ->
    case erl_scan:string(Str, {1, 1}) of
        {error, {_Loc, Mod, Err}, Loc} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok, Tokens, _EndLoc} ->
            {ok, Tokens}
    end;
tokens_query(Str, incomplete) ->
    case erl_scan:tokens([], Str, {1, 1}, [text]) of
        {done, {ok, _Tokens, _EndLoc}, _Rest} ->
            %% unexpected - Str contains a dot in the middle
            %% FIXME extract location - {dot, Loc} = lists:last(Tokens)
            throw({error, unexpected_dot});
        {done, {error, {Loc, Mod, Err}, _EndLoc}, _Rest} ->
            %% FIXME throw or return error
            xprof_core_lib:err(Loc, Mod, Err);
        {more, {erl_scan_continuation, Cs, _Col,
                RevTokens,
                _Line, _St, Any, _Fun}} when is_list(RevTokens) ->
            Rest = case {Cs, Any} of
                       {[], [_|_]} ->
                           %% unterminated atom
                           lists:reverse(Any);
                       {[],{RevCs, _, _StartLine, _StartCol}} ->
                           %% scan_string/scan_qatom
                           %% FIXME: add case for "... 'asd" -> Any = {"dsa","dsa",1,11}
                           %% idea: Str+eof => {done,{error,{_Loc,erl_scan,{string,$',"asd"}},_EndLoc}, eof}
                           lists:reverse(RevCs);
                       {[], {BaseInt, NumRev, _TextBase}} when is_integer(BaseInt), is_list(NumRev) ->
                           %% scan_based_int
                           integer_to_list(BaseInt) ++ "#" ++ lists:reverse(NumRev);
                       {[], Int} when is_integer(Int) ->
                           %% scan whitespace
                           "";
                       {_, Any} when is_list(Cs), (Any =:= [] orelse not is_list(Any)) ->
                           Cs
                   end,
            {ok, lists:reverse(RevTokens), Rest}
    end.

parse_query_tokens([{atom, _, Cmd}|T], cmd, _C, _P) ->
    parse_query_tokens(T, key, Cmd, []);
parse_query_tokens([{atom, _, Key}|T], key, Cmd, Params) ->
    parse_query_tokens(T, {eq, Key}, Cmd, Params);
parse_query_tokens([{'=', _}|T], {eq, Key}, Cmd, Params) ->
    parse_query_tokens(T, {value, Key}, Cmd, Params);
parse_query_tokens([_|_] = T, {value, Key}, Cmd, Params) ->
    case parse_value(T, []) of
        {ok, ValueAst, TRest} ->
            parse_query_tokens(TRest, comma, Cmd, [{Key, ValueAst}|Params]);
        _Error ->
            {more, {value, Key, T}, Cmd, Params}
    end;
parse_query_tokens([{',', _}|T], comma, Cmd, Params) ->
    parse_query_tokens(T, key, Cmd, Params);
parse_query_tokens([], key, Cmd, Params) ->
    {ok, Cmd, lists:reverse(Params)};
parse_query_tokens([], State, Cmd, Params) ->
    {more, State, Cmd, Params};
parse_query_tokens([H|_], State, _, _) ->
    {unexpected, H, State}.


parse_value([_|_] = T, Head) ->
    {TH, TT} = tokens_to_comma(T),
    case {erl_parse:parse_exprs(Head ++ TH ++ [{dot,{2,1}}]), TT} of
        {{error, _} = Error, []} ->
            Error;
        {{error, _} = Error, [{',', _}]} ->
            Error;
        {{error, _}, [{',', _} = Comma|TTT]} ->
            parse_value(TTT, Head ++ TH ++ [Comma]);
        {{ok, AST}, _} ->
            {ok, AST, TT}
    end.


tokens_to_comma(Tokens) ->
    lists:splitwith(
      fun(Token) -> element(1, Token) =/= ',' end,
      Tokens).


tokens_query(Str) ->
    case erl_scan:string(Str, {1,1}) of
        {error, {_Loc, Mod, Err}, Loc} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok,[{atom, _, Cmd}], _EndLoc} ->
            %% shortcut
            {ok, Cmd, []};
        {ok,[{'#', _} = Hash, {atom, _, _} = CmdToken|Rest], _EndLoc} ->
            {AST, Trunc} = parse_reduce(Hash, CmdToken, Rest, _Trunc = false),
            {Cmd, Params} = record_to_opts(AST),
            case Trunc of
                false ->
                    %% all is good, managed to parse the whole query
                    case replace_mfa_with_string(Str, Rest, AST, Params) of
                        false ->
                            {ok, Cmd, Params};
                        NewParams ->
                            {ok, Cmd, NewParams}
                    end;
                true ->
                    %% had to truncate the query
                    %% this is only ok if the last key is `mfa'
                    case replace_mfa_with_string(Str, Rest, AST, Params) of
                        false ->
                            {error, parsing_error};
                        NewParams ->
                            {ok, Cmd, NewParams}
                    end
            end
    end.

parse_reduce(Hash, Cmd, Rest, Trunc) ->
    RecordTokens = braces(Hash, Cmd, Rest),
    case erl_parse:parse_exprs(RecordTokens) of
        {error, {ErrLoc, _Mod, _Err}} ->
            NewRest = case tokens_to(ErrLoc, Rest) of
                          Rest ->
                              %% the error is at the end of the tokenlist
                              %% no token is removed so let's drop the last one and hope for the best
                              droplast(Rest);
                          RestHd ->
                              RestHd
                      end,
            parse_reduce(Hash, Cmd, NewRest, true);
        {ok, AST} ->
            {AST, Trunc}
    end.

braces(Hash, {atom, Loc, _} = Cmd, Rest) ->
    [Hash, Cmd, {'{', Loc}|Rest++[{'}', {2,1}},{dot, {2,2}}]].

record_to_opts([{record, _, Cmd, Fields}]) ->
    Params = [{Key, ValueAst}
              || {record_field, _, {atom, _, Key}, ValueAst} <- Fields],
    {Cmd, Params}.

replace_mfa_with_string(OrigQuery, RestTokens, AST, Params) ->
    case last_field_is_mfa(AST) of
        {true, MFAStart} ->
            [{atom, _, mfa}, {'=', _}, NextElem|_] = tokens_from(MFAStart, RestTokens),
            {_, MfaValueStartColumn} = element(2, NextElem),
            MFAQuery = lists:sublist(OrigQuery, MfaValueStartColumn, length(OrigQuery)),
            lists:keyreplace(mfa, 1, Params, {mfa, MFAQuery});
        false ->
            false
    end.

last_field_is_mfa([{record, _, _, []}]) ->
    false;
last_field_is_mfa([{record, _, _, Fields}]) ->
    case lists:last(Fields) of
        {record_field, Loc, {atom, _, mfa}, _Value} ->
            {true, Loc};
        _ ->
            false
    end.

tokens_to(Loc, Tokens) ->
    lists:takewhile(
      fun(Token) -> element(2, Token) =/= Loc end,
      Tokens).

tokens_from(Loc, Tokens) ->
    lists:dropwhile(
      fun(Token) -> element(2, Token) =/= Loc end,
      Tokens).

%% @doc Parse a query string that represents either a module-function-arity
%% or an xprof-flavoured match-spec fun in Erlang syntax.
%% In the later case the last element of the tuple is the abstract syntax tree
%% of the clauses of the anonimous function.
parse_match_spec(Str) ->
    case tokens_ms(Str) of
        {mfa, _} = MFA ->
            MFA;
        {clauses, M, F, Tokens} ->
            Clauses = parse_ms(Tokens),
            {clauses, M, F, Clauses}
    end.

tokens_ms(Str) ->
    case erl_scan:string(Str, {1,1}) of
        {error, {_Loc, Mod, Err}, Loc} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok, [{atom, _, M}, {':', _},
              {atom, _, F}, {'/', _},
              {integer, _, A}], _EndLoc} ->
            {mfa, {M, F, A}};
        {ok, [{atom, _, M}, {':', _},
              {atom, _, F}|Tokens], _EndLoc} when Tokens =/= [] ->
            {clauses, M, F, [{'fun', 0}|ensure_end(ensure_body(Tokens))]};
        {ok, Tokens, _EndLoc} ->
            xprof_core_lib:err("expression is not an xprof match-spec fun ~w", [Tokens])
    end.

%% @doc Ensure the fun has at least a trivial function body "-> true".
%% Omitting body is only allowed if there is only a single clause.
ensure_body(Tokens) ->
    case lists:keymember('->', 1, Tokens) of
        true ->
            Tokens;
        false ->
            Loc = get_loc(lists:last(Tokens)),
            Tokens ++ [{'->', Loc}, {atom, Loc, true}]
    end.

get_loc({_, Loc}) -> Loc;
get_loc({_, Loc, _}) -> Loc.

%% @doc Ensure the fun is properly closed with "end."
ensure_end(Tokens) ->
    case lists:reverse(Tokens) of
        [{dot, _}, {'end', _}| _] -> Tokens;
        [{dot, Loc}|T] -> lists:reverse(T, [{'end', Loc}, {dot, Loc}]);
        [Last|_] = R ->
            Loc = element(2, Last),
            lists:reverse(R, [{'end', Loc}, {dot, Loc}])
    end.

parse_ms(Tokens) ->
    case erl_parse:parse_exprs(Tokens) of
        {error, {Loc, Mod, Err}} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok, [{'fun', _Loc, {clauses, Clauses}}]} ->
            Clauses;
        {ok, _} ->
            xprof_core_lib:err("expression is not an xprof match-spec fun")
    end.

%%
%% Functions for autocomplete
%%

hidden_function(behaviour_info) -> true;
hidden_function(module_info) -> true;
hidden_function(Fun) ->
    case atom_to_list(Fun) of
        "-" ++ _ ->
            %% filter out local functions generated for fun objects
            %% and list comprehensions like '-filter_funs/2-fun-0-'
            true;
        _ -> false
    end.

fmt_mfa(Mod, Fun, Arity) ->
    fmt("~w:~w/~b", [Mod, Fun, Arity]).

fmt_mod(Mod) ->
    fmt("~w", [Mod]).

fmt_mod_and_delim(Mod) ->
    fmt("~w:", [Mod]).

fmt_fun(Fun) ->
    fmt("~w", [Fun]).

fmt_fun_and_arity(Fun, Arity) ->
    fmt("~w/~b", [Fun, Arity]).

fmt_exception(Class, Reason) ->
    Stacktrace = [],
    SkipFun = fun(_M, _F, _A) -> false end,
    PrettyFun = fun(Term, _Indent) -> io_lib:format("~tp", [Term]) end,
    Encoding = unicode,
    unicode:characters_to_binary(
      ["** "|lib:format_exception(1, Class, Reason, Stacktrace,
                                  SkipFun, PrettyFun, Encoding)]).

fmt_term(Term) ->
    fmt("~tp", [Term]).

fmt(Fmt, Args) ->
    unicode:characters_to_binary(io_lib:format(Fmt, Args)).

%% lists:droplast/1 added in OTP 17.0
droplast([_T])  -> [];
droplast([H|T]) -> [H|droplast(T)].
