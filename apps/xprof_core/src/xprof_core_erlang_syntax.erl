%%%
%%% @doc Module to parse and format expressions in Erlang syntax
%%%
-module(xprof_core_erlang_syntax).

-behaviour(xprof_core_language).

-export([parse_query/1,
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
parse_query("#" ++ _ = _Query) ->
    {error, not_implemented};
parse_query(Query) ->
    {ok, funlatency, [{mfa, Query}]}.

tokens_query(Str) ->
    case erl_scan:string(Str, {1,1}) of
        {error, {_Loc, Mod, Err}, Loc} ->
            xprof_core_lib:err(Loc, Mod, Err);
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
    case tokens(Str) of
        {mfa, _} = MFA ->
            MFA;
        {clauses, M, F, Tokens} ->
            Clauses = parse(Tokens),
            {clauses, M, F, Clauses}
    end.

tokens(Str) ->
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

parse(Tokens) ->
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
