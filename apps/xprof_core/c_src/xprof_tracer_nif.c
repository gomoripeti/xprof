#include "erl_nif.h"

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};

ERL_NIF_INIT(xprof_tracer_nif, nif_funcs, load, NULL, upgrade, unload)

static ERL_NIF_TERM atom_call;
static ERL_NIF_TERM atom_return_from;
static ERL_NIF_TERM atom_trace_status;
static ERL_NIF_TERM atom_trace;
static ERL_NIF_TERM atom_remove;
static ERL_NIF_TERM atom_discard;
static ERL_NIF_TERM atom_match_spec_result;
static ERL_NIF_TERM atom_arity;
static ERL_NIF_TERM atom_extra;
static ERL_NIF_TERM atom_trace_ts;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  atom_call = enif_make_atom(env, "call");
  atom_return_from = enif_make_atom(env, "return_from");
  atom_trace_status = enif_make_atom(env, "trace_status");
  atom_trace = enif_make_atom(env, "trace");
  atom_remove = enif_make_atom(env, "remove");
  atom_discard = enif_make_atom(env, "discard");
  atom_match_spec_result = enif_make_atom(env, "match_spec_result");
  atom_arity = enif_make_atom(env, "arity");
  atom_extra = enif_make_atom(env, "extra");
  atom_trace_ts = enif_make_atom(env, "trace_ts");
  atom_undefined = enif_make_atom(env, "undefined");
  atom_ok = enif_make_atom(env, "ok");

  *priv_data = NULL;
  return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL || *priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
	return -1;
    }
    return 0;
}

/*
 * argv[0]: TraceTag
 * argv[1]: TracerState
 * argv[2]: Tracee
 */
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid to_pid;
    ERL_NIF_TERM dont_trace = enif_is_identical(argv[0], atom_trace_status) ?
        atom_remove : atom_discard;

    if (enif_get_local_pid(env, argv[1], &to_pid))
        if (!enif_is_process_alive(env, &to_pid))
          /* tracer is dead so we should remove this tracepoint */
          return dont_trace;

    /* Only generate trace for when tracer != tracee */
    if (enif_is_identical(argv[1], argv[2]))
        return dont_trace;

    /* Only trigger trace messages on 'call' and 'return_from  */
    if (enif_is_identical(atom_call, argv[0]) ||
        enif_is_identical(atom_return_from, argv[0])
        )
       return atom_trace;

    /* Have to answer trace_status */
    if (enif_is_identical(atom_trace_status, argv[0]))
        return atom_trace;

    return dont_trace;
}

/*
 * argv[0]: TraceTag, should only be 'call' or 'return_from'
 * argv[1]: TracerState, process to send trace msg to
 * argv[2]: Tracee
 * argv[3]: call: {M,F,Args} return_from: {M,F,Arity}
 * argv[4]: Options in case of return_from  #{extra => Result}
 */
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifPid to_pid;

  if (enif_get_local_pid(env, argv[1], &to_pid)) {

      /* generate timestamp
       * for now use deprecated enif_now_time
       * but later switch to cpu time or monotonic time
       * see erts/emulator/common/nif/erl_tracer_nif.c
       */

      ERL_NIF_TERM ts;
      ts = enif_now_time(env);


      ERL_NIF_TERM msg, arity_opt;

      if (enif_is_identical(atom_call, argv[0])) {

        /* msg: {trace_ts, TraceePid, call, {M,F,Arity}, MSRes, StartTime}
         * where MSRes == match_spec_result which can be 'arity' or Args
         */

        ERL_NIF_TERM msres;
        enif_get_map_value(env, argv[4],
                           atom_match_spec_result,
                           &msres);

        msg =
          enif_make_tuple6(env,
                           atom_trace_ts,
                           argv[2],
                           argv[0],
                           argv[3],
                           msres,
                           ts);

      } else if (enif_is_identical(atom_return_from, argv[0])) {

        /* msg: {trace_ts, TraceePid, return_from, {M,F,Arity}, Result, EndTime}
         * where Result is 'undefined' if match_spec_result == 'arity'
         * and the original return value of the function otherwise
         */

        ERL_NIF_TERM result;
        if (enif_get_map_value(env, argv[4],
                               atom_match_spec_result,
                               &arity_opt)
            && enif_is_identical(atom_arity, arity_opt)) {
          result = atom_undefined;
        } else {
          enif_get_map_value(env, argv[4],
                             atom_extra,
                             &result);
        }

        msg =
          enif_make_tuple6(env,
                           atom_trace_ts,
                           argv[2],
                           argv[0],
                           argv[3],
                           result,
                           ts);

      } else {
        return atom_ok;
      }

      enif_send(env, &to_pid, NULL, msg);
    }

    return atom_ok;
}
