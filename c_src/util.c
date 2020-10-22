// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include "jiffy.h"
#include <stdio.h>

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

ERL_NIF_TERM
make_ok(jiffy_st* st, ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, st->atom_ok, value);
}

ERL_NIF_TERM
make_error(jiffy_st* st, ErlNifEnv* env, const char* error)
{
    return enif_make_tuple2(env, st->atom_error, make_atom(env, error));
}

ERL_NIF_TERM
make_obj_error(jiffy_st* st, ErlNifEnv* env,
        const char* error, ERL_NIF_TERM obj)
{
    ERL_NIF_TERM reason = enif_make_tuple2(env, make_atom(env, error), obj);
    return enif_make_tuple2(env, st->atom_error, reason);
}

int
get_bytes_per_iter(ErlNifEnv* env, ERL_NIF_TERM val, size_t* bpi)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    const ERL_NIF_TERM* tuple;
    int arity;
    unsigned int bytes;

    if(!enif_get_tuple(env, val, &arity, &tuple)) {
        return 0;
    }

    if(arity != 2) {
        return 0;
    }

    if(enif_compare(tuple[0], st->atom_bytes_per_iter) != 0) {
        return 0;
    }

    if(!enif_get_uint(env, tuple[1], &bytes)) {
        return 0;
    }

    // Calculate the number of bytes per reduction
    *bpi = (size_t) (bytes / DEFAULT_ERLANG_REDUCTION_COUNT);

    return 1;
}

int
get_bytes_per_red(ErlNifEnv* env, ERL_NIF_TERM val, size_t* bpi)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    const ERL_NIF_TERM* tuple;
    int arity;
    unsigned int bytes;

    if(!enif_get_tuple(env, val, &arity, &tuple)) {
        return 0;
    }

    if(arity != 2) {
        return 0;
    }

    if(enif_compare(tuple[0], st->atom_bytes_per_red) != 0) {
        return 0;
    }

    if(!enif_get_uint(env, tuple[1], &bytes)) {
        return 0;
    }

    *bpi = (size_t) bytes;

    return 1;
}

int
get_null_term(ErlNifEnv* env, ERL_NIF_TERM val, ERL_NIF_TERM *null_term)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    const ERL_NIF_TERM* tuple;
    int arity;

    if(!enif_get_tuple(env, val, &arity, &tuple)) {
      return 0;
    }

    if(arity != 2) {
      return 0;
    }

    if(enif_compare(tuple[0], st->atom_null_term) != 0) {
      return 0;
    }

    if(!enif_is_atom(env, tuple[1])) {
      return 0;
    }

    *null_term = tuple[1];

    return 1;
}

int
should_yield(size_t used, size_t bytes_per_red)
{
    return (used / bytes_per_red) >= DEFAULT_ERLANG_REDUCTION_COUNT;
}

void
bump_used_reds(ErlNifEnv* env, size_t used, size_t bytes_per_red)
{
#if CONSUME_TIMESLICE_PRESENT
    size_t reds_used;
    size_t pct_used;

    reds_used = used / bytes_per_red;
    pct_used = 100 * reds_used / DEFAULT_ERLANG_REDUCTION_COUNT;

    if(pct_used > 0) {
        if(pct_used > 100) {
            pct_used = 100;
        }

        enif_consume_timeslice(env, pct_used);
    }
#endif

    (void) env;
    (void) used;
    (void) bytes_per_red;
}
