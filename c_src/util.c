// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include "jiffy.h"

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

    *bpi = (size_t) bytes;

    return 1;
}

int
consume_timeslice(ErlNifEnv* env, size_t used, size_t limit)
{
#if(ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 4)
    double u = (double) used;
    double l = (double) limit;
    int perc = (int) (100.0 * (u / l));

    if(perc < 1) {
        perc = 1;
    } else if(perc > 100) {
        perc = 100;
    }

    return enif_consume_timeslice(env, perc);
#else
    return 0;
#endif
}
