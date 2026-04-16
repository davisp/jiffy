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

    // Calculate the number of bytes per reduction. Clamp to 1 so we
    // avoid a divide-by-zero in bump_used_reds.
    *bpi = (size_t) (bytes / DEFAULT_ERLANG_REDUCTION_COUNT);
    if(*bpi == 0) {
        *bpi = 1;
    }

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

    // Same get_bytes_per_iter, clamp to 1 to avoid a divide by 0
    *bpi = (size_t) bytes;
    if(*bpi == 0) {
        *bpi = 1;
    }

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
