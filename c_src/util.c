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

int
get_reductions(ErlNifEnv *env, ERL_NIF_TERM term, jiffy_st* st, size_t* val)
{
    int arity;
    const ERL_NIF_TERM *tuple;

    return enif_get_tuple(env, term, &arity, &tuple) &&
           arity == 2 &&
           enif_compare(tuple[0], st->atom_reductions) == 0 &&
           enif_get_int(env, tuple[1], (int*) val) &&
           val >= 0;
}

int
jiffy_consume_timeslice(ErlNifEnv *env, size_t reds, size_t cur, size_t* proc) {
#if ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 4
#define PERCENTS 10
    if (reds > 0 && cur - *proc >= reds / PERCENTS) {
        int percents = 100 * (cur - *proc) / reds;
        percents = (percents <   1) ? 1 : (
                   (percents > 100) ? 100 :
                    percents );
        *proc = cur;
        return enif_consume_timeslice(env, percents);
    }
#endif
    return 0;
}
