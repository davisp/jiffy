// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include "erl_nif.h"
#include "jiffy.h"

typedef struct {
    // The Wrapper is a struct intended to be used as a resource to hold a
    // binary that's been validated by jiffy to be a valid JSON value

    ErlNifEnv*      env;    // Process independent env to hold the wrapped binary
    ERL_NIF_TERM    bin;
} Wrapper;

static ERL_NIF_TERM
wrap_new(ErlNifEnv* process_env, ErlNifEnv* process_independent_env, ERL_NIF_TERM binary)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(process_env);

    Wrapper* wrapper_p = enif_alloc_resource(st->res_wrapper, sizeof(Wrapper));
    ERL_NIF_TERM wrapper_term = enif_make_resource(process_env, wrapper_p);
    enif_release_resource(wrapper_p);

    wrapper_p->env = process_independent_env;
    wrapper_p->bin = binary;

    return wrapper_term;
}

ERL_NIF_TERM
wrap_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM binary = argv[0];
    if(!enif_is_binary(env, binary)) {
        return enif_make_badarg(env);
    }

    ErlNifEnv* process_independent_env = enif_alloc_env();
    ERL_NIF_TERM bin_copy = enif_make_copy(process_independent_env, binary);

    return wrap_new(env, process_independent_env, bin_copy);
}

ERL_NIF_TERM
wrap_enif_make_sub_binary(ErlNifEnv* env, ERL_NIF_TERM bin_term, size_t pos, size_t size)
{
    ErlNifEnv* process_independent_env = enif_alloc_env();
    // sub_bin must be created in the same env as the parent binary and then copied
    ERL_NIF_TERM sub_bin = enif_make_sub_binary(env, bin_term, pos, size);
    return wrap_new(env, process_independent_env, enif_make_copy(process_independent_env, sub_bin));
}

int
unwrap(ErlNifEnv* env, ERL_NIF_TERM wrapper_resource, ERL_NIF_TERM* bin_term_p)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);

    Wrapper* wrapper_p = NULL;
    if(!enif_get_resource(env, wrapper_resource, st->res_wrapper, (void**) &wrapper_p)) {
        return 0;
    }

    *bin_term_p = enif_make_copy(env, wrapper_p->bin);
    return 1;
}

void
wrapper_destroy(ErlNifEnv* env, void* obj)
{
    Wrapper* wrapper_p = (Wrapper*) obj;
    enif_free_env(wrapper_p->env);
}

