#include "jiffy.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    jiffy_st* st = enif_alloc(sizeof(jiffy_st));
    if(st == NULL) {
        return 1;
    }
    
    st->atom_ok = make_atom(env, "ok");
    st->atom_error = make_atom(env, "error");
    st->atom_null = make_atom(env, "null");
    st->atom_true = make_atom(env, "true");
    st->atom_false = make_atom(env, "false");
    st->atom_bignum = make_atom(env, "bignum");

    // Markers used in encoding
    st->ref_object = make_atom(env, "$object_ref$");
    st->ref_array = make_atom(env, "$array_ref$");

    *priv = (void*) st;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    *priv = *old_priv;
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ErlNifFunc funcs[] =
{
    {"nif_decode", 1, decode},
    {"nif_encode", 1, encode}
};

ERL_NIF_INIT(jiffy, funcs, &load, &reload, &upgrade, &unload);
