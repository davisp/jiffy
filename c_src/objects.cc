// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include <set>
#include <string>

#include <assert.h>

#include "erl_nif.h"

#define MAP_TYPE_PRESENT \
    ((ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 6) \
    || (ERL_NIF_MAJOR_VERSION > 2))

#define BEGIN_C extern "C" {
#define END_C }

BEGIN_C

#include "jiffy.h"

#if (ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION < 17)

/* enif_make_new_atom was introduce in OTP-26 */

static int enif_make_new_atom_len(ErlNifEnv *env, const char *name, size_t len,
                                  ERL_NIF_TERM *atom, ErlNifCharEncoding encoding)
{
    *atom = enif_make_atom_len(env, name, len);
    return !enif_is_exception(env, *atom);
}

#endif

static int
make_key(ErlNifEnv* env, ERL_NIF_TERM key, ERL_NIF_TERM *deckey, js_labels labels) {
    ErlNifBinary bin;

    switch (labels) {
    case jsl_binary:
        *deckey = key;
        return 1;

    case jsl_atom:
        if (!enif_inspect_binary(env, key, &bin))
            return 0;
        if (enif_make_existing_atom_len(env, (char *)bin.data, bin.size, deckey, ERL_NIF_UTF8)) {
            return 1;
        }
        if (enif_make_new_atom_len(env, (char *)bin.data, bin.size, deckey, ERL_NIF_UTF8)) {
            return 1;
        }
        return 0;

    case jsl_existing_atom:
        if (!enif_inspect_binary(env, key, &bin))
            return 0;
        return enif_make_existing_atom_len(env, (char *)bin.data, bin.size, deckey, ERL_NIF_UTF8);

    case jsl_attempt_atom:
        if (!enif_inspect_binary(env, key, &bin))
            return 0;
        if (!enif_make_existing_atom_len(env, (char *)bin.data, bin.size, deckey, ERL_NIF_UTF8))
            *deckey = key;
        return 1;
    }

    return 0;
}

int
make_object(ErlNifEnv* env, ERL_NIF_TERM pairs, ERL_NIF_TERM* out,
        int ret_map, int dedupe_keys, js_labels labels)
{
    ERL_NIF_TERM ret;
    ERL_NIF_TERM key, deckey;
    ERL_NIF_TERM val;

    std::set<std::string> seen;

#if MAP_TYPE_PRESENT

    ERL_NIF_TERM old_val;

    if(ret_map) {
        ret = enif_make_new_map(env);
        while(enif_get_list_cell(env, pairs, &val, &pairs)) {
            if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
                assert(0 == 1 && "Unbalanced object pairs.");
            }
	    if (!make_key(env, key, &deckey, labels))
		return 0;
            if(!enif_get_map_value(env, ret, deckey, &old_val)) {
                if(!enif_make_map_put(env, ret, deckey, val, &ret)) {
                    return 0;
                }
            }
        }
        *out = ret;
        return 1;
    }
#endif

    ret = enif_make_list(env, 0);
    while(enif_get_list_cell(env, pairs, &val, &pairs)) {
        if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
            assert(0 == 1 && "Unbalanced object pairs.");
        }
	if (!make_key(env, key, &deckey, labels))
	    return 0;
        if(dedupe_keys) {
            ErlNifBinary bin;
            if(!enif_inspect_binary(env, key, &bin)) {
                return 0;
            }
            std::string skey((char*) bin.data, bin.size);
            if(seen.count(skey) == 0) {
                seen.insert(skey);
                val = enif_make_tuple2(env, deckey, val);
                ret = enif_make_list_cell(env, val, ret);
            }
        } else {
            val = enif_make_tuple2(env, deckey, val);
            ret = enif_make_list_cell(env, val, ret);
        }
    }
    *out = enif_make_tuple1(env, ret);

    return 1;
}

END_C
