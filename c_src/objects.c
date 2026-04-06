// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <string.h>

#include "erl_nif.h"

// Must match MAP_SMALL_MAP_LIMIT in OTP's erts/emulator/beam/erl_map.h.
// enif_make_map_from_arrays correctly rejects duplicate keys for
// flatmaps but not for hashmaps (see OTP issue #10975) we rely on
// detecting when we can skip deduplicating based on the small map
// size limit.
#define JIFFY_MAP_SMALL_MAP_LIMIT 32
#define JIFFY_SMALL_PROPLIST_SIZE 32
// Limit up to when we'll allocate the hash table on the stack.
// Must be a power of 2, since we're using a bitmask. Also, we want
// to stay well below 4KB on the stack. That can be checked wit
// something like:
//  clang -O3 -S -fverbose-asm c_src/objects.c -I$(erlnif_include) -Ic_src && grep rsp objects.s
//    subq    $2680, %rsp
//
#define HT_STACK_SLOTS 128

typedef struct {
    ERL_NIF_TERM key;
    int used;
} ht_slot;

// We're using masks, so get the next power of 2 starting with 16.
static unsigned int
ht_size_power_of_2(unsigned int count)
{
    unsigned int p = 16;
    unsigned int target = count * 2;
    while(p < target) {
        p <<= 1;
    }
    return p;
}

// Insert key into table. Returns 1 if inserted, 0 if duplicate. For the salt
// pass the monotonic nanosecond time or something like that.
static inline int
ht_insert(ErlNifEnv* env, ht_slot* table, unsigned int mask,  ERL_NIF_TERM key, ErlNifUInt64 salt)
{
    ErlNifUInt64 h = enif_hash(ERL_NIF_INTERNAL_HASH, key, salt);
    unsigned int idx = (unsigned int)(h & mask);
    while(table[idx].used) {
        if(enif_compare(table[idx].key, key) == 0) {
            return 0;
        }
        idx = (idx + 1) & mask;
    }
    table[idx].key = key;
    table[idx].used = 1;
    return 1;
}


static int
make_map(ErlNifEnv* env, ERL_NIF_TERM pairs, ERL_NIF_TERM* out)
{
    ERL_NIF_TERM ret;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

    // Get the item count up front so we can size the arrays and
    // (if needed) the dedup hash table.
    unsigned int list_len = 0;
    int rv = enif_get_list_length(env, pairs, &list_len);
    assert(rv && "pairs must be a list");
    assert((list_len % 2) == 0 && "Unbalanced object pairs.");
    unsigned int count = list_len / 2;

    if(count == 0) {
        *out = enif_make_new_map(env);
        return 1;
    }

    // Stack allocation is a pointer bump. If we don't initialize and use
    // it, it should not be a performance hit. Using alloca is another
    // option, but that one is non-standard so avoid it for now.
    ERL_NIF_TERM small_keys[JIFFY_MAP_SMALL_MAP_LIMIT];
    ERL_NIF_TERM small_vals[JIFFY_MAP_SMALL_MAP_LIMIT];
    ERL_NIF_TERM* keys = (count <= JIFFY_MAP_SMALL_MAP_LIMIT)
        ? small_keys
        : (ERL_NIF_TERM*) enif_alloc(count * sizeof(ERL_NIF_TERM));
    ERL_NIF_TERM* vals = (count <= JIFFY_MAP_SMALL_MAP_LIMIT)
        ? small_vals
        : (ERL_NIF_TERM*) enif_alloc(count * sizeof(ERL_NIF_TERM));

    // Go in reverse order so that last write wins. It's just the behavior
    // we had before and we're preserving it here for backward compatibility.
    unsigned int i = count;
    while(enif_get_list_cell(env, pairs, &val, &pairs)) {
        if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
            assert(0 && "Unbalanced object pairs.");
        }
        --i;
        keys[i] = key;
        vals[i] = val;
    }

    // With OTP issue #10975 we know enif_make_map_from_arrays works for
    // small maps (<=32 keys), so we can safely use it and not dedup first.
    if(count <= JIFFY_MAP_SMALL_MAP_LIMIT) {
        rv = enif_make_map_from_arrays(env, keys, vals, count, &ret);
        if(rv) {
            *out = ret;
            if(keys != small_keys) {
                enif_free(keys);
            }
            if(vals != small_vals) {
                enif_free(vals);
            }
            return 1;
        }
        // Found dups: fall through, dedup, and try again.
    }

    // Build a hash table for dedup. Walk arrays from the back so the
    // last JSON value for each key is seen first (last-write-wins).
    unsigned int ht_size = ht_size_power_of_2(count);
    ht_slot stack_table[HT_STACK_SLOTS];
    ht_slot* table = (ht_size <= HT_STACK_SLOTS)
        ? stack_table
        : (ht_slot*) enif_alloc(ht_size * sizeof(ht_slot));
    memset(table, 0, ht_size * sizeof(ht_slot));
    unsigned int mask = ht_size - 1;
    ErlNifUInt64 salt = (ErlNifUInt64) enif_monotonic_time(ERL_NIF_NSEC);

    unsigned int unique = 0;
    for(int j = (int)count - 1; j >= 0; j--) {
        if(ht_insert(env, table, mask, keys[j], salt)) {
            unique++;
            keys[count - unique] = keys[j];
            vals[count - unique] = vals[j];
        }
    }

    if(table != stack_table) {
        enif_free(table);
    }

    rv = enif_make_map_from_arrays(env,
        keys + count - unique,
        vals + count - unique,
        unique, &ret);
    assert(rv && "enif_make_map_from_arrays failed after dedup");

    if(keys != small_keys) {
        enif_free(keys);
    }
    if(vals != small_vals) {
        enif_free(vals);
    }

    *out = ret;
    return 1;
}


// Build an EJSON proplist {[{key, val}, ...]} from the interleaved pairs list.
//
// When dedupe_keys is set, only the last occurrence of each key is kept
// (last-wins).
//
static int
make_proplist(ErlNifEnv* env, ERL_NIF_TERM pairs, ERL_NIF_TERM* out,
    int dedupe_keys)
{
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

    unsigned int list_len = 0;
    int rv = enif_get_list_length(env, pairs, &list_len);
    assert(rv && "pairs must be a list");
    assert((list_len % 2) == 0 && "Unbalanced object pairs.");
    unsigned int count = list_len / 2;

    if(count == 0) {
        *out = enif_make_tuple1(env, enif_make_list(env, 0));
        return 1;
    }

    ERL_NIF_TERM small_arr[JIFFY_SMALL_PROPLIST_SIZE];
    ERL_NIF_TERM* arr = (count <= JIFFY_SMALL_PROPLIST_SIZE)
        ? small_arr
        : (ERL_NIF_TERM*) enif_alloc(count * sizeof(ERL_NIF_TERM));

    ht_slot stack_table[HT_STACK_SLOTS];
    ht_slot* table = NULL;
    unsigned int mask = 0;
    ErlNifUInt64 salt = 0;

    if(dedupe_keys) {
        unsigned int ht_size = ht_size_power_of_2(count);
        table = (ht_size <= HT_STACK_SLOTS)
            ? stack_table
            : (ht_slot*) enif_alloc(ht_size * sizeof(ht_slot));
        memset(table, 0, ht_size * sizeof(ht_slot));
        mask = ht_size - 1;
        salt = (ErlNifUInt64) enif_monotonic_time(ERL_NIF_NSEC);
    }

    // Fill array backwards since list is reversed from parsing
    unsigned int unique = 0;
    unsigned int i = count;
    while(enif_get_list_cell(env, pairs, &val, &pairs)) {
        if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
            assert(0 && "Unbalanced object pairs.");
        }
        if(dedupe_keys) {
            if(ht_insert(env, table, mask, key, salt)) {
                --i;
                arr[i] = enif_make_tuple2(env, key, val);
                unique++;
            }
        } else {
            --i;
            arr[i] = enif_make_tuple2(env, key, val);
            unique++;
        }
    }

    if(dedupe_keys && table != stack_table) {
        enif_free(table);
    }

    ERL_NIF_TERM list = enif_make_list_from_array(env, arr + count - unique, unique);

    if(arr != small_arr) {
        enif_free(arr);
    }

    *out = enif_make_tuple1(env, list);
    return 1;
}


int
make_object(ErlNifEnv* env, ERL_NIF_TERM pairs, ERL_NIF_TERM* out,
    int ret_map, int dedupe_keys)
{
    if(ret_map) {
        return make_map(env, pairs, out);
    } else {
        return make_proplist(env, pairs, out, dedupe_keys);
    }
}
