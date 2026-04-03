// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#ifndef JIFFY_H
#define JIFFY_H

#include "erl_nif.h"

#define DEFAULT_BYTES_PER_REDUCTION 20

// This used to be 2000 and in 19.2 was bumped to 4000
// #define CONTEXT_REDS in erts/emulator/beam/erl_vm.h
#define DEFAULT_ERLANG_REDUCTION_COUNT 4000

// Check for C99
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
  #define JIFFY_RESTRICT restrict
#else
  #define JIFFY_RESTRICT
#endif

#if WINDOWS || WIN32
  #define inline __inline
#endif

typedef struct {
    ERL_NIF_TERM    atom_ok;
    ERL_NIF_TERM    atom_error;
    ERL_NIF_TERM    atom_null;
    ERL_NIF_TERM    atom_true;
    ERL_NIF_TERM    atom_false;
    ERL_NIF_TERM    atom_bignum;
    ERL_NIF_TERM    atom_bignum_e;
    ERL_NIF_TERM    atom_bigdbl;
    ERL_NIF_TERM    atom_partial;
    ERL_NIF_TERM    atom_uescape;
    ERL_NIF_TERM    atom_pretty;
    ERL_NIF_TERM    atom_force_utf8;
    ERL_NIF_TERM    atom_iter;
    ERL_NIF_TERM    atom_bytes_per_iter;
    ERL_NIF_TERM    atom_bytes_per_red;
    ERL_NIF_TERM    atom_return_maps;
    ERL_NIF_TERM    atom_return_trailer;
    ERL_NIF_TERM    atom_has_trailer;
    ERL_NIF_TERM    atom_nil;
    ERL_NIF_TERM    atom_use_nil;
    ERL_NIF_TERM    atom_null_term;
    ERL_NIF_TERM    atom_escape_forward_slashes;
    ERL_NIF_TERM    atom_dedupe_keys;
    ERL_NIF_TERM    atom_copy_strings;

    ERL_NIF_TERM    ref_object;
    ERL_NIF_TERM    ref_array;

    ErlNifResourceType* res_dec;
    ErlNifResourceType* res_enc;
} jiffy_st;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
int get_bytes_per_iter(ErlNifEnv* env, ERL_NIF_TERM val, size_t* bpi);
int get_bytes_per_red(ErlNifEnv* env, ERL_NIF_TERM val, size_t* bpr);
int get_null_term(ErlNifEnv* env, ERL_NIF_TERM val, ERL_NIF_TERM *null_term);
static inline size_t yield_threshold(size_t bytes_per_red) {
    return bytes_per_red * DEFAULT_ERLANG_REDUCTION_COUNT;
}
void bump_used_reds(ErlNifEnv* env, size_t used, size_t bytes_per_red);

ERL_NIF_TERM decode_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM decode_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encode_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encode_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

void dec_destroy(ErlNifEnv* env, void* obj);
void enc_destroy(ErlNifEnv* env, void* obj);

int make_object(ErlNifEnv* env, ERL_NIF_TERM pairs, ERL_NIF_TERM* out,
        int ret_map, int dedupe_keys);

int int_from_hex(const unsigned char* p);
int int_to_hex(int val, unsigned char* p);
int utf8_len(int c);
int utf8_esc_len(int c);
size_t utf8_validate(unsigned char* data, size_t size);
int utf8_to_unicode(unsigned char* buf, size_t size);
int unicode_to_utf8(int c, unsigned char* buf);
int unicode_from_pair(int hi, int lo);
int unicode_uescape(int c, unsigned char* buf);

#endif // Included JIFFY_H
