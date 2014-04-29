// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#ifndef JIFFY_H
#define JIFFY_H

#include "erl_nif.h"

#define REDUCTIONS 1000

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
    ERL_NIF_TERM    atom_reductions;

    ERL_NIF_TERM    ref_object;
    ERL_NIF_TERM    ref_array;

    ErlNifResourceType *res_encoder;
    ErlNifResourceType *res_decoder;
} jiffy_st;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_ok(jiffy_st* st, ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM make_error(jiffy_st* st, ErlNifEnv* env, const char* error);
int get_reductions(ErlNifEnv *env, ERL_NIF_TERM term, jiffy_st* st, size_t* val);
int jiffy_consume_timeslice(ErlNifEnv *env, size_t reds, size_t cur, size_t* proc);

ERL_NIF_TERM decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
void enc_destroy(ErlNifEnv* env, void* e);
void dec_destroy(ErlNifEnv* env, void* d);

int int_from_hex(const unsigned char* p);
int int_to_hex(int val, char* p);
int utf8_len(int c);
int utf8_esc_len(int c);
int utf8_validate(unsigned char* data, size_t size);
int utf8_to_unicode(unsigned char* buf, size_t size);
int unicode_to_utf8(int c, unsigned char* buf);
int unicode_from_pair(int hi, int lo);
int unicode_uescape(int c, char* buf);
int double_to_shortest(char *buf, size_t size, size_t* len, double val);

#endif // Included JIFFY_H
