// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#ifndef TERMSTACK_H
#define TERMSTACK_H

#include "erl_nif.h"

#define SMALL_TERMSTACK_SIZE 16

typedef struct {
    ERL_NIF_TERM* elements;
    size_t size;
    size_t top;

    ERL_NIF_TERM __default_elements[SMALL_TERMSTACK_SIZE];
} TermStack;


ERL_NIF_TERM termstack_save(ErlNifEnv* env, TermStack* stack);
int termstack_restore(ErlNifEnv* env, ERL_NIF_TERM from, TermStack* stack);
void termstack_destroy(TermStack* stack);

void termstack_push(TermStack* stack, ERL_NIF_TERM term);
ERL_NIF_TERM termstack_pop(TermStack* stack);
int termstack_is_empty(TermStack* stack);

#endif
