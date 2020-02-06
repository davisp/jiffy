// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#ifndef TERMSTACK_H
#define TERMSTACK_H

#include "erl_nif.h"

#ifdef _WIN32
#define JIFFY_EXPORT __declspec(dllexport)
#else
#define JIFFY_EXPORT
#endif

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

JIFFY_EXPORT void termstack_push(TermStack* stack, ERL_NIF_TERM term);
JIFFY_EXPORT ERL_NIF_TERM termstack_pop(TermStack* stack);
JIFFY_EXPORT int termstack_is_empty(TermStack* stack);

#endif
