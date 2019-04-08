// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "jiffy.h"
#include "termstack.h"

ERL_NIF_TERM
termstack_save(ErlNifEnv* env, TermStack* stack)
{
    return enif_make_tuple_from_array(env, stack->elements, stack->top);
}

int
termstack_restore(ErlNifEnv* env, ERL_NIF_TERM from, TermStack* stack)
{
    const ERL_NIF_TERM* elements;
    int arity;

    if(enif_get_tuple(env, from, &arity, &elements)) {
        stack->top = arity;

        if(arity <= SMALL_TERMSTACK_SIZE) {
            stack->elements = &stack->__default_elements[0];
            stack->size = SMALL_TERMSTACK_SIZE;
        } else {
            stack->size = arity * 2;
            stack->elements = enif_alloc(stack->size * sizeof(ERL_NIF_TERM));

            if(!stack->elements) {
                return 0;
            }
        }

        memcpy(stack->elements, elements, arity * sizeof(ERL_NIF_TERM));
        return 1;
    }

    return 0;
}

void
termstack_destroy(TermStack* stack)
{
    if(stack->elements != &stack->__default_elements[0]) {
        enif_free(stack->elements);
    }
}

inline void
termstack_push(TermStack* stack, ERL_NIF_TERM term)
{

    if(stack->top == stack->size) {
        size_t new_size = stack->size * 2;
        size_t num_bytes = new_size * sizeof(ERL_NIF_TERM);

        if (stack->elements == &stack->__default_elements[0]) {
            ERL_NIF_TERM* elems = enif_alloc(num_bytes);
            memcpy(elems, stack->elements, num_bytes);
            stack->elements = elems;
        } else {
            stack->elements = enif_realloc(stack->elements, num_bytes);
        }

        stack->size = new_size;
    }

    assert(stack->top < stack->size);
    stack->elements[stack->top++] = term;
}

inline ERL_NIF_TERM
termstack_pop(TermStack* stack)
{
    assert(stack->top > 0 && stack->top <= stack->size);
    return stack->elements[--stack->top];
}

inline int
termstack_is_empty(TermStack* stack)
{
    return stack->top == 0;
}
