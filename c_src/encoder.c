// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "jiffy.h"

#define BIN_INC_SIZE 2048

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))

#define MAYBE_PRETTY(e)             \
do {                                \
    if(e->pretty) {                 \
        if(!enc_shift(e))           \
            return 0;               \
    }                               \
} while(0)

#if WINDOWS || WIN32
#define inline __inline
#define snprintf  _snprintf
#endif

typedef struct {
    ErlNifEnv*      env;
    jiffy_st*       atoms;
    int             uescape;
    int             pretty;

    int             shiftcnt;
    int             count;

    int             iolen;
    ERL_NIF_TERM    iolist;
    ErlNifBinary*   curr;


    char*           p;
    unsigned char*  u;
    size_t          i;
} Encoder;


// String constants for pretty printing.
// Every string starts with its length.
#define NUM_SHIFTS 8
static char* shifts[NUM_SHIFTS] = {
    "\x01\n",
    "\x03\n  ",
    "\x05\n    ",
    "\x07\n      ",
    "\x09\n        ",
    "\x0b\n          ",
    "\x0d\n            ",
    "\x0f\n              "
};


int
enc_init(Encoder* e, ErlNifEnv* env, ERL_NIF_TERM opts, ErlNifBinary* bin)
{
    ERL_NIF_TERM val;

    e->env = env;
    e->atoms = enif_priv_data(env);
    e->uescape = 0;
    e->pretty = 0;
    e->shiftcnt = 0;
    e->count = 0;

    if(!enif_is_list(env, opts)) {
        return 0;
    }

    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(enif_compare(val, e->atoms->atom_uescape) == 0) {
            e->uescape = 1;
        } else if(enif_compare(val, e->atoms->atom_pretty) == 0) {
            e->pretty = 1;
        } else if(enif_compare(val, e->atoms->atom_force_utf8) == 0) {
            // Ignore, handled in Erlang
        } else {
            return 0;
        }
    }

    e->iolen = 0;
    e->iolist = enif_make_list(env, 0);
    e->curr = bin;
    if(!enif_alloc_binary(BIN_INC_SIZE, e->curr)) {
        return 0;
    }

    memset(e->curr->data, 0, e->curr->size);

    e->p = (char*) e->curr->data;
    e->u = (unsigned char*) e->curr->data;
    e->i = 0;

    return 1;
}

void
enc_destroy(Encoder* e)
{
    if(e->curr != NULL) {
        enif_release_binary(e->curr);
    }
}

ERL_NIF_TERM
enc_error(Encoder* e, const char* msg)
{
    //assert(0 && msg);
    return make_error(e->atoms, e->env, msg);
}

static inline int
enc_ensure(Encoder* e, size_t req)
{
    size_t need = e->curr->size;
    while(req >= (need - e->i)) need <<= 1;

    if(need != e->curr->size) {
        if(!enif_realloc_binary(e->curr, need)) {
            return 0;
        }
        e->p = (char*) e->curr->data;
        e->u = (unsigned char*) e->curr->data;
    }

    return 1;
}

int
enc_result(Encoder* e, ERL_NIF_TERM* value)
{
    if(e->i != e->curr->size) {
        if(!enif_realloc_binary(e->curr, e->i)) {
            return 0;
        }
    }

    *value = enif_make_binary(e->env, e->curr);
    e->curr = NULL;
    return 1;
}

int
enc_done(Encoder* e, ERL_NIF_TERM* value)
{
    ERL_NIF_TERM last;

    if(e->iolen == 0) {
        return enc_result(e, value);
    }

    if(e->i > 0 ) {
        if(!enc_result(e, &last)) {
            return 0;
        }

        e->iolist = enif_make_list_cell(e->env, last, e->iolist);
        e->iolen++;
    }

    *value = e->iolist;
    return 1;
}

static inline int
enc_unknown(Encoder* e, ERL_NIF_TERM value)
{
    ErlNifBinary* bin = e->curr;
    ERL_NIF_TERM curr;

    if(e->i > 0) {
        if(!enc_result(e, &curr)) {
            return 0;
        }

        e->iolist = enif_make_list_cell(e->env, curr, e->iolist);
        e->iolen++;
    }

    e->iolist = enif_make_list_cell(e->env, value, e->iolist);
    e->iolen++;

    // Reinitialize our binary for the next buffer.
    e->curr = bin;
    if(!enif_alloc_binary(BIN_INC_SIZE, e->curr)) {
        return 0;
    }

    memset(e->curr->data, 0, e->curr->size);

    e->p = (char*) e->curr->data;
    e->u = (unsigned char*) e->curr->data;
    e->i = 0;

    return 1;
}

static inline int
enc_literal(Encoder* e, const char* literal, size_t len)
{
    if(!enc_ensure(e, len)) {
        return 0;
    }

    memcpy(&(e->p[e->i]), literal, len);
    e->i += len;
    e->count++;
    return 1;
}

static inline int
enc_string(Encoder* e, ERL_NIF_TERM val)
{
    ErlNifBinary bin;
    char atom[512];

    unsigned char* data;
    size_t size;

    int esc_extra = 0;
    int ulen;
    int uval;
    int i;

    if(enif_is_binary(e->env, val)) {
        if(!enif_inspect_binary(e->env, val, &bin)) {
            return 0;
        }
        data = bin.data;
        size = bin.size;
    } else if(enif_is_atom(e->env, val)) {
        if(!enif_get_atom(e->env, val, atom, 512, ERL_NIF_LATIN1)) {
            return 0;
        }
        data = (unsigned char*) atom;
        size = strlen(atom);
    } else {
        return 0;
    }

    i = 0;
    while(i < size) {
        switch((char) data[i]) {
            case '\"':
            case '\\':
            case '\b':
            case '\f':
            case '\n':
            case '\r':
            case '\t':
                esc_extra += 1;
                i++;
                continue;
            default:
                if(data[i] < 0x20) {
                    esc_extra += 5;
                    i++;
                    continue;
                } else if(data[i] < 0x80) {
                    i++;
                    continue;
                }
                ulen = utf8_validate(&(data[i]), size - i);
                if(ulen < 0) {
                    return 0;
                }
                if(e->uescape) {
                    uval = utf8_to_unicode(&(data[i]), ulen);
                    if(uval < 0) {
                        return 0;
                    }
                    esc_extra += utf8_esc_len(uval);
                    if(ulen < 0) {
                        return 0;
                    }
                }
                i += ulen;
        }
    }

    if(!enc_ensure(e, size + esc_extra + 2)) {
        return 0;
    }

    e->p[e->i++] = '\"';

    i = 0;
    while(i < size) {
        switch((char) data[i]) {
            case '\"':
            case '\\':
                e->p[e->i++] = '\\';
                e->u[e->i++] = data[i];
                i++;
                continue;
            case '\b':
                e->p[e->i++] = '\\';
                e->p[e->i++] = 'b';
                i++;
                continue;
            case '\f':
                e->p[e->i++] = '\\';
                e->p[e->i++] = 'f';
                i++;
                continue;
            case '\n':
                e->p[e->i++] = '\\';
                e->p[e->i++] = 'n';
                i++;
                continue;
            case '\r':
                e->p[e->i++] = '\\';
                e->p[e->i++] = 'r';
                i++;
                continue;
            case '\t':
                e->p[e->i++] = '\\';
                e->p[e->i++] = 't';
                i++;
                continue;
            default:
                if(data[i] < 0x20) {
                    ulen = unicode_uescape(data[i], &(e->p[e->i]));
                    if(ulen < 0) {
                        return 0;
                    }
                    e->i += ulen;
                    i++;
                } else if((data[i] & 0x80) && e->uescape) {
                    uval = utf8_to_unicode(&(data[i]), size-i);
                    if(uval < 0) {
                        return 0;
                    }

                    ulen = unicode_uescape(uval, &(e->p[e->i]));
                    if(ulen < 0) {
                        return 0;
                    }
                    e->i += ulen;

                    ulen = utf8_len(uval);
                    if(ulen < 0) {
                        return 0;
                    }
                    i += ulen;
                } else {
                    e->u[e->i++] = data[i++];
                }
        }
    }

    e->p[e->i++] = '\"';
    e->count++;

    return 1;
}

static inline int
enc_long(Encoder* e, ErlNifSInt64 val)
{
    if(!enc_ensure(e, 32)) {
        return 0;
    }

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
    snprintf(&(e->p[e->i]), 32, "%ld", val);
#elif SIZEOF_LONG == 8
    snprintf(&(e->p[e->i]), 32, "%ld", val);
#else
    snprintf(&(e->p[e->i]), 32, "%lld", val);
#endif

    e->i += strlen(&(e->p[e->i]));
    e->count++;

    return 1;
}

static inline int
enc_double(Encoder* e, double val)
{
    char* start;
    size_t len;

    if(!enc_ensure(e, 32)) {
        return 0;
    }

    start = &(e->p[e->i]);

    if(!double_to_shortest(start, e->curr->size, &len, val)) {
        return 0;
    }

    e->i += len;
    e->count++;
    return 1;
}

static inline int
enc_char(Encoder* e, char c)
{
    if(!enc_ensure(e, 1)) {
        return 0;
    }

    e->p[e->i++] = c;
    return 1;
}

static int
enc_shift(Encoder* e) {
    int i;
    char* shift;
    assert(e->shiftcnt >= 0 && "Invalid shift count.");
    shift = shifts[MIN(e->shiftcnt, NUM_SHIFTS-1)];

    if(!enc_literal(e, shift + 1, *shift))
        return 0;

    // Finish the rest of this shift it's it bigger than
    // our largest predefined constant.
    for(i = NUM_SHIFTS - 1; i < e->shiftcnt; i++) {
        if(!enc_literal(e, "  ", 2))
            return 0;
    }

    return 1;
}

static inline int
enc_start_object(Encoder* e)
{
    e->count++;
    e->shiftcnt++;
    if(!enc_char(e, '{'))
        return 0;
    MAYBE_PRETTY(e);
    return 1;
}

static inline int
enc_end_object(Encoder* e)
{
    e->shiftcnt--;
    MAYBE_PRETTY(e);
    return enc_char(e, '}');
}

static inline int
enc_start_array(Encoder* e)
{
    e->count++;
    e->shiftcnt++;
    if(!enc_char(e, '['))
        return 0;
    MAYBE_PRETTY(e);
    return 1;
}

static inline int
enc_end_array(Encoder* e)
{
    e->shiftcnt--;
    MAYBE_PRETTY(e);
    return enc_char(e, ']');
}

static inline int
enc_colon(Encoder* e)
{
    if(e->pretty)
        return enc_literal(e, " : ", 3);
    return enc_char(e, ':');
}

static inline int
enc_comma(Encoder* e)
{
    if(!enc_char(e, ','))
        return 0;
    MAYBE_PRETTY(e);
    return 1;
}

ERL_NIF_TERM
encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Encoder enc;
    Encoder* e = &enc;

    ErlNifBinary bin;
    ERL_NIF_TERM ret;

    ERL_NIF_TERM stack;
    ERL_NIF_TERM curr;
    ERL_NIF_TERM item;
    const ERL_NIF_TERM* tuple;
    int arity;
    ErlNifSInt64 lval;
    double dval;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enc_init(e, env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    stack = enif_make_list(env, 1, argv[0]);

    while(!enif_is_empty_list(env, stack)) {
        if(!enif_get_list_cell(env, stack, &curr, &stack)) {
            ret = enc_error(e, "internal_error");
            goto done;
        }
        if(enif_is_identical(curr, e->atoms->ref_object)) {
            if(!enif_get_list_cell(env, stack, &curr, &stack)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(enif_is_empty_list(env, curr)) {
                if(!enc_end_object(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }
            if(!enif_get_list_cell(env, curr, &item, &curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(!enif_get_tuple(env, item, &arity, &tuple)) {
                ret = enc_error(e, "invalid_object_pair");
                goto done;
            }
            if(arity != 2) {
                ret = enc_error(e, "invalid_object_pair");
                goto done;
            }
            if(!enc_comma(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(!enc_string(e, tuple[0])) {
                ret = enc_error(e, "invalid_object_key");
                goto done;
            }
            if(!enc_colon(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            stack = enif_make_list_cell(env, curr, stack);
            stack = enif_make_list_cell(env, e->atoms->ref_object, stack);
            stack = enif_make_list_cell(env, tuple[1], stack);
        } else if(enif_is_identical(curr, e->atoms->ref_array)) {
            if(!enif_get_list_cell(env, stack, &curr, &stack)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(enif_is_empty_list(env, curr)) {
                if(!enc_end_array(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }
            if(!enc_comma(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(!enif_get_list_cell(env, curr, &item, &curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            stack = enif_make_list_cell(env, curr, stack);
            stack = enif_make_list_cell(env, e->atoms->ref_array, stack);
            stack = enif_make_list_cell(env, item, stack);
        } else if(enif_compare(curr, e->atoms->atom_null) == 0) {
            if(!enc_literal(e, "null", 4)) {
                ret = enc_error(e, "null");
                goto done;
            }
        } else if(enif_compare(curr, e->atoms->atom_true) == 0) {
            if(!enc_literal(e, "true", 4)) {
                ret = enc_error(e, "true");
                goto done;
            }
        } else if(enif_compare(curr, e->atoms->atom_false) == 0) {
            if(!enc_literal(e, "false", 5)) {
                ret = enc_error(e, "false");
                goto done;
            }
        } else if(enif_is_binary(env, curr)) {
            if(!enc_string(e, curr)) {
                ret = enc_error(e, "invalid_string");
                goto done;
            }
        } else if(enif_is_atom(env, curr)) {
            if(!enc_string(e, curr)) {
                ret = enc_error(e, "invalid_string");
                goto done;
            }
        } else if(enif_get_int64(env, curr, &lval)) {
            if(!enc_long(e, lval)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        } else if(enif_get_double(env, curr, &dval)) {
            if(!enc_double(e, dval)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        } else if(enif_get_tuple(env, curr, &arity, &tuple)) {
            if(arity != 1) {
                ret = enc_error(e, "invalid_ejson");
                goto done;
            }
            if(!enif_is_list(env, tuple[0])) {
                ret = enc_error(e, "invalid_object");
                goto done;
            }
            if(!enc_start_object(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(enif_is_empty_list(env, tuple[0])) {
                if(!enc_end_object(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }
            if(!enif_get_list_cell(env, tuple[0], &item, &curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(!enif_get_tuple(env, item, &arity, &tuple)) {
                ret = enc_error(e, "invalid_object_member");
                goto done;
            }
            if(arity != 2) {
                ret = enc_error(e, "invalid_object_member_arity");
                goto done;
            }
            if(!enc_string(e, tuple[0])) {
                ret = enc_error(e, "invalid_object_member_key");
                goto done;
            }
            if(!enc_colon(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            stack = enif_make_list_cell(env, curr, stack);
            stack = enif_make_list_cell(env, e->atoms->ref_object, stack);
            stack = enif_make_list_cell(env, tuple[1], stack);
        } else if(enif_is_list(env, curr)) {
            if(!enc_start_array(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(enif_is_empty_list(env, curr)) {
                if(!enc_end_array(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }
            if(!enif_get_list_cell(env, curr, &item, &curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            stack = enif_make_list_cell(env, curr, stack);
            stack = enif_make_list_cell(env, e->atoms->ref_array, stack);
            stack = enif_make_list_cell(env, item, stack);
        } else {
            if(!enc_unknown(e, curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        }
    }

    if(!enc_done(e, &item)) {
        ret = enc_error(e, "internal_error");
        goto done;
    }

    if(e->iolen == 0) {
        ret = item;
    } else {
        ret = enif_make_tuple2(env, e->atoms->atom_partial, item);
    }

done:
    enc_destroy(e);
    return ret;
}
