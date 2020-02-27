// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "jiffy.h"
#include "termstack.h"

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

    size_t          bytes_per_red;

    int             uescape;
    int             pretty;
    int             use_nil;
    int             escape_forward_slashes;

    int             shiftcnt;
    int             count;

    size_t          iosize;
    ERL_NIF_TERM    iolist;
    int             partial_output;

    ErlNifBinary    buffer;
    int             have_buffer;

    unsigned char*  p;
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


Encoder*
enc_new(ErlNifEnv* env)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    Encoder* e = enif_alloc_resource(st->res_enc, sizeof(Encoder));

    e->atoms = st;
    e->bytes_per_red = DEFAULT_BYTES_PER_REDUCTION;
    e->uescape = 0;
    e->pretty = 0;
    e->use_nil = 0;
    e->escape_forward_slashes = 0;
    e->shiftcnt = 0;
    e->count = 0;

    e->iosize = 0;
    e->iolist = enif_make_list(env, 0);

    e->partial_output = 0;

    if(!enif_alloc_binary(BIN_INC_SIZE, &e->buffer)) {
        enif_release_resource(e);
        return NULL;
    }

    e->have_buffer = 1;

    e->p = e->buffer.data;
    e->i = 0;

    return e;
}

int
enc_init(Encoder* e, ErlNifEnv* env)
{
    e->env = env;
    return 1;
}

void
enc_destroy(ErlNifEnv* env, void* obj)
{
    Encoder* e = (Encoder*) obj;

    if(e->have_buffer) {
        enif_release_binary(&e->buffer);
    }
}

ERL_NIF_TERM
enc_error(Encoder* e, const char* msg)
{
    //assert(0 && msg);
    return make_error(e->atoms, e->env, msg);
}

ERL_NIF_TERM
enc_obj_error(Encoder* e, const char* msg, ERL_NIF_TERM obj)
{
    return make_obj_error(e->atoms, e->env, msg, obj);
}

int
enc_flush(Encoder* e)
{
    ERL_NIF_TERM bin;

    if(e->i == 0) {
        return 1;
    }

    if(e->i < e->buffer.size) {
        if(!enif_realloc_binary(&e->buffer, e->i)) {
            return 0;
        }
    }

    bin = enif_make_binary(e->env, &e->buffer);
    e->have_buffer = 0;

    e->iolist = enif_make_list_cell(e->env, bin, e->iolist);
    e->iosize += e->i;

    return 1;
}

static inline int
enc_ensure(Encoder* e, size_t req)
{
    size_t new_size = BIN_INC_SIZE;

    if(e->have_buffer) {
        if(req < (e->buffer.size - e->i)) {
            return 1;
        }

        if(!enc_flush(e)) {
            return 0;
        }

        if(e->have_buffer) {
            return 1;
        }
    }

    for(new_size = BIN_INC_SIZE; new_size < req; new_size <<= 1);

    if(!enif_alloc_binary(new_size, &e->buffer)) {
        return 0;
    }

    e->have_buffer = 1;

    e->p = e->buffer.data;
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
enc_unknown(Encoder* e, ERL_NIF_TERM value) {
    // Bignums are encoded in Erlang as the NIF API
    // does not have functions for dealing with them.
    if(!enc_flush(e)) {
        return 0;
    }

    e->iolist = enif_make_list_cell(e->env, value, e->iolist);
    e->partial_output = 1;

    return 1;
}

static inline int
enc_special_character(Encoder* e, int val) {
    switch(val) {
        case '\"':
        case '\\':
            e->p[e->i++] = '\\';
            e->p[e->i++] = val;
            return 1;
        case '\b':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'b';
            return 1;
        case '\f':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'f';
            return 1;
        case '\n':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'n';
            return 1;
        case '\r':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'r';
            return 1;
        case '\t':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 't';
            return 1;
        case '/':
            if(e->escape_forward_slashes) {
                e->p[e->i++] = '\\';
            }
            e->p[e->i++] = '/';
            return 1;
        default:
            if(val < 0x20) {
                e->i += unicode_uescape(val, &(e->p[e->i]));
                return 1;
            }

            return 0;
    }
}

static int
enc_atom(Encoder* e, ERL_NIF_TERM val)
{
    static const int MAX_ESCAPE_LEN = 12;
    unsigned char data[512];

    size_t size;
    int i;

    if(!enif_get_atom(e->env, val, (char*)data, 512, ERL_NIF_LATIN1)) {
        return 0;
    }

    size = strlen((const char*)data);

    /* Reserve space for the first quotation mark and most of the output. */
    if(!enc_ensure(e, size + MAX_ESCAPE_LEN + 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';

    i = 0;
    while(i < size) {
        if(!enc_ensure(e, MAX_ESCAPE_LEN)) {
            return 0;
        }

        if(enc_special_character(e, data[i])) {
            i++;
        } else if(data[i] < 0x80) {
            e->p[e->i++] = data[i];
            i++;
        } else if(data[i] >= 0x80) {
            /* The atom encoding is latin1, so we don't need validation
             * as all latin1 characters are valid Unicode codepoints. */
            if (!e->uescape) {
                e->i += unicode_to_utf8(data[i], &e->p[e->i]);
            } else {
                e->i += unicode_uescape(data[i], &e->p[e->i]);
            }

            i++;
        }
    }

    if(!enc_ensure(e, 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';
    e->count++;

    return 1;
}

static int
enc_string(Encoder* e, ERL_NIF_TERM val)
{
    static const int MAX_ESCAPE_LEN = 12;
    ErlNifBinary bin;

    unsigned char* data;
    size_t size;
    int esc_len;
    int ulen;
    int uval;
    int i;

    if(!enif_inspect_binary(e->env, val, &bin)) {
        return 0;
    }

    data = bin.data;
    size = bin.size;

    /* Reserve space for the first quotation mark and most of the output. */
    if(!enc_ensure(e, size + MAX_ESCAPE_LEN + 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';

    i = 0;
    while(i < size) {
        if(!enc_ensure(e, MAX_ESCAPE_LEN)) {
            return 0;
        }

        if(enc_special_character(e, data[i])) {
            i++;
        } else if(data[i] < 0x80) {
            e->p[e->i++] = data[i++];
        } else if(data[i] >= 0x80) {
            ulen = utf8_validate(&(data[i]), size - i);

            if (ulen < 0) {
                return 0;
            } else if (e->uescape) {
                uval = utf8_to_unicode(&(data[i]), size-i);
                if(uval < 0) {
                    return 0;
                }

                esc_len = unicode_uescape(uval, &(e->p[e->i]));
                if(esc_len < 0) {
                    return 0;
                }

                e->i += esc_len;
            } else {
                memcpy(&e->p[e->i], &data[i], ulen);
                e->i += ulen;
            }

            i += ulen;
        }
    }

    if(!enc_ensure(e, 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';
    e->count++;

    return 1;
}

static inline int
enc_object_key(ErlNifEnv *env, Encoder* e, ERL_NIF_TERM val)
{
    if(enif_is_atom(env, val)) {
        return enc_atom(e, val);
    }

    return enc_string(e, val);
}

// From https://www.slideshare.net/andreialexandrescu1/three-optimization-tips-for-c-15708507

#define P01 10
#define P02 100
#define P03 1000
#define P04 10000
#define P05 100000
#define P06 1000000
#define P07 10000000
#define P08 100000000
#define P09 1000000000
#define P10 10000000000
#define P11 100000000000L
#define P12 1000000000000L

int
digits10(ErlNifUInt64 v)
{
    if (v < P01) return 1;
    if (v < P02) return 2;
    if (v < P03) return 3;
    if (v < P12) {
        if (v < P08) {
            if (v < P06) {
                if (v < P04) {
                    return 4;
                }
                return 5 + (v >= P05);
            }
            return 7 + (v >= P07);
        }
        if (v < P10) {
            return 9 + (v >= P09);
        }
        return 11 + (v >= P11);
    }
    return 12 + digits10(v / P12);
}

unsigned int
u64ToAsciiTable(unsigned char *dst, ErlNifUInt64 value)
{
    static const char digits[201] =
        "0001020304050607080910111213141516171819"
        "2021222324252627282930313233343536373839"
        "4041424344454647484950515253545556575859"
        "6061626364656667686970717273747576777879"
        "8081828384858687888990919293949596979899";
    const int length = digits10(value);
    int next = length - 1;
    while (value >= 100) {
        const int i = (value % 100) * 2;
        value /= 100;
        dst[next] = digits[i + 1];
        dst[next - 1] = digits[i];
        next -= 2;
    }
    // Handle last 1-2 digits.
    if (value < 10) {
        dst[next] = '0' + (unsigned int) value;
    } else {
        const int i = (unsigned int) value * 2;
        dst[next] = digits[i + 1];
        dst[next - 1] = digits[i];
    }
    return length;
}

unsigned
i64ToAsciiTable(unsigned char *dst, ErlNifSInt64 value)
{
    if (value < 0) {
        *dst++ = '-';
        return 1 + u64ToAsciiTable(dst, -value);
    } else {
        return u64ToAsciiTable(dst, value);
    }
}

static inline int
enc_long(Encoder* e, ErlNifSInt64 val)
{
    if(!enc_ensure(e, 32)) {
        return 0;
    }

    e->i += i64ToAsciiTable(&(e->p[e->i]), val);
    e->count++;

    return 1;
}

static inline int
enc_double(Encoder* e, double val)
{
    unsigned char* start;
    size_t len;

    if(!enc_ensure(e, 32)) {
        return 0;
    }

    start = &(e->p[e->i]);

    if(!double_to_shortest(start, e->buffer.size, &len, val)) {
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

#if MAP_TYPE_PRESENT
int
enc_map_to_ejson(ErlNifEnv* env, ERL_NIF_TERM map, ERL_NIF_TERM* out)
{
    ErlNifMapIterator iter;
    size_t size;

    ERL_NIF_TERM list;
    ERL_NIF_TERM tuple;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

    if(!enif_get_map_size(env, map, &size)) {
        return 0;
    }

    list = enif_make_list(env, 0);

    if(size == 0) {
        *out = enif_make_tuple1(env, list);
        return 1;
    }

    if(!enif_map_iterator_create(env, map, &iter, ERL_NIF_MAP_ITERATOR_HEAD)) {
        return 0;
    }

    do {
        if(!enif_map_iterator_get_pair(env, &iter, &key, &val)) {
            enif_map_iterator_destroy(env, &iter);
            return 0;
        }
        tuple = enif_make_tuple2(env, key, val);
        list = enif_make_list_cell(env, tuple, list);
    } while(enif_map_iterator_next(env, &iter));

    enif_map_iterator_destroy(env, &iter);

    *out = enif_make_tuple1(env, list);
    return 1;
}
#endif

ERL_NIF_TERM
encode_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    Encoder* e;

    ERL_NIF_TERM opts;
    ERL_NIF_TERM val;
    ERL_NIF_TERM tmp_argv[3];

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    e = enc_new(env);
    if(e == NULL) {
        return make_error(st, env, "internal_error");
    }

    tmp_argv[0] = enif_make_resource(env, e);
    tmp_argv[1] = enif_make_tuple1(env, argv[0]);
    tmp_argv[2] = enif_make_list(env, 0);

    enif_release_resource(e);

    opts = argv[1];
    if(!enif_is_list(env, opts)) {
        return enif_make_badarg(env);
    }

    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(enif_is_identical(val, e->atoms->atom_uescape)) {
            e->uescape = 1;
        } else if(enif_is_identical(val, e->atoms->atom_pretty)) {
            e->pretty = 1;
        } else if(enif_is_identical(val, e->atoms->atom_escape_forward_slashes)) {
            e->escape_forward_slashes = 1;
        } else if(enif_is_identical(val, e->atoms->atom_use_nil)) {
            e->use_nil = 1;
        } else if(enif_is_identical(val, e->atoms->atom_force_utf8)) {
            // Ignore, handled in Erlang
        } else if(get_bytes_per_iter(env, val, &(e->bytes_per_red))) {
            continue;
        } else if(get_bytes_per_red(env, val, &(e->bytes_per_red))) {
            continue;
        } else {
            return enif_make_badarg(env);
        }
    }

    return encode_iter(env, 3, tmp_argv);
}

ERL_NIF_TERM
encode_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    Encoder* e;
    TermStack stack;

    ERL_NIF_TERM ret = 0;

    ERL_NIF_TERM curr;
    ERL_NIF_TERM item;
    const ERL_NIF_TERM* tuple;
    ERL_NIF_TERM tmp_argv[3];
    int arity;
    ErlNifSInt64 lval;
    double dval;

    void* res;

    size_t start;
    size_t bytes_processed = 0;

    if(!enif_get_resource(env, argv[0], st->res_enc, &res)) {
        return enif_make_badarg(env);
    }

    e = (Encoder*) res;

    if(!enc_init(e, env)) {
        return enif_make_badarg(env);
    }

    if(!termstack_restore(env, argv[1], &stack)) {
        return enif_make_badarg(env);
    }

    e->iolist = argv[2];

    start = e->iosize + e->i;

    while(!termstack_is_empty(&stack)) {
        bytes_processed = (e->iosize + e->i) - start;

        if(should_yield(bytes_processed, e->bytes_per_red)) {

            assert(enif_is_list(env, e->iolist));

            tmp_argv[0] = argv[0];
            tmp_argv[1] = termstack_save(env, &stack);
            tmp_argv[2] = e->iolist;

            termstack_destroy(&stack);
            bump_used_reds(env, bytes_processed, e->bytes_per_red);

#if SCHEDULE_NIF_PRESENT
            return enif_schedule_nif(
                    env,
                    "nif_encode_iter",
                    0,
                    encode_iter,
                    3,
                    tmp_argv
                );
#else
            return enif_make_tuple2(
                    env,
                    st->atom_iter,
                    enif_make_tuple_from_array(env, tmp_argv, 3)
                );
#endif
        }

        curr = termstack_pop(&stack);

        if(enif_is_atom(env, curr)) {
            if(enif_is_identical(curr, e->atoms->ref_object)) {
                curr = termstack_pop(&stack);

                if(!enif_get_list_cell(env, curr, &item, &curr)) {
                    if(!enc_end_object(e)) {
                        ret = enc_error(e, "internal_error");
                        goto done;
                    }
                    continue;
                }
                if(!enif_get_tuple(env, item, &arity, &tuple)) {
                    ret = enc_obj_error(e, "invalid_object_member", item);
                    goto done;
                }
                if(arity != 2) {
                    ret = enc_obj_error(e, "invalid_object_member_arity", item);
                    goto done;
                }
                if(!enc_comma(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                if(!enc_object_key(env, e, tuple[0])) {
                    ret = enc_obj_error(e, "invalid_object_member_key", tuple[0]);
                    goto done;
                }
                if(!enc_colon(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }

                termstack_push(&stack, curr);
                termstack_push(&stack, e->atoms->ref_object);
                termstack_push(&stack, tuple[1]);
            } else if(enif_is_identical(curr, e->atoms->ref_array)) {
                curr = termstack_pop(&stack);

                if(!enif_get_list_cell(env, curr, &item, &curr)) {
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

                termstack_push(&stack, curr);
                termstack_push(&stack, e->atoms->ref_array);
                termstack_push(&stack, item);
            } else if(enif_is_identical(curr, e->atoms->atom_null)) {
                if(!enc_literal(e, "null", 4)) {
                    ret = enc_error(e, "null");
                    goto done;
                }
            } else if(e->use_nil && enif_is_identical(curr, e->atoms->atom_nil)) {
                if(!enc_literal(e, "null", 4)) {
                    ret = enc_error(e, "null");
                    goto done;
                }
            } else if(enif_is_identical(curr, e->atoms->atom_true)) {
                if(!enc_literal(e, "true", 4)) {
                    ret = enc_error(e, "true");
                    goto done;
                }
            } else if(enif_is_identical(curr, e->atoms->atom_false)) {
                if(!enc_literal(e, "false", 5)) {
                    ret = enc_error(e, "false");
                    goto done;
                }
            } else if(!enc_atom(e, curr)) {
                ret = enc_obj_error(e, "invalid_string", curr);
                goto done;
            }
        } else if(enif_is_binary(env, curr)) {
            if(!enc_string(e, curr)) {
                ret = enc_obj_error(e, "invalid_string", curr);
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
                ret = enc_obj_error(e, "invalid_ejson", curr);
                goto done;
            }
            if(!enif_is_list(env, tuple[0])) {
                ret = enc_obj_error(e, "invalid_object", curr);
                goto done;
            }
            if(!enc_start_object(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(!enif_get_list_cell(env, tuple[0], &item, &curr)) {
                if(!enc_end_object(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }
            if(!enif_get_tuple(env, item, &arity, &tuple)) {
                ret = enc_obj_error(e, "invalid_object_member", item);
                goto done;
            }
            if(arity != 2) {
                ret = enc_obj_error(e, "invalid_object_member_arity", item);
                goto done;
            }
            if(!enc_object_key(env, e, tuple[0])) {
                ret = enc_obj_error(e, "invalid_object_member_key", tuple[0]);
                goto done;
            }
            if(!enc_colon(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }

            termstack_push(&stack, curr);
            termstack_push(&stack, e->atoms->ref_object);
            termstack_push(&stack, tuple[1]);
#if MAP_TYPE_PRESENT
        } else if(enif_is_map(env, curr)) {
            if(!enc_map_to_ejson(env, curr, &curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }

            termstack_push(&stack, curr);
#endif
        } else if(enif_is_list(env, curr)) {
            if(!enc_start_array(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }

            if(!enif_get_list_cell(env, curr, &item, &curr)) {
                if(!enc_end_array(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }

            termstack_push(&stack, curr);
            termstack_push(&stack, e->atoms->ref_array);
            termstack_push(&stack, item);
        } else {
            if(!enc_unknown(e, curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        }
    }

    if(!enc_flush(e)) {
        ret = enc_error(e, "internal_error");
        goto done;
    }

    assert(enif_is_list(env, e->iolist));

    if(e->partial_output) {
        ret = enif_make_tuple2(env, e->atoms->atom_partial, e->iolist);
    } else {
        ret = e->iolist;
    }

done:
    bump_used_reds(env, bytes_processed, e->bytes_per_red);
    termstack_destroy(&stack);

    return ret;
}
