// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#ifndef JIFFY_UTF8_H
#define JIFFY_UTF8_H

#include <string.h>

static const unsigned char hexvals[256] = {
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,
      8,   9, 255, 255, 255, 255, 255, 255,
    255,  10,  11,  12,  13,  14,  15, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255,  10,  11,  12,  13,  14,  15, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,

    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255
};

static const char hexdigits[16] = {
    '0', '1', '2', '3',
    '4', '5', '6', '7',
    '8', '9', 'A', 'B',
    'C', 'D', 'E', 'F'
};

static inline int
int_from_hex(const unsigned char* p)
{
    unsigned char* h = (unsigned char*) p;
    int ret;

    if(hexvals[*(h+0)] == 255) return -1;
    if(hexvals[*(h+1)] == 255) return -1;
    if(hexvals[*(h+2)] == 255) return -1;
    if(hexvals[*(h+3)] == 255) return -1;

    ret = (hexvals[*(h+0)] << 12)
        + (hexvals[*(h+1)] << 8)
        + (hexvals[*(h+2)] << 4)
        + (hexvals[*(h+3)] << 0);

    return ret;
}

static inline int
int_to_hex(int val, unsigned char* p)
{
    if(val < 0 || val > 65535)
        return -1;

    p[0] = hexdigits[(val >> 12) & 0xF];
    p[1] = hexdigits[(val >> 8) & 0xF];
    p[2] = hexdigits[(val >> 4) & 0xF];
    p[3] = hexdigits[val & 0xF];

    return 1;
}

static inline int
utf8_len(int c)
{
    if(c < 128) {
        return 1;
    } else if(c < 0x800) {
        return 2;
    } else if(c < 0x10000) {
        if(c < 0xD800 || (c > 0xDFFF)) {
            return 3;
        } else {
            return -1;
        }
    } else if(c <= 0x10FFFF) {
        return 4;
    } else {
        return -1;
    }
}

static inline int
utf8_esc_len(int c)
{
    if(c < 0x10000) {
        return 6;
    } else if(c <= 0x10FFFF) {
        return 12;
    } else {
        return -1;
    }
}

static inline int
utf8_to_unicode(unsigned char* buf, size_t size)
{
    int ret;
    if((buf[0] & 0x80) == 0x00) {
        // 0xxxxxxx
        ret = buf[0];
    } else if((buf[0] & 0xE0) == 0xC0 && size >= 2) {
        // 110xxxxy 10yyyyyy
        ret = ((buf[0] & 0x1F) << 6)
            | ((buf[1] & 0x3F));
    } else if((buf[0] & 0xF0) == 0xE0 && size >= 3) {
        // 1110xxxx 10xyyyyy 10yyyyyy
        ret = ((buf[0] & 0x0F) << 12)
            | ((buf[1] & 0x3F) << 6)
            | ((buf[2] & 0x3F));
        if(ret >= 0xD800 && ret <= 0xDFFF) {
            ret = -1;
        }
    } else if((buf[0] & 0xF8) == 0xF0 && size >= 4) {
        // 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
        ret = ((buf[0] & 0x07) << 18)
            | ((buf[1] & 0x3F) << 12)
            | ((buf[2] & 0x3F) << 6)
            | ((buf[3] & 0x3F));
    } else {
        ret = -1;
    }
    return ret;
}

// Lead-byte length table
//   0     : invalid lead byte, len 2 overlongs, > U+10FFFF [F5..FF]
//   1     : ASCII
//   2,3,4 : lead byte of that sequence
//
// Note: we mark some overlongs here right away (2 char ones and greater than
// F4). Below we'll only need continuation byte checks and two boundary checks
// (b0 and b1) for overlong 3 and 4, surrogate 3 and over-long 4.
//
// The idea is originally from https://arxiv.org/pdf/2010.03090 "Validating
// UTF-8 In Less Than One Instruction Per Byte" with an accompanying blog
// https://lemire.me/blog/2020/10/20/ridiculously-fast-unicode-utf-8-validation
//
static const unsigned char utf8_seq_len[256] = {
    /* 0x00 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x10 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x20 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x30 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x40 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x50 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x60 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x70 */ 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
    /* 0x80 */ 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    /* 0x90 */ 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    /* 0xA0 */ 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    /* 0xB0 */ 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    /* 0xC0 */ 0,0,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,
    /* 0xD0 */ 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,
    /* 0xE0 */ 3,3,3,3,3,3,3,3, 3,3,3,3,3,3,3,3,
    /* 0xF0 */ 4,4,4,4,4,0,0,0, 0,0,0,0,0,0,0,0
    /*         0 1 2 3 4 5 6 7  8 9 A B C D E F */
};

static inline size_t
utf8_validate(const unsigned char* JIFFY_RESTRICT data, size_t size)
{
    unsigned int b0 = data[0];
    unsigned int len = utf8_seq_len[b0];
    if(len == 0 || len > size) {
        return 0;
    }
    if(len == 1) {
        // We should never get here from dec_string since we pre-filter bytes < 0x80.
        return 1; // LCOV_EXCL_LINE
    }

    // This is the tricky bit: pack the 1 to 3 continuation bytes into an
    // unsigned int then do masked compares on all at the same time. I saw this
    // the first time in yyjson library.
    unsigned int cont = (unsigned int)data[1];
    if(len >= 3) {
        cont |= (unsigned int)data[2] << 8;
    }
    if(len == 4) {
        cont |= (unsigned int)data[3] << 16;
    }
    static const unsigned int CONT_MASK[5]   = {0, 0, 0x0000C0, 0x00C0C0, 0xC0C0C0};
    static const unsigned int CONT_EXPECT[5] = {0, 0, 0x000080, 0x008080, 0x808080};
    if((cont & CONT_MASK[len]) != CONT_EXPECT[len]) {
        return 0;
    }

    if(len == 2) {
        return 2;
    }

    // Boundary checks based on b0 and b1. The length table has already
    // filtered overlong 2-byte leads and leads > F4. (See table note)
    unsigned int b1 = cont & 0xFF;
    if(len == 3) {
        // E0 80..9F is overlong 3-byte
        if((b0 == 0xE0) & (b1 < 0xA0)) {
            return 0;
        }
        // ED A0..BF is a surrogate.
        if((b0 == 0xED) & (b1 >= 0xA0)) {
            return 0;
        }
        return 3;
    }
    // F0 80..8F overlong 4-byte
    if((b0 == 0xF0) & (b1 < 0x90)) {
        return 0;
    }
    // F4 90..BF these are invalid
    if((b0 == 0xF4) & (b1 >= 0x90)) {
        return 0;
    }
    return 4;
}

static inline int
unicode_to_utf8(int c, unsigned char* buf)
{
    if(c < 0x80) {
        buf[0] = c;
        return 1;
    } else if(c < 0x800) {
        buf[0] = 0xC0 + (c >> 6);
        buf[1] = 0x80 + (c & 0x3F);
        return 2;
    } else if(c < 0x10000) {
        if(c < 0xD800 || (c > 0xDFFF)) {
            buf[0] = 0xE0 + (c >> 12);
            buf[1] = 0x80 + ((c >> 6) & 0x3F);
            buf[2] = 0x80 + (c & 0x3F);
            return 3;
        } else {
            return -1;
        }
    } else if(c <= 0x10FFFF) {
        buf[0] = 0xF0 + (c >> 18);
        buf[1] = 0x80 + ((c >> 12) & 0x3F);
        buf[2] = 0x80 + ((c >> 6) & 0x3F);
        buf[3] = 0x80 + (c & 0x3F);
        return 4;
    }
    return -1;
}

static inline int
unicode_from_pair(int hi, int lo)
{
    if(hi < 0xD800 || hi >= 0xDC00) return -1;
    if(lo < 0xDC00 || lo > 0xDFFF) return -1;
    return ((hi & 0x3FF) << 10) + (lo & 0x3FF) + 0x10000;
}

static inline int
unicode_uescape(int val, unsigned char* p)
{
    int n;
    if(val < 0x10000) {
        p[0] = '\\';
        p[1] = 'u';
        if(int_to_hex(val, p+2) < 0) {
            return -1;
        }
        return 6;
    } else if (val <= 0x10FFFF) {
        n = val - 0x10000;
        p[0] = '\\';
        p[1] = 'u';
        if(int_to_hex((0xD800 | ((n >> 10) & 0x03FF)), p+2) < 0) {
            return -1;
        }
        p[6] = '\\';
        p[7] = 'u';
        if(int_to_hex((0xDC00 | (n & 0x03FF)), p+8) < 0) {
            return -1;
        }
        return 12;
    }
    return -1;
}

#endif // JIFFY_UTF8_H
