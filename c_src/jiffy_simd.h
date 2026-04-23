// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.
//
// Structure the scan-ahead logic so it's both readable and so that Clang and
// GCC will auto-vectorize the code. For the auto-vectorizer we want to have a
// loop with a known bound and no early exit, which is a bit tricky, since the
// whole idea is to exit when we find a "stop" character. To get the best of
// both worlds, scan 32 bytes at a time and then have an outer loop around that
//
// We get auto-vectorization by default with Clang (MacOS 15+) and GCC (14+)
//
// Clang
// ===
// % cc -S -O3 -Rpass='.*vectoriz.*' -I "$ERL_INC" -I c_src  -c c_src/jiffy.c  -o jiffy.s 2>&1 | grep -i -w 'vectorized' | grep simd
// c_src/jiffy_simd.h:22:61: remark: Vectorized horizontal reduction with cost -112 and with tree size 3 [-Rpass=slp-vectorizer]
// c_src/jiffy_simd.h:22:61: remark: Vectorized horizontal reduction with cost -87 and with tree size 3 [-Rpass=slp-vectorizer]
//
// GCC
// ====
// % gcc-14 -S -O3 -fopt-info-vec-all -I "$ERL_INC" -I c_src  -c c_src/jiffy.c -o jiffy.s 2>&1 | grep -i -w 'optimized'| grep simd
// c_src/jiffy_simd.h:21:23: optimized: loop vectorized using 16 byte vectors
// c_src/jiffy_simd.h:21:23: optimized: loop vectorized using 16 byte vectors
// c_src/jiffy_simd.h:21:23: optimized: loop vectorized using 16 byte vectors
//
// If we know we'll deploy on the same architecture we're compiling on we can
// use -march=native and get even better vectorization with 32 byte chunks at a
// time
//
// GCC /w -march=native
// ====
// % gcc-14 -march=native -S -O3 -fopt-info-vec-all -I "$ERL_INC" -I c_src  -c c_src/jiffy.c -o jiffy.s 2>&1 | grep -i -w 'optimized'| grep simd
// c_src/jiffy_simd.h:21:23: optimized: loop vectorized using 32 byte vectors
// c_src/jiffy_simd.h:21:23: optimized: loop vectorized using 32 byte vectors
// c_src/jiffy_simd.h:21:23: optimized: loop vectorized using 32 byte vectors

#ifndef JIFFY_SIMD_H
#define JIFFY_SIMD_H

#define JIFFY_SIMD_BLOCK_SIZE 32

static inline unsigned int
jiffy_block_has_stop(const unsigned char* JIFFY_RESTRICT p)
{
    unsigned int bad = 0;
    for (int i = 0; i < JIFFY_SIMD_BLOCK_SIZE; i++) {
        unsigned char c = p[i];
        bad |= (unsigned)(c < 0x20) | (unsigned)(c >= 0x80) | (unsigned)(c == '"') | (unsigned)(c == '\\');
    }
    return bad;
}

static inline size_t
jiffy_scan_ascii_string_body(const unsigned char* JIFFY_RESTRICT p, size_t len, size_t i)
{
    while (i + JIFFY_SIMD_BLOCK_SIZE <= len && !jiffy_block_has_stop(p + i)) {
        i += JIFFY_SIMD_BLOCK_SIZE;
    }
    while (i < len && p[i] >= 0x20 && p[i] < 0x80 && p[i] != '"' && p[i] != '\\') {
        i++;
    }
    return i;
}

// Variant of the scan which lets UTF-8 multi-byte sequences pass through. This
// so we can scan a whole block and then validate it as a block later.
static inline unsigned int
jiffy_block_has_utf8_stop(const unsigned char* JIFFY_RESTRICT p)
{
    unsigned int bad = 0;
    for (int i = 0; i < JIFFY_SIMD_BLOCK_SIZE; i++) {
        unsigned char c = p[i];
        bad |= (unsigned)(c < 0x20) | (unsigned)(c == '"') | (unsigned)(c == '\\');
    }
    return bad;
}

static inline size_t
jiffy_scan_utf8_string_body(const unsigned char* JIFFY_RESTRICT p, size_t len, size_t i)
{
    while (i + JIFFY_SIMD_BLOCK_SIZE <= len && !jiffy_block_has_utf8_stop(p + i)) {
        i += JIFFY_SIMD_BLOCK_SIZE;
    }
    while (i < len && p[i] >= 0x20 && p[i] != '"' && p[i] != '\\') {
        i++;
    }
    return i;
}

#endif
