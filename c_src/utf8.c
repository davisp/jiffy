// This file is part of Jiffy released under the MIT license. 
// See the LICENSE file for more information.

static const char hexvals[256] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

static const char hexdigits[16] = {
    '0', '1', '2', '3',
    '4', '5', '6', '7',
    '8', '9', 'A', 'B',
    'C', 'D', 'E', 'F'
};

int
int_from_hex(const unsigned char* p)
{
    unsigned char* h = (unsigned char*) p;
    int ret;

    if(hexvals[*(h+0)] < 0) return -1;
    if(hexvals[*(h+1)] < 0) return -1;
    if(hexvals[*(h+2)] < 0) return -1;
    if(hexvals[*(h+3)] < 0) return -1;

    ret = (hexvals[*(h+0)] << 12)
        + (hexvals[*(h+1)] << 8)
        + (hexvals[*(h+2)] << 4)
        + (hexvals[*(h+3)] << 0);

    return ret;
}

int
int_to_hex(int val, char* p)
{
    if(val < 0 || val > 65535)
        return 0;

    p[0] = hexdigits[(val >> 12) & 0xF];
    p[1] = hexdigits[(val >> 8) & 0xF];
    p[2] = hexdigits[(val >> 4) & 0xF];
    p[3] = hexdigits[val & 0xF];

    return 1;
}

int
utf8_len(int c)
{
    if(c < 128) {
        return 1;
    } else if(c < 0x800) {
        return 2;
    } else if(c < 0x10000) {
        if(c < 0xD800 || (c > 0xDFFF && c < 0xFFFE)) {
            return 3;
        } else {
            return -1;
        }
    } else if(c < 0x200000) {
        return 4;
    } else if(c < 0x4000000) {
        return 5;
    } else if(c < 0x80000000) {
        return 6;
    } else {
        return -1;
    }
}

int
utf8_from_pair(int hi, int lo)
{
    if(hi < 0xD800 || hi >= 0xDC00) return -1;
    if(lo < 0xDC00 || lo > 0xDFFF) return -1;
    return ((hi & 0x3FF) << 10) + (lo & 0x3FF) + 0x10000;
}

int
utf8_to_binary(int c, unsigned char* buf)
{
    if(c < 0x80) {
        buf[0] = (unsigned char) c;
        return 1;
    } else if(c < 0x800) {
        buf[0] = (unsigned char) 0xC0 + (c >> 6);
        buf[1] = (unsigned char) 0x80 + (c & 0x3F);
        return 2;
    } else if(c < 0x10000) {
        if(c < 0xD800 || (c > 0xDFFF && c < 0xFFFE)) {
            buf[0] = (unsigned char) 0xE0 + (c >> 12);
            buf[1] = (unsigned char) 0x80 + ((c >> 6) & 0x3F);
            buf[2] = (unsigned char) 0x80 + (c & 0x3F);
            return 3;
        } else {
            return -1;
        }
    } else if(c < 0x200000) {
        buf[0] = (unsigned char) 0xF0 + (c >> 18);
        buf[1] = (unsigned char) 0x80 + ((c >> 12) & 0x3F);
        buf[2] = (unsigned char) 0x80 + ((c >> 6) & 0x3F);
        buf[3] = (unsigned char) 0x80 + (c & 0x3F);
        return 4;
    } else if(c < 0x4000000) {
        buf[0] = (unsigned char) 0xF8 + (c >> 24);
        buf[1] = (unsigned char) 0x80 + ((c >> 18) & 0x3F);
        buf[2] = (unsigned char) 0x80 + ((c >> 12) & 0x3F);
        buf[3] = (unsigned char) 0x80 + ((c >> 6) & 0x3F);
        buf[4] = (unsigned char) 0x80 + (c & 0x3F);
        return 5;
    } else if(c < 0x80000000) {
        buf[0] = (unsigned char) 0xFC + (c >> 30);
        buf[1] = (unsigned char) 0x80 + ((c >> 24) & 0x3F);
        buf[2] = (unsigned char) 0x80 + ((c >> 18) & 0x3F);
        buf[3] = (unsigned char) 0x80 + ((c >> 12) & 0x3F);
        buf[4] = (unsigned char) 0x80 + ((c >> 6) & 0x3F);
        buf[5] = (unsigned char) 0x80 + (c & 0x3F);
        return 6;
    }
    return -1;
}

