#include <string.h>

#include "double-conversion.h"


using namespace double_conversion;


#ifdef __cplusplus
extern "C" {
#endif

// Returns the length of the string
int
double_to_shortest(char *buf, size_t size, double val)
{
    int len = -1;

    StringBuilder builder(buf, size);
    const DoubleToStringConverter& dc =
        DoubleToStringConverter::EcmaScriptConverter();

    dc.ToShortest(val, &builder);
    len = builder.position();
    buf = builder.Finalize();
    return len;
}

#ifdef __cplusplus
}
#endif
