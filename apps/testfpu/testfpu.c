

double vals[] = {1.0, 2.0, 4.0, -1.5, -1.375};

double sum(double * array, int len);


void __attribute__ ((noinline))
print_double(double d)
{
    // yeah, there's likely no vga
    // so just store the value.
    // you are single-stepping this right?
    vals[0] = d;
}

int
main(void)
{
    double result;

    /* test pgflt handler */
    //*(char *)0x123 = 0;

    result = sum(vals, sizeof(vals)/sizeof(double));
    print_double(result);

    return 0;
}

// include this at the end
#include "ozapp.h"
