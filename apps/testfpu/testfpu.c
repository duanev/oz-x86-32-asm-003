

double vals[] = {1.1, 2.2, 3.3};

double sum(double * array, int len);


void __attribute__ ((noinline))
print_double(double d)
{
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
