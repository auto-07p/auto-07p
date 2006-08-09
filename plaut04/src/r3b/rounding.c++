#include <math.h>

////////////////////////////////////////////////
//
double round(bool key, double value, int digits)
//
////////////////////////////////////////////////
{
    double tmp;
    tmp = pow(10.0, digits);
    if(key)
        return floor(value/tmp)*tmp;

    return ceil(value/tmp)*tmp;
}


////////////////////////////////////////////////
//
void rounding(double& amax, double& amin)
//
////////////////////////////////////////////////
{
    double edif, tmp;
    int adif;
    int isswap = 0;
    int token = 1;

    if(amin ==0 && amax == 0) return;

    if(amin < 0 )
    {
        if(amax < 0)
        {
            tmp = -amax;
            amax = -amin;
            amin = tmp;
            isswap = 1;
        }
        else
        {
            amin = -amin;
            isswap = 2;
            if(amax < amin)
            {
                tmp = amax;
                amax = amin;
                amin = tmp;
                isswap = 3;
                token = -1;
            }
        }
    }
    edif = log10(amax/amin);

    if((int)floor(edif) >= 0)
    {
        adif = (int)floor( log10(fabs(amax)))-1;
        amax = round(false, amax, adif);

        if(token<0) amin = round(false, amin, adif);
        else amin = round(true, amin, adif);
    }
    else
    {
        adif = (int)floor( log10( amax - amin ))-1;
        amax = round(false, amax, adif);
        amin = round(true, amin, adif);
    }
    if(isswap == 1)
    {
        tmp = -amax;
        amax = -amin;
        amin = tmp;
    }
    else if(isswap == 2)
    {
        amin = - amin;
    }
    else if(isswap == 3)
    {
        tmp = -amax;
        amax = amin;
        amin = tmp;
    }
}
