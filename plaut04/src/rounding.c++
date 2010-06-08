#include "rounding.h"
#include <math.h>

double round(bool key, double value, int digits)
{
	double tmp;
	tmp = pow(10.0, digits);
	if(key)
		return floor(value/tmp)*tmp;
	
	return ceil(value/tmp)*tmp;
}

void rounding(double& amax, double& amin)
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
//	cout <<" x1 = "<<amax<<"  x2 = "<<amin<<endl;
	edif = log10(amax/amin);
//	cout <<" edif = "<<edif<<endl;

	// edif (- [0, unlimit)

	if((int)floor(edif) >= 0)
	{
	// a.x 1.3E-8, 9.2E3
		adif = (int)floor( log10(fabs(amax)))-1;
		amax = round(false, amax, adif);
		
//		adif = (int)floor( log10(abs(amin)))-1;
		if(token<0) amin = round(false, amin, adif);
		else amin = round(true, amin, adif);
//	cout <<" 1 xxma1 = "<<amax<<"  xmin2 = "<<amin<<endl;
	}
	else 
	{ // edif == 0  same magnitude
		adif = (int)floor( log10( amax - amin ))-1; 
		amax = round(false, amax, adif);
//		adif = (int)floor( log10(abs(amin)))-1;
		amin = round(true, amin, adif);
//	cout <<" 2 xxma1 = "<<amax<<"  xmin2 = "<<amin<<endl;
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
//	cout <<" 3 xxma1 = "<<amax<<"  xmin2 = "<<amin<<endl;
}

