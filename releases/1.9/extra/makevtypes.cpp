#include <math.h>
#include <stdio.h>

int main(int nargs, char** args)
{
	const int nTypes = 12;
	const char* types[nTypes] = { "Int", "Dbl", "Str", "Vec", "Mat", "Ptr", "IntA", "DblA", "StrA", "VecA", "MatA", "PtrA" };
	int n, m, values[nTypes];
	for (n=0; n<nTypes; ++n) values[n] = pow(2,n);

	// Single types first...
	for (n=0; n<nTypes; ++n) printf("%s = %i, ", types[n], values[n]);

	// Pairs of types...
	for (n=0; n<nTypes; ++n)
	{
		for (m=0; m<nTypes; ++m) printf("%s%s = %i, ", types[n], types[m], values[n]+(values[m] << nTypes));
	}
	
	return 0;
}
