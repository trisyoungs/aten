/*
	*** Symmetry generator definition
	*** src/base/generator.cpp
	Copyright T. Youngs 2007-2010

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "base/generator.h"
#include "base/dnchar.h"
#include "base/lineparser.h"
#include "base/sysfunc.h"
#include "base/sginfo.h"
#include <string.h>

// Constructor
Generator::Generator()
{
	// Private variables
	matrix_.set(3, 0.0, 0.0, 0.0, 1.0);

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set from plain text string
bool Generator::set(const char *s)
{
	msg.enter("Generator::set");
	static LineParser lp;
	char part[32], sub[16];
	int n, count;
	const char *c;
	// Split line into three arguments
	lp.getArgsDelim(s, LineParser::Defaults);
	if (lp.nArgs() != 3)
	{
		msg.print("Tried to set a symmetry Generator from text ('%s') that didn't split into three arguments.\n", s);
		msg.exit("Generator::set");
		return FALSE;	
	}
	// Loop over arguments and set parameters
	for (n=0; n<3; n++)
	{
		// Copy parser argument into temporary string
		strcpy(part, lp.argc(n));
		c = &part[0];
		count = 0;
		// Step through characters in 'part', adding until we find a (second) 'delimiting' character
		while (*c != '\0')
		{
			if ( ( (*c == '-') || (*c == '+') ) && (count != 0))
			{
				// This constitutes a sub-part of the string
				sub[count] = '\0';
				setMatrixPart(n, sub);
				count = 0;
			}
			sub[count] = *c;
			count ++;
			c ++;
		}
		// Check for remaining 'item'
		if (count != 0)
		{
			sub[count] = '\0';
			setMatrixPart(n, sub);
		}
		//printf("PART=[%s], SUB=[%s]\n", part, sub);
	}
	msg.exit("Generator::set");
	return TRUE;
}

// Set partial element of matrix or translation vector
bool Generator::setMatrixPart(int row, const char *s)
{
	// The string provided either contains (-)xyz, or a translation amount
	const char *c;
	int multiplier = 0;
	// Check for plus/minus signs
	c = &s[0];
	if (*c == '-') multiplier = -1;
	else if (*c == '+') multiplier = 1;
	// Skip to next character if necessary
	if (multiplier == 0) multiplier = 1;
	else c = &s[1];
	//printf("MULTIPLIER = %i, original=[%s], now=[%s]\n", multiplier, s, c);
	// Now, check if this character is x, y, or z.
	if ( (*c >= 88) && (*c <= 90) ) matrix_.set(row, *c-88, multiplier);
	else if ( (*c >= 120) && (*c <= 122) ) matrix_.set(row, *c-120, multiplier);
	else
	{
		// Must be a number....
		int num = atoi(s);
//  		printf("Translation integer is %i.\n", num);
		matrix_.set(row, 3, num / (1.0*STBF));
// 		translation.set(row, value);
	}
	return TRUE;
}

// Set rotation matrix row
void Generator::setRotation(int row, double x, double y, double z)
{
	if ((row < 0) || (row > 3)) printf("Generator: Rotation matrix row %i is out of range.\n", row);
	else matrix_.rows[row].set(x, y, z, matrix_.get(row,3) );
}

// Set translation column
void Generator::setTranslation(double tx, double ty, double tz, double divisor)
{
	matrix_.set(0,3,tx/divisor);
	matrix_.set(1,3,ty/divisor);
	matrix_.set(2,3,tz/divisor);
// 	matrix_.print();
}

// Return name of generator
const char *Generator::name() const
{
	return name_.get();
}

// Return matrix of generator
Mat4<double> &Generator::matrix()
{
	return matrix_;
}
