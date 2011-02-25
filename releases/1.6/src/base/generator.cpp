/*
	*** Symmetry generator definition
	*** src/base/generator.cpp
	Copyright T. Youngs 2007-2011

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
	lp.getArgsDelim(LineParser::Defaults, s);
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
bool Generator::setMatrixPart(int col, const char *s)
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
	if ( (*c >= 88) && (*c <= 90) ) matrix_[col*4 + (*c-88)] = multiplier;     // BROKEN?
	else if ( (*c >= 120) && (*c <= 122) ) matrix_[col*4 + (*c-120)] = multiplier;
	else
	{
		// Must be a number....
		int num = atoi(s);
//  		printf("Translation integer is %i.\n", num);
		matrix_[12+col] = num / (1.0*STBF);
// 		translation.set(row, value);
	}
	return TRUE;
}

// Set rotation matrix col
void Generator::setRotation(int col, double x, double y, double z)
{
	if ((col < 0) || (col > 3)) printf("Generator: Rotation matrix column %i is out of range.\n", col);
	else matrix_.setColumn(col, x, y, z, matrix_[col*4+3]);
}

// Set translation column
void Generator::setTranslation(double tx, double ty, double tz, double divisor)
{
	matrix_[12] = tx/divisor;
	matrix_[13] = ty/divisor;
	matrix_[14] = tz/divisor;
// 	matrix_.print();
}

// Return name of generator
const char *Generator::name() const
{
	return name_.get();
}

// Return matrix of generator
Matrix &Generator::matrix()
{
	return matrix_;
}
