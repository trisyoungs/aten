/*
	*** Symmetry generator definition
	*** src/base/generator.cpp
	Copyright T. Youngs 2007-2012

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

// Set partial element of matrix or translation vector
void Generator::setMatrixPart(int row, const char *s)
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
	if ( (*c >= 88) && (*c <= 90) ) matrix_[(*c-88)*4+row] = multiplier;
	else if ( (*c >= 120) && (*c <= 122) ) matrix_[(*c-120)*4+row] = multiplier;
	else
	{
		// Must be a number, and hence part of the translation vector
		int num = atoi(beforeChar(s,'/')), denom = atoi(afterChar(s,'/'));
		double t = (double) num / (double) denom;
		matrix_[12+row] = t;
	}
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
	lp.getArgsDelim(0, s);
	if (lp.nArgs() != 3)
	{
		msg.print("Tried to set a symmetry Generator from text ('%s') that didn't split into three arguments.\n", s);
		msg.exit("Generator::set");
		return FALSE;
	}
	
	// Clear any existing data
	matrix_.zero();
	
	// Loop over arguments (equivalent to rows in matrix) and set parameters
	for (n=0; n<3; ++n)
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
			++count;
			++c;
		}
		// Check for remaining 'item'
		if (count != 0)
		{
			sub[count] = '\0';
			setMatrixPart(n, sub);
		}
	}

	msg.exit("Generator::set");
	return TRUE;
}

// Set rotation matrix row (not including translation vector)
void Generator::setRotationRow(int row, double x, double y, double z)
{
	matrix_.setRow(row, x, y, z);
}

// Set translation column
void Generator::setTranslation(double tx, double ty, double tz, double divisor)
{
	matrix_[12] = tx/divisor;
	matrix_[13] = ty/divisor;
	matrix_[14] = tz/divisor;
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
