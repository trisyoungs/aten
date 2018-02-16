/*
	*** Symmetry generator definition
	*** src/sg/generator.cpp
	Copyright T. Youngs 2007-2018

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

#include "sg/generator.h"
#include "base/lineparser.h"
#include "base/sysfunc.h"
#include "sg/sginfo.h"
#include <string.h>

ATEN_USING_NAMESPACE

// Constructor
Generator::Generator() : ListItem<Generator>()
{
}

// Set partial element of matrix or translation vector
void Generator::setMatrixPart(int row, QString part)
{
	// The string provided either contains (-)xyz, or a translation amount
	int pos = 0;
	int multiplier = 0;

	// Check for plus/minus signs
	if (part.at(0) == '-') multiplier = -1;
	else if (part.at(0) == '+') multiplier = 1;

	// Skip to next character if necessary
	if (multiplier == 0) multiplier = 1;
	else ++pos;

	//printf("MULTIPLIER = %i, original=[%s], now=[%s]\n", multiplier, s, c);
	// Now, check if this character is x, y, or z.
	if (part.at(pos).isUpper()) matrix_[(part.at(pos).toLatin1()-88)*4+row] = multiplier;
	else if (part.at(pos).isLower()) matrix_[(part.at(pos).toLatin1()-120)*4+row] = multiplier;
	else
	{
		// Must be a number, and hence part of the translation vector
		QStringList parts = part.split("/");
		if (parts.count() != 2) Messenger::print("Generator::setMatrixPart() - Mangled Generator matrix passed (%s).", qPrintable(part));
		else matrix_[12+row] = parts.at(0).toDouble() / parts.at(1).toDouble();
	}
}

// Set from plain text string
bool Generator::set(QString xyzName)
{
	Messenger::enter("Generator::set");

	LineParser lp;
	QString part, sub;
	int n, count;
	const char* c;
	
	// Split line into three arguments
	lp.getArgsDelim(0, xyzName);
	if (lp.nArgs() != 3)
	{
		Messenger::print("Tried to set a symmetry Generator from text ('%s') that didn't split into three arguments.", qPrintable(xyzName));
		Messenger::exit("Generator::set");
		return false;
	}
	
	// Clear any existing data
	matrix_.zero();
	
	// Loop over arguments (equivalent to rows in matrix) and set parameters
	for (n=0; n<3; ++n)
	{
		// Copy parser argument into temporary string
		part = lp.argc(n);
		sub.clear();

		// Step through characters in 'part', adding until we find a (second) 'delimiting' character
		for (int i = 0; i < part.count(); ++i)
		{
			// Store old sub-part if we find a new delimiting character, and sub is not empty
			if ( ( (part.at(i) == '-') || (part.at(i) == '+') ) && (!sub.isEmpty()))
			{
				// This constitutes a sub-part of the string
				setMatrixPart(n, sub);
				sub.clear();
			}
			sub += part.at(i);
		}

		// Check for remaining 'item'
		if (!sub.isEmpty()) setMatrixPart(n, sub);
	}

	Messenger::exit("Generator::set");
	return true;
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
QString Generator::name() const
{
	return name_;
}

// Return matrix of generator
Matrix& Generator::matrix()
{
	return matrix_;
}
