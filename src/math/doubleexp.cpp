/*
	*** Double/Exponent value
	*** src/math/doubleexp.cpp
	Copyright T. Youngs 2013-2017

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

#include "math/doubleexp.h"
#include <QStringList>
#include <math.h>
#include <stdio.h>
#include <limits>

ATEN_USING_NAMESPACE

// Constructors
DoubleExp::DoubleExp()
{
	mantissa_ = 0.0;
	exponent_ = 0.0;
	recalculate();
}
DoubleExp::DoubleExp(double value)
{
	set(value);
	recalculate();
}
DoubleExp::DoubleExp(double mantissa, int exponent)
{
	mantissa_ = mantissa;
	exponent_ = exponent;
	recalculate();
}

/*
 * Operators
 */

// Assignment operator
DoubleExp& DoubleExp::operator=(double d)
{
	set(d);

	return *this;
}

// Equality Operator
bool DoubleExp::operator==(const double other)
{
	// Create temporary DoubleExp from the other value
	DoubleExp otherValue(other);
	if (otherValue.exponent_ != exponent_) return false;
	if (fabs(otherValue.mantissa_ - mantissa_) > 1.0e-8) return false;

	return true;
}

// Inequality Operator
bool DoubleExp::operator!=(const double other)
{
	// Create temporary DoubleExp from the other value
	DoubleExp otherValue(other);
	if (otherValue.exponent_ != exponent_) return true;
	if (fabs(otherValue.mantissa_ - mantissa_) > 1.0e-8) return true;

	return false;
}

/*
 * Functions
 */

// Recalculate value
void DoubleExp::recalculate()
{
	value_ = mantissa_ * pow(10.0,exponent_);
}

// Retrieve full, real value
double DoubleExp::value() const
{
	return value_;
}

// Retrieve text representation of value
QString DoubleExp::text(int precision)
{
	QString text;

	// First, create mantissa part based on number of decimals supplied
	text = QString::number(mantissa_, 'f', precision);

	// Second, add on exponential part (if non-zero)
	if (exponent_ != 0) text += "e" + QString::number(exponent_);

	return text;
}

// Set mantissa and exponent
void DoubleExp::set(double mantissa, int exponent)
{
	mantissa_ = mantissa;
	exponent_ = exponent;
	recalculate();
}

// Set from normal value
void DoubleExp::set(double value)
{
	if (fabs(value) > std::numeric_limits<double>::min()) exponent_ = floor(log10(fabs(value)));
	else exponent_ = 0;
	mantissa_ = value / pow(10.0,exponent_);
	recalculate();
// 	printf("Input value %f gives mantissa of %f and exponent of %i\n", value, mantissa_, exponent_);
}

// Set from supplied text
void DoubleExp::set(QString text)
{
	// Does the text contain an exponent part?
	if (text.contains('e', Qt::CaseInsensitive))
	{
		// Get mantissa / exponential parts of string
		QStringList parts = text.split('e', QString::SkipEmptyParts, Qt::CaseInsensitive);
		mantissa_ = parts.at(0).toDouble();
		exponent_ = parts.at(1).toInt();
	}
	else
	{
		exponent_ = 0;
		mantissa_ = text.toDouble();
	}
	recalculate();
}

// Set mantissa
void DoubleExp::setMantissa(double mantissa)
{
	mantissa_ = mantissa;
	recalculate();
}

// Return mantissa
double DoubleExp::mantissa() const
{
	return mantissa_;
}

// Set exponent alone
void DoubleExp::setExponent(int exponent)
{
	exponent_ = exponent;
	recalculate();
}

// Return exponent
int DoubleExp::exponent() const
{
	return exponent_;
}

/*
 * Static Member Functions
 */

// Return text representation of supplied value
QString DoubleExp::text(double value, int precision)
{
	DoubleExp tempValue = value;
	return tempValue.text(precision);
}
