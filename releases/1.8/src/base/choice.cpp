/*
	*** Choice Class
	*** src/base/choice.cpp
	Copyright T. Youngs 2007-2013

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

#include "base/choice.h"
#include "base/constants.h"
#include <stdio.h>

// Constructor
Choice::Choice()
{
	choice_ = Choice::No;
}
Choice::Choice(Choice::ChoiceType ct)
{
	choice_ = ct;
}
Choice::Choice(bool b)
{
	choice_ = b;
}

/*
// Data
*/

// Resolve data to supplied value (if current choice_ == Default)
void Choice::resolve(bool defaultvalue)
{
	if (choice_ == Choice::Default) choice_ = (defaultvalue ? Choice::Yes : Choice::No);
}

/*
// Operators
*/

// Assignment (enum)
void Choice::operator=(Choice::ChoiceType ct)
{
	choice_ = ct;
}

// Assignment (bool)
void Choice::operator=(bool b)
{
	choice_ = (b ? Choice::Yes : Choice::No);
}

// Equality (bool)
bool Choice::operator==(bool b)
{
	if (choice_ == Choice::Default)
	{
		printf("Warning - Choice value has not been resolved from 'Default'. Equating to FALSE.\n");
		return FALSE;
	}
	return (b == (choice_ == Choice::Yes ? TRUE : FALSE));
}

// Equality (Choice)
bool Choice::operator==(Choice::ChoiceType ct)
{
	return (choice_ == ct);
}

// Inequivalence
bool Choice::operator!=(bool b)
{
	if (choice_ == Choice::Default)
	{
		printf("Warning - Choice value has not been resolved from 'Default'. (In)Equating to FALSE.\n");
		return FALSE;
	}
	return (b != (choice_ == Choice::Yes ? TRUE : FALSE));
}

// Inequality (Choice)
bool Choice::operator!=(Choice::ChoiceType ct)
{
	return (choice_ != ct);
}

// Conversion
Choice::operator bool()
{
	if (choice_ == Choice::Default)
	{
		printf("Warning - Choice value has not been resolved from 'Default'. Converting to FALSE.\n");
		return FALSE;
	}
	return (choice_ == Choice::Yes ? TRUE : FALSE);
}
