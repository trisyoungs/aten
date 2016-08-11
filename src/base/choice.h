/*
	*** Choice Class
	*** src/base/choice.h
	Copyright T. Youngs 2007-2016

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

#ifndef ATEN_CHOICE_H
#define ATEN_CHOICE_H

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Choice Variable
class Choice
{
	public:
	// Choice enum
	enum ChoiceType { No, Yes, Default };
	// Constructors
	Choice();
	Choice(Choice::ChoiceType);


	/*
	 * Data
	 */
	private:
	// Choice value
	int choice_;
	
	public:
	// Resolve data to supplied value (if current choice_ == Default)
	void resolve(bool defaultvalue);


	/*
	 * Operators
	 */
	public:
	// Assignment
	Choice& operator=(AtenSpace::Choice::ChoiceType ct);
	// Equality
	bool operator==(Choice::ChoiceType ct);
	// Inequality
	bool operator!=(Choice::ChoiceType ct);
	// Conversion
	operator bool();
};

ATEN_END_NAMESPACE

#endif

