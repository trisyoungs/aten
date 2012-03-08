/*
	*** Choice Class
	*** src/base/choice.h
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

#ifndef ATEN_CHOICE_H
#define ATEN_CHOICE_H

// Choice Variable
class Choice
{
	public:
	// Choice enum
	enum ChoiceType { No, Yes, Default };
	// Constructors
	Choice();
	Choice(Choice::ChoiceType);
	Choice(bool);


	/*
	// Data
	*/
	private:
	// Choice value
	int choice_;
	
	public:
	// Resolve data to supplied value (if current choice_ == Default)
	void resolve(bool defaultvalue);


	/*
	// Operators
	*/
	public:
	// Assignment
	void operator=(bool b);
	void operator=(Choice::ChoiceType ct);
	// Equality
	bool operator==(Choice::ChoiceType ct);
	bool operator==(bool b);
	// Inequality
	bool operator!=(Choice::ChoiceType ct);
	bool operator!=(bool b);
	// Conversion
	operator bool();
};

#endif

