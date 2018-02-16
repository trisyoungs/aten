/*
	*** Basic calculable definition
	*** src/methods/calculable.cpp
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

#include "methods/calculable.h"

ATEN_USING_NAMESPACE

// Constructor
Calculable::Calculable() : ListItem<Calculable>()
{
}

// Destructor
Calculable::~Calculable()
{
}

// Set identifiable name of the quantity
void Calculable::setName(QString s)
{
	name_ = s;
}

// Return name of the quantity
QString Calculable::name() const
{
	return name_;
}

// Set filename of the quantity
void Calculable::setFilename(QString filename)
{
	filename_ = filename;
}

// Return filename of the quantity
QString Calculable::filename() const
{
	return filename_;
}
