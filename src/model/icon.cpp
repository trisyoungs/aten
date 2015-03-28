/*
	*** Model icon functions
	*** src/model/icon.cpp
	Copyright T. Youngs 2007-2015

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

#include "model/model.h"

ATEN_USING_NAMESPACE

// Return whether icon is currently valid
bool Model::iconIsValid()
{
	return (iconPoint_ == changeLog_.log(Log::Structure));  // ATEN2 TODO Make a general class to allow more than one log quantity to be compared?
}

// Set icon
void Model::setIcon(QIcon icon)
{
	icon_ = icon;
}

// Return icon
QIcon& Model::icon()
{
	return icon_;
}