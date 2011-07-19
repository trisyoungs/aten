/*
	*** TTableWidgetItem - QTableWidgetItem with custom data structure
	*** src/gui/ttablewidgetitem.h
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

#ifndef ATEN_TTABLEWIDGETATOM_H
#define ATEN_TTABLEWIDGETATOM_H

#include <QtGui/QTableWidgetItem>
#include "parser/returnvalue.h"

class TTableWidgetItem : public QTableWidgetItem
{
	public:
	// Constructor
	TTableWidgetItem();

	/*
	// Associated Data
	*/
	public:
	// Associated data item
	ReturnValue data;
};

#endif

