/*
	*** TListWidgetItem - QListWidgetItem with custom data storage
	*** src/gui/tlistwidgetitem.h
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

#ifndef ATEN_TLISTWIDGETITEM_H
#define ATEN_TLISTWIDGETITEM_H

#include <QtWidgets/QListWidgetItem>
#include "parser/returnvalue.h"

ATEN_USING_NAMESPACE

class TListWidgetItem : public QListWidgetItem
{
	public:
	// Constructor
	TListWidgetItem(QListWidget *parent = 0);

	/*
	// Associated Data
	*/
	public:
	// Associated data item
	ReturnValue data;
};

#endif

