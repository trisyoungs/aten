/*
	*** TTreeWidgetItem - QTreeWidgetItem with custom data class
	*** src/gui/ttreewidgetitem.h
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

#ifndef ATEN_TTREEWIDGETITEM_H
#define ATEN_TTREEWIDGETITEM_H

#include <QtGui/QTreeWidget>
#include "parser/returnvalue.h"

// Custom QTreeWidgetItem
class TTreeWidgetItem : public QTreeWidgetItem
{
	public:
	// Constructors
	TTreeWidgetItem(QTreeWidgetItem *parent);
	TTreeWidgetItem(QTreeWidget *parent);

	/*
	// Associated Data
	*/
	public:
	// Associated data item
	ReturnValue data;
};

#endif

