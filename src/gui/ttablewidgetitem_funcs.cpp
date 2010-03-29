/*
	*** Qt TTableWidgetItem functions
	*** src/gui/ttablewidgetitem_funcs.cpp
	Copyright T. Youngs 2007-2010

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

#include "gui/ttablewidgetitem.h"

// Constructor
TTableWidgetItem::TTableWidgetItem() : QTableWidgetItem()
{
	// Private variables
	model_ = NULL;
}

// Set the model pointer in the widget
void TTableWidgetItem::setModel(Model *source)
{
	model_ = source;
}

// Return the grid pointer in the widget
Model *TTableWidgetItem::model()
{
	return model_;
}

