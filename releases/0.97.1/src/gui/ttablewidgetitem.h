/*
	*** Qt tablewidget item
	*** src/gui/ttablewidgetitem.h
	Copyright T. Youngs 2007,2008

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

// Forward Declarations
class Model;

class TTableWidgetItem : public QTableWidgetItem
{
	public:
	// Constructor
	TTableWidgetItem();

	/*
	// Pointers
	*/
	private:
	Model *model_;
	

	public:
	// Set the model pointer in the widget
	void setModel(Model *source);
	// Return the grid pointer in the widget
	Model *model();
};

#endif

