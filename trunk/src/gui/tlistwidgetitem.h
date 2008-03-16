/*
	*** Qt listwidget item
	*** src/gui/tlistwidgetitem.h
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

#ifndef ATEN_TLISTWIDGETATOM_H
#define ATEN_TLISTWIDGETATOM_H

#include <QtGui/QListWidgetItem>

// Forward Declarations
class forcefield;
class grid;

class TListWidgetItem : public QListWidgetItem
{
	public:
	// Constructor
	TListWidgetItem(QListWidget *parent = 0);

	/*
	// Pointers
	*/
	private:
	forcefield *ff;
	grid *g;
	

	public:
	// Set the ff pointer in the widget
	void set_ff(forcefield *source) { ff = source; }
	// Return the ff pointer in the widget
	forcefield *get_ff() { return ff; }
	// Set the grid pointer in the widget
	void set_grid(grid *source) { g = source; }
	// Return the grid pointer in the widget
	grid *get_grid() { return g; }

};

#endif

