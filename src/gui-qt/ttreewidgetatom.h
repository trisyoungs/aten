/*
	*** Qt treewidget atom item
	*** src/gui-qt/ttreewidgetatom.h
	Copyright T. Youngs 2007

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

#ifndef H_TTREEWIDGETATOM_H
#define H_TTREEWIDGETATOM_H

#include <QtGui/QTreeWidget>

// Enum detailing atom data columns
enum twa_column { TWA_ID=1, TWA_ELEMENT, TWA_RX, TWA_RY, TWA_RZ };

// Forward Declarations
class atom;

class TTreeWidgetAtom : public QTreeWidgetItem
{
	public:
	// Constructor
	TTreeWidgetAtom(QTreeWidgetItem *parent = 0);

	/*
	// Atom Pointer
	*/
	private:
	atom *i;

	public:
	// Set the atom pointer in the widget
	void set_atom(atom *source) { i = source; }
	// Return the atom pointer in the widget
	atom *get_atom() { return i; }

	/*
	// Set Data functions
	*/
	public:
	void set_columns();
};

#endif

