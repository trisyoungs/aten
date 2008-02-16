/*
	*** Qt treewidget item
	*** src/gui-qt/ttreewidgetitem.h
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

// Atom data columns
enum twa_column { TW_A_ID=1, TW_A_ELEMENT, TW_A_RX, TW_A_RY, TW_A_RZ };

// FFAtom data columns
enum twffa_column { TW_FFA_NAME=0, TW_FFA_DESCRIPTION };

// Forward Declarations
class atom;
class ffatom;

class TTreeWidgetItem : public QTreeWidgetItem
{
	public:
	// Constructor
	TTreeWidgetItem(QTreeWidgetItem *parent = 0);

	/*
	// Pointers
	*/
	private:
	atom *i;
	ffatom *ffa;
	

	public:
	// Set the atom pointer in the widget
	void set_atom(atom *source) { i = source; }
	// Return the atom pointer in the widget
	atom *get_atom() { return i; }
	// Set the ffatom pointer in the widget
	void set_ffatom(ffatom *source) { ffa = source; }
	// Return the ffatom pointer in the widget
	ffatom *get_ffatom() { return ffa; }

	/*
	// Set Data functions
	*/
	public:
	// Set column data from atom pointer
	void set_atom_columns();
	// Set column data from ffatom pointer
	void set_ffatom_columns();
};

#endif

