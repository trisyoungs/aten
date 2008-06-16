/*
	*** Qt treewidget item
	*** src/gui/ttreewidgetitem.h
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

#ifndef ATEN_TTREEWIDGETATOM_H
#define ATEN_TTREEWIDGETATOM_H

#include <QtGui/QTreeWidget>

// Atom data columns
enum twa_column { TW_A_ID=1, TW_A_ELEMENT, TW_A_RX, TW_A_RY, TW_A_RZ };

// FFAtom data columns
enum twffa_column { TW_FFA_NAME=0, TW_FFA_DESCRIPTION };

// Forward Declarations
class Atom;
class ForcefieldAtom;
class Pattern;

// Custom QTreeWidgetItem
class TTreeWidgetItem : public QTreeWidgetItem
{
	public:
	// Constructors
	TTreeWidgetItem(QTreeWidgetItem *parent);
	TTreeWidgetItem(QTreeWidget *parent);

	/*
	// Pointers
	*/
	private:
	Atom *atom_;
	ForcefieldAtom *forcefieldAtom_;
	Pattern *pattern_;

	public:
	// Set the atom pointer in the widget
	void setAtom(Atom *source);
	// Return the atom pointer in the widget
	Atom *atom();
	// Set the ForcefieldAtom pointer in the widget
	void setForcefieldAtom(ForcefieldAtom *source);
	// Return the ForcefieldAtom pointer in the widget
	ForcefieldAtom *forcefieldAtom();
	// Set the pattern pointer in the widget
	void setPattern(Pattern *source);
	// Return the pattern pointer in the widget
	Pattern *pattern();

	/*
	// Set Data functions
	*/
	public:
	// Set column data from atom pointer
	void setAtomColumns();
	// Set column data from ForcefieldAtom pointer
	void setForcefieldAtomColumns();
};

#endif

