/*
	*** Fragment Model Data
	*** src/base/fragment.h
	Copyright T. Youngs 2007-2010

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ATEN_FRAGMENTDATA_H
#define ATEN_FRAGMENTDATA_H

#include <QtGui/QPixmap>

// Forward Declarations
class Atom;
class Model;

// Fragment Model data
class Fragment
{
	public:
	// Constructor
	Fragment();
	// List pointers
	Fragment *prev, *next;

	private:
	// Model to which the data is relevant
	Model *model_;
	// Link atom, which acts as connection point for fragment
	Atom *linkAtom_;
	// Bond partner for link atom (if any)
	Atom *linkPartner_;
	// QPixmap containing miniature picture of fragment
	QPixmap pixmap_;

	public:
	// Set data from source model
	void setModel(Model *m);
	// Return model pointer
	Model *model();
	// Return link atom
	Atom *linkAtom();
	// Return link atom partner
	Atom *linkPartner();
	// Return pixmap
	QPixmap &pixmap();
};

#endif
