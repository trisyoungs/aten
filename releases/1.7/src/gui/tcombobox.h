/*
	*** TTableWidget Class
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

#include <QtGui/QComboBox>

// Forward Declarations
class Model;

class TComboBox : public QComboBox
{
	public:
	// Constructor
	TComboBox(QWidget *parent);

	/*
	// Associated Data
	*/
	private:
	// Pointer data
	void *pointer_;
	// Integer data
	int integer_;

	public:
	// Set the pointer in the widget
	void setPointer(void *ptr);
	// Return the pointer in the widget
	void *pointer();
	// Set the integer value in the widget
	void setInteger(int i);
	// Return the integer value in the widget
	int integer();
};

#endif

