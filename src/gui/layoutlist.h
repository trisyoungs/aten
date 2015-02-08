/*
	*** Layout List
	*** src/gui/layoutlist.h
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

#ifndef ATEN_LAYOUTLIST_H
#define ATEN_LAYOUTLIST_H

#include "base/dnchar.h"
#include "math/constants.h"
#include "templates/list.h"

// Forward Declarations
class QGridLayout;
class QWidget;

// QGridLayout*/int/int class
class LayoutData : public ListItem<LayoutData>
{
	public:
	// Constructor
	LayoutData();

	private:
	// Associated name of layout (e.g. group name)
	Dnchar name_;
	// Layout pointer
	QGridLayout *layout_;
	// Current row and column in layout
	int row_;
	int column_;

	public:
	// Set associated name of layout
	void setName(const char *name);
	// Return associated name of layout
	const char *name();
	// Set layout pointer
	void setLayout(QGridLayout *layout);
	// Return layout pointer
	QGridLayout *layout();
	// Return current column number (and increase by number specified)
	int column(int span = 1);
	// Return current row number (moving to next line if flagged)
	int row(bool newline = FALSE);
	// Add a widget to the contained layout
	void addWidget(QWidget *w, int colspan, bool newline);
};

// Layout list
class LayoutList
{
	public:
	// Constructor
	LayoutList();

	private:
	// List of layout data
	List<LayoutData> layouts_;

	public:
	// Clear list
	void clear();
	// Add new item to the list
	LayoutData *add(const char *name, QGridLayout *layout);
	// Find existing layout
	LayoutData *find(const char *name);
};

#endif
