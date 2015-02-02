/*
	*** Qt Layout List
	*** src/gui/layoutlist.cpp
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

#include "gui/layoutlist.h"
#include <string.h>
#include "QtGui/QGridLayout"
#include "QtGui/QWidget"

/*
// LayoutData class
*/

// Constructor
LayoutData::LayoutData() : ListItem<LayoutData>()
{
	// Private variables
	layout_ = NULL;
	row_ = 0;
	column_ = 0;
}

// Set associated name of layout
void LayoutData::setName(const char *name)
{
	name_ = name;
}

// Return associated name of layout
const char *LayoutData::name()
{
	return name_.get();
}

// Set layout pointer
void LayoutData::setLayout(QGridLayout *layout)
{
	layout_ = layout;
}

// Return layout pointer
QGridLayout *LayoutData::layout()
{
	return layout_;
}

// Return current column number (and increase)
int LayoutData::column(int span)
{
	int old = column_;
	column_ += span;
	return old;
}

// Return current row number (moving to next line if flagged)
int LayoutData::row(bool newline)
{
	// If 'newline' is flagged, skip to next row (but only if current column is nonzero)
	if (newline && (column_ != 0))
	{
		row_++;
		column_ = 0;
	}
	return row_;
}

// Add a widget to the contained layout
void LayoutData::addWidget(QWidget *w, int colspan, bool newline)
{
	if (layout_ == NULL)
	{
		printf("Internal Error: No layout pointer exists in this LayoutData.\n");
		return;
	}
	int r = row(newline);
	int c = column(colspan);
	layout_->addWidget(w, r, c, 1, colspan);
}

/*
// Layout list
*/

// Constructor
LayoutList::LayoutList()
{
}


// Clear list
void LayoutList::clear()
{
	layouts_.clear();
}

// Add a new item to the list
LayoutData *LayoutList::add(const char *name, QGridLayout *layout)
{
	LayoutData *ld = layouts_.add();
	ld->setName(name);
	ld->setLayout(layout);
	return ld;
}

// Find existing layout
LayoutData *LayoutList::find(const char *name)
{
	LayoutData *ld;
	for (ld = layouts_.first(); ld != NULL; ld = ld->next) if (strcmp(name, ld->name()) == 0) break;
	return ld;
}
