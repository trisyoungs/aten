/*
	*** TSpaceSaver Functions
	*** src/gui/tpopupwidget_funcs.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/tspacesaver.hui"

// Constructor
TSpaceSaver::TSpaceSaver(QWidget* parent) : QWidget(parent)
{
	spaceSavers_.add(this);
}

/*
 * Protected Functions
 */


/*
 * Public
 */

/*
 * Virtual Reimplementations
 */

/*
 * Internal Data
 */

// Begin monitoring and adjusting widgets according to available space
static void TSpaceSaver::beginSpaceSaving()
{
	// Loop over space savers and set internal pointers
	for (RefListItem<TSpaceSaver,int>* ri = spaceSavers_.first(); ri != NULL; ri = ri->next)
	{
		TSpaceSaver* spaceSaver = ri->item;

		// Get layout for this space saver
		QLayout* layout = spaceSaver->layout();
		if (layout == NULL)
		{
			Messenger::error("Error: No layout found for this space saver.");
			continue;
		}
	}
}
