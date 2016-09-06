/*
	*** TDynamicWidget Functions
	*** src/gui/tdynamicwidget_funcs.cpp
	Copyright T. Youngs 2016-2016

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

#include "gui/tdynamicwidget.hui"
#include "gui/tdynamiclayout.hui"
#include "gui/tmenubutton.hui"
#include "base/messenger.h"
#include <QLayout>

// Static singletons
RefList<TDynamicWidget,int> TDynamicWidget::dynamicWidgets_;

// Constructor
TDynamicWidget::TDynamicWidget(QWidget* parent) : QWidget(parent)
{
	dynamicWidgets_.add(this);
}

/*
 * Virtual Reimplementations
 */

void TDynamicWidget::resizeEvent(QResizeEvent* event)
{
}

/*
 * Internal Data
 */

// Begin monitoring and adjusting widgets according to available space
void TDynamicWidget::beginSpaceSaving()
{
	// Loop over space savers and set internal pointers
	for (RefListItem<TDynamicWidget,int>* ri = dynamicWidgets_.first(); ri != NULL; ri = ri->next)
	{
		TDynamicWidget* dynamicWidget = ri->item;

		// Get layout for this space saver
		QLayout* layout = dynamicWidget->layout();
		if (layout == NULL)
		{
			Messenger::error("Error: No layout found for this space saver.");
			continue;
		}

		// Is this a QGridLayout?
		QGridLayout* gridLayout = qobject_cast<QGridLayout*>(layout);

		// Create new dynamic layout for this widget
		TDynamicLayout* dynamicLayout = new TDynamicLayout(layout->margin(), layout->spacing(), layout->spacing());

		// Transfer widgets to new dynamic layout - for grid layouts be careful to preserve widget layout order
		QLayoutItem* item;
		if (gridLayout)
		{
			for (int column = 0; column < gridLayout->columnCount(); ++column)
				for (int row = 0; row < gridLayout->rowCount(); ++row)
				{
					item = gridLayout->itemAtPosition(row, column);
					if (!item) continue;
					int itemIndex = gridLayout->indexOf(item->widget());
					dynamicLayout->addItem(gridLayout->takeAt(itemIndex));
				}
		}
		else while ((item = layout->takeAt(0))) dynamicLayout->addItem(item);

		// Delete the old layout, and apply the new one
		delete layout;
		dynamicWidget->setLayout(dynamicLayout);
	}
}
