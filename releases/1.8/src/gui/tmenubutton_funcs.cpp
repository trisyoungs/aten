/*
	*** TMenuButton Functions
	*** src/gui/tmenubutton_funcs.cpp
	Copyright T. Youngs 2007-2013

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

#include "gui/tmenubutton.uih"
#include <QtGui/QMouseEvent>
#include <QtGui/QMenu>
#include <QtGui/QHBoxLayout>

// Constructor
TMenuButton::TMenuButton(QWidget *parent) : QPushButton(parent)
{
	menu_.setFont(font());
	menu_.setPalette(palette());
	QObject::connect(this, SIGNAL(clicked(bool)), this, SLOT(menuButtonPressed(bool)));
}

// Add item to menu
bool TMenuButton::addMenuItem(const char *text, int id)
{
	// Search for existing item with same ID
	if (actions_.containsData(id))
	{
		printf("TMenuButton::addMenuItem - id '%i' is already in use.\n", id);
		return FALSE;
	}
	QAction *action = menu_.addAction(text);
	actions_.add(action, id);
	QObject::connect(action, SIGNAL(triggered(bool)), this, SLOT(menuActionTriggered(bool)));
	return TRUE;
}

void TMenuButton::menuActionTriggered(bool checked)
{
	// Cast sender and determine which action sent the signal
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("TMenuButton::menuActionTriggered - Couldn't cast sender into QAction.\n");
		return;
	}
	// Search stored list of QActions
	for (Refitem<QAction,int> *ri = actions_.first(); ri != NULL; ri = ri->next)
	{
		if (action == ri->item)
		{
			emit(menuItemClicked(ri->data));
			return;
		}
	}
	printf("TMenuButton::menuActionTriggered - Couldn't find QAction in list.\n");
}

void TMenuButton::menuButtonPressed(bool checked)
{
	menu_.exec(mapToGlobal(QPoint(0,height())));
}
