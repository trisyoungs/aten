/*
	*** Popup Widget - File Open
	*** src/gui/popupfileopen_funcs.cpp
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

#include "gui/popupfileopen.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
FileOpenPopup::FileOpenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent), maxRecentFiles_(100)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void FileOpenPopup::popup()
{
	refreshing_ = true;

	// Recreate the list
	ui.FilesTable->clear();
	ui.FilesTable->setColumnCount(1);
	ui.FilesTable->setRowCount(recentFiles_.count());

	QTableWidgetItem* item;
	for (int n=0; n<recentFiles_.count(); ++n)
	{
		QFileInfo fileInfo(recentFiles_.at(n));
		if (!fileInfo.exists())
		{
			recentFiles_.removeAt(n);
			continue;
		}

		item = new QTableWidgetItem(recentFiles_.at(n));
		ui.FilesTable->setItem(n, 0, item);
	}

	ui.FilesTable->setColumnWidth(0, width());

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool FileOpenPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "addRecentFile")
	{
		// Get file information for the supplied filename
		QFileInfo newFileInfo(rv.asString());

		// Check to see if the file already exists in the list
		for (int n=0; n<recentFiles_.count(); ++n)
		{
			QFileInfo oldFileInfo(recentFiles_.at(n));
			if (newFileInfo == oldFileInfo)
			{
				recentFiles_.move(n, 0);
				return true;
			}
		}

		// Not in the list, so add it to the top
		recentFiles_.prepend(rv.asString());

		// Remove files until we reach the 
		while (recentFiles_.count() > maxRecentFiles_) recentFiles_.removeLast();

		return true;
	}
	else if (methodName == "maxRecentFiles")
	{
		rv = maxRecentFiles_;
		return true;
	}
	else if (methodName == "nRecentFiles")
	{
		rv = recentFiles_.count();
		return true;
	}
	else if (methodName == "recentFile")
	{
		rv = recentFiles_.at(rv.asInteger());
		return true;
	}
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

void FileOpenPopup::on_FilesTable_itemDoubleClicked(QTableWidgetItem* item)
{
	// ATEN2 TODO
}

/*
// Load recent file
void AtenWindow::loadRecent()
{
	QString filename;
	Model* m;
	Tree* filter;

	// Cast sending QAction and grab filename
	QAction* action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenWindow::loadRecent - Sender was not a QAction.\n");
		return;
	}

	// Grab the filename from the action
	filename = action->data().toString();

	// See if any loaded model filename matches this filename
	for (m = aten_.models(); m != NULL; m = m->next)
	{
		Messenger::print(Messenger::Verbose, "Checking loaded models for '%s': %s", qPrintable(filename), qPrintable(m->filename()));
		if (filename == m->filename())
		{
			Messenger::print(Messenger::Verbose, "Matched filename to loaded model.");
			aten_.setCurrentModel(m);
			// Update GUI
			updateWidgets(AtenWindow::AllTarget);
			return;
		}
	}

	// If we get to here then the model is not currently loaded...
	filter = aten_.probeFile(filename, FilterData::ModelImport);
	if (filter != NULL)
	{
		ReturnValue rv;
		filter->executeRead(filename, rv);

		// Update GUI
		updateWidgets(AtenWindow::AllTarget);
	}
	else
	{
		// Remove file from recent files list
		int last, n;
		for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;
		for (n=last+1; n<MAXRECENTFILES; ++n)
		{
			if (actionRecentFile[last]->isVisible())
			{
				actionRecentFile[n-1]->setText(actionRecentFile[n]->text());
				actionRecentFile[n-1]->setData(actionRecentFile[n]->data());
			}
		}
	}
}

// Add file to top of recent list
void AtenWindow::addRecent(QString filename)
{
	// Find unused (i.e. still hidden) recent file action
	int last, n;
	QString temp;
	for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;

	// 'last' now holds the first empty slot in the recent files list.
	// If 'last' == MAXRECENTFILES then shuffle top 'n-1' down a position and add at '0'.
	if (last == MAXRECENTFILES)
	{
		// Push the top items down the list
		for (n=MAXRECENTFILES-2; n>=0; n--)
		{
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
			temp.sprintf("&%i %s", n+1, qPrintable(actionRecentFile[n]->data().toString()));
			actionRecentFile[n+1]->setText(temp);
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
		}
		last = 0;
	}

	// Set the new data
	temp.sprintf("&%i %s", last, qPrintable(filename));
	actionRecentFile[last]->setText(temp);
	actionRecentFile[last]->setData(filename);
	actionRecentFile[last]->setVisible(true);
}*/