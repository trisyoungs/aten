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
FileOpenPopup::FileOpenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent), maxRecentFiles_(100)
{
	// Set up interface
	ui.setupUi(this);

	// Connect signal for context menu on FilesTable
	connect(ui.FilesTable, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(filesTableContextMenuRequested(QPoint)));
}

// Update controls (before show()) (virtual)
void FileOpenPopup::updateControls()
{
	refreshing_ = true;

	// Recreate the list
	ui.FilesTable->clear();
	ui.FilesTable->setColumnCount(1);
	ui.FilesTable->setRowCount(recentFiles_.count());

	QTableWidgetItem* item;
	int count = 0;
	for (int n=0; n<recentFiles_.count(); ++n)
	{
		QFileInfo fileInfo(recentFiles_.at(n));
		if (!fileInfo.exists())
		{
			recentFiles_.removeAt(n);
			continue;
		}

		item = new QTableWidgetItem(recentFiles_.at(n));
		item->setToolTip(recentFiles_.at(n));
		item->setData(Qt::UserRole, count);
		ui.FilesTable->setItem(count++, 0, item);
	}

	ui.FilesTable->setColumnWidth(0, width());
	ui.FilesTable->setRowCount(count);

	refreshing_ = false;
}

// Call named method associated to popup
bool FileOpenPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
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
		recentFiles_.prepend(newFileInfo.absoluteFilePath());

		// Remove files until we reach the max number (or lower) of allowable files
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
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Widget Functions
 */

void FileOpenPopup::on_FilesTable_itemClicked(QTableWidgetItem* item)
{
	if (item == NULL) return;

	// Load model
	if (parent_.aten().loadModel(item->text())) parent_.aten().setSingleModelVisible(parent_.aten().currentModel());

	// Update main window
	parent_.updateWidgets(AtenWindow::AllTarget);

	done();
}

// Context menu requested for FilesTable
void FileOpenPopup::filesTableContextMenuRequested(const QPoint& point)
{
	// Is there an item under the pointer?
	QTableWidgetItem* item = ui.FilesTable->itemAt(point);
	if (!item) return;

	// Build the context menu to display
	QMenu contextMenu;
	QAction* removeAction = contextMenu.addAction("&Remove Entry");
// 	QAction* pinAction = contextMenu.addAction("&Pin");
// 	QAction* unpinAction = contextMenu.addAction("&Unpin");
	
	// Show it
	QAction* menuResult = contextMenu.exec(QCursor::pos());

	// What was clicked?
	if (menuResult == removeAction)
	{
		recentFiles_.removeAt(item->data(Qt::UserRole).toInt());
		updateControls();
	}
// 	else if (menuResult == pinAction)
// 	{
// 	}
// 	else if (menuResult == unpinAction)
// 	{
// 	}
}
