/*
	*** Popup Widget - File Session
	*** src/gui/popupfilesession_funcs.cpp
	Copyright T. Youngs 2007-2018

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

#include "gui/popupfilesession.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include <main/aten.h>
#include <QMessageBox>
#include <QFileDialog>

ATEN_USING_NAMESPACE

// Constructor
FileSessionPopup::FileSessionPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void FileSessionPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool FileSessionPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
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

void FileSessionPopup::on_ClearButton_clicked(bool checked)
{
	if (QMessageBox::question(this, "Clear Session", "This will clear all current data. Proceed?") == QMessageBox::Yes)
	{
		parent_.aten().clearSession();
	
		parent_.aten().addModel();

		parent_.updateWidgets(AtenWindow::AllTargets);
	}

	// Hide popup
	done();
}

void FileSessionPopup::on_LoadButton_clicked(bool checked)
{
	if (QMessageBox::question(this, "Load Session", "This will clear all current data. Proceed?") == QMessageBox::Yes)
	{
		// Get session filename to load
		static QDir currentDirectory_(parent_.aten().workDir());
		QString filename = QFileDialog::getOpenFileName(this, "Load Session", currentDirectory_.path(), "Aten Session File (*.asf);; All Files (*)");
		if (!filename.isEmpty())
		{
			// Store path for next use
			currentDirectory_.setPath(filename);

			if (!parent_.aten().loadSession(filename))
			{
				QMessageBox::warning(this, "Error loading session file", "The session file could not be loaded.\nAt this point I would blame the developer.");
			}

			parent_.updateWidgets(AtenWindow::AllTargets);
		}
	}

	// Hide popup
	done();
}

void FileSessionPopup::on_SaveButton_clicked(bool checked)
{
	// Check for existing session file name...
	static QDir currentDirectory_(parent_.aten().workDir());
	if (!parent_.aten().sessionFilename().isEmpty())
	{
		// Does this file exist?
		if (QFile::exists(parent_.aten().sessionFilename()))
		{
			if (QMessageBox::question(this, "Save Session", "A file named '" + parent_.aten().sessionFilename() + "' already exists.\nOverwrite it?") == QMessageBox::No)
			{
				
				QString filename = QFileDialog::getSaveFileName(this, "Save Session", currentDirectory_.path(), "Aten Session File (*.asf);; All Files (*)");
				if (!filename.isEmpty())
				{
					// Store path for next use
					currentDirectory_.setPath(filename);

					if (!parent_.aten().saveSession(filename))
					{
						QMessageBox::warning(this, "Error saving session file", "The session file could not be saved.");
					}
				}
			}
			else
			{
				// OK to overwrite old file...
				if (!parent_.aten().saveSession(parent_.aten().sessionFilename()))
				{
					QMessageBox::warning(this, "Error saving session file", "The session file could not be saved.");
				}
			}
		}
	}
	else
	{
		QString filename = QFileDialog::getSaveFileName(this, "Save Session", currentDirectory_.path(), "Aten Session File (*.asf);; All Files (*)");
		if (!filename.isEmpty())
		{
			// Store path for next use
			currentDirectory_.setPath(filename);

			if (!parent_.aten().saveSession(filename))
			{
				QMessageBox::warning(this, "Error saving session file", "The session file could not be saved.");
			}
		}
	}

	// Hide popup
	done();
}

void FileSessionPopup::on_SaveAsButton_clicked(bool checked)
{
	static QDir currentDirectory_(parent_.aten().workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Session", currentDirectory_.path(), "Aten Session File (*.asf);; All Files (*)");
	if (!filename.isEmpty())
	{
		// Does this file exist?
		if (QFile::exists(filename))
		{
			if (QMessageBox::question(this, "Save Session", "A file named '" + parent_.aten().sessionFilename() + "' already exists.\nOverwrite it?") == QMessageBox::Yes)
			{
				
				// Store path for next use
				currentDirectory_.setPath(filename);

				if (!parent_.aten().saveSession(filename))
				{
					QMessageBox::warning(this, "Error saving session file", "The session file could not be saved.");
				}
			}
		}
		else
		{
			// Store path for next use
			currentDirectory_.setPath(filename);

			if (!parent_.aten().saveSession(filename))
			{
				QMessageBox::warning(this, "Error saving session file", "The session file could not be saved.");
			}
		}
	}

	// Hide popup
	done();
}

