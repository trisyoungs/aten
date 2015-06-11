/*
	*** Popup Widget - File Aten
	*** src/gui/popupfileaten_funcs.cpp
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

#include "gui/popupfileaten.h"
#include "main/aten.h"
#include "main/version.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "gui/prefs.h"
#include <QMessageBox>

ATEN_USING_NAMESPACE

// Constructor
FileAtenPopup::FileAtenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Set version label
	ui.VersionLabel->setText("v" + QString(ATENVERSION));
}

// Show popup, updating any controls as necessary beforehand
void FileAtenPopup::popup()
{
	refreshing_ = true;

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool FileAtenPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
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

void FileAtenPopup::on_StoreStateButton_clicked(bool checked)
{
	// Store settings
	QSettings settings;

	// Toolbar visibility / position
	settings.setValue("MainWinPositions", parent_.saveState() );
	settings.setValue("MainWinGeometries", saveGeometry() );
	settings.setValue("MainWinSize", size());
	settings.setValue("MainWinPosition", pos());

	// Atoms table and models list
	settings.setValue("AtomsTableCollapsed", !parent_.ui.AtomsTableToggleButton->isChecked());
	settings.setValue("ModelsListCollapsed", !parent_.ui.ModelsListToggleButton->isChecked());

	// Synchronise (i.e. save) changes to settings
	settings.sync();

	// Hide popup
	done();
}

void FileAtenPopup::on_PreferencesButton_clicked(bool checked)
{
	AtenPrefs prefsDialog(parent_);
	prefsDialog.setControls();
	prefsDialog.exec();

	// Hide popup
	done();
}

void FileAtenPopup::on_ReloadFiltersButton_clicked(bool checked)
{
	parent_.aten().reloadFilters();
	
	if (parent_.aten().failedFilters().count() > 0)
	{
		QMessageBox::warning(this, "Aten", "Errors encountered while reloading filters - see message box for details.", QMessageBox::Ok);
	}

	// Hide popup
	done();
}
