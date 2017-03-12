/*
	*** Popup Widget - File Aten
	*** src/gui/popupfileaten_funcs.cpp
	Copyright T. Youngs 2007-2017

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
#include "gui/prefs.h"
#include "gui/aboutplugins.h"
#include "base/namespace.h"
#include <QMessageBox>

ATEN_USING_NAMESPACE

// Constructor
FileAtenPopup::FileAtenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Set version label
	ui.VersionLabel->setText("v" + QString(ATENVERSION));
}

// Update controls (before show()) (virtual)
void FileAtenPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool FileAtenPopup::callMethod(QString methodName, ReturnValue& rv)
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

void FileAtenPopup::on_StoreStateButton_clicked(bool checked)
{
	// Store settings
	QSettings settings;

	// Toolbar visibility / position
	settings.setValue("MainWinPositions", parent_.saveState() );
	settings.setValue("MainWinGeometries", parent_.saveGeometry() );
	settings.setValue("MainWinSize", parent_.size());
	settings.setValue("MainWinPosition", parent_.pos());

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
	prefsDialog.exec();

	// Hide popup
	done();
}

// ATEN2 TODO ENDOFFILTERS
void FileAtenPopup::on_ReloadPluginsButton_clicked(bool checked)
{
// 	parent_.aten().reloadFilters();

	// Hide popup
	done();
}

void FileAtenPopup::on_PluginInfoButton_clicked(bool checked)
{
	AtenAboutPlugins aboutPlugins(parent_);
	
	aboutPlugins.exec();

	// Hide popup
	done();
}
