/*
	*** Qt GUI: Forcefields window
	*** src/gui/forcefield_funcs.cpp
	Copyright T. Youngs 2007-2011

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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/forcefields.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/tlistwidgetitem.h"
#include "gui/gui.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/pattern.h"
#include "base/sysfunc.h"

// Constructor
AtenForcefields::AtenForcefields(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	typelistElement_ = -1;
	shouldRefresh_ = FALSE;
	checkedItem_ = NULL;

	// Create open forcefield dialog
	QStringList filters;
	openForcefieldDialog = new QFileDialog(this);
	openForcefieldDialog->setFileMode(QFileDialog::ExistingFile);
	openForcefieldDialog->setDirectory(aten.dataDir());
	openForcefieldDialog->setWindowTitle("Open Forcefield");
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefield Files (*.ff)";
	openForcefieldDialog->setFilters(filters);

	// Create save forcefield dialog
	saveForcefieldDialog = new QFileDialog(this);
	saveForcefieldDialog->setWindowTitle("Save Forcefield");
	saveForcefieldDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveForcefieldDialog->setDirectory(aten.workDir());
	saveForcefieldDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefield Files (*.ff)";
	saveForcefieldDialog->setFilters(filters);
}

// Destructor
AtenForcefields::~AtenForcefields()
{
}

// Show window
void AtenForcefields::showWindow()
{
	show();
	if (shouldRefresh_) refresh();
}

// Update the list of loaded forcefields
void AtenForcefields::refresh()
{

}



void AtenForcefields::dialogFinished(int result)
{
	gui.mainWindow->ui.actionForcefieldsWindow->setChecked(FALSE);
}

