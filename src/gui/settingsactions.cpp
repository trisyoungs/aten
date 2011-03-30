/*
	*** System Actions
	*** src/gui/systemactions.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/toolbox.h"

// Show preferences window
void AtenForm::on_actionPreferences_triggered(bool checked)
{
	gui.prefsDialog->setControls();
	gui.prefsDialog->exec();
}

// Reload all filters
void AtenForm::on_actionReloadFilters_triggered(bool checked)
{
	if (aten.reloadFilters() > 0)
	{
		QMessageBox::warning(this, "Aten", "Errors encountered while reloading filters - see message box for details.", QMessageBox::Ok);
	}
	createDialogFilters();
	gui.loadModelDialog->setControls();
}

// Show main ToolBox
void AtenForm::on_actionShowToolBox_triggered(bool checked)
{
	gui.toolBoxWidget->setVisible(TRUE);
	gui.toolBoxWidget->setFloating(TRUE);
// 	gui.toolBoxWidget->
}
