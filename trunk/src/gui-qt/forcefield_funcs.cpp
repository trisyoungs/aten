/*
	*** Qt forcefield functions interface
	*** src/gui-qt/forcefield_funcs.cpp
	Copyright T. Youngs 2007

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

#include "base/master.h"
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"
#include <QtGui/QListWidgetItem>
#include <QtGui/QFileDialog>

void AtenForm::refresh_forcefieldpage()
{
	// Update the list of loaded forcefields
	ui.ForcefieldList->clear();
	QListWidgetItem *item;
	for (forcefield *ff = master.get_ffs(); ff != NULL; ff = ff->next)
	{
		item = new QListWidgetItem(ui.ForcefieldList);
		item->setText(ff->get_name());
	}
	// Select the current FF.
	if (master.get_currentff() == NULL) ui.ForcefieldList->setCurrentRow(0);
	else ui.ForcefieldList->setCurrentRow(master.get_currentff_id());
}

void AtenForm::on_ForcefieldList_currentRowChanged(int row)
{
	// Set the current forcefield in master to reflect the list change
	master.set_currentff_by_id(row);
}

void AtenForm::on_LoadForcefieldButton_clicked(bool checked)
{
	QString filename;
	if (openffdialog->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = openffdialog->selectedFilter();
		filename = openffdialog->selectedFiles().first();
		master.load_ff(qPrintable(filename));
		refresh_forcefieldpage();
	}
}

void AtenForm::on_RemoveForcefieldButton_clicked(bool checked)
{
	master.remove_ff(master.get_currentff());
	refresh_forcefieldpage();
}

void AtenForm::on_EditForcefieldButton_clicked(bool checked)
{
	printf("Forcefield editor not yet implemented.\n");
}

void AtenForm::on_AssignFFToCurrentButton_clicked(bool checked)
{
	master.get_currentmodel()->set_ff(master.get_currentff());
}

void AtenForm::on_AssignFFToAllButton_clicked(bool checked)
{
	for (model *m = master.get_models(); m != NULL; m = m->next) m->set_ff(master.get_currentff());
}

void AtenForm::on_TypeModelButton_clicked(bool checked)
{
	if (master.get_currentmodel()->type_all()) gui.mainview.postredisplay();
}

void AtenForm::on_UntypeModelButton_clicked(bool checked)
{
	master.get_currentmodel()->remove_typing();
	gui.mainview.postredisplay();
}

