/*
	*** Qt vibration window functions
	*** src/gui/vibrations_funcs.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/vibrations.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Constructor
AtenVibrations::AtenVibrations(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	// Private variables
	refreshing_ = FALSE;
	shouldRefresh_ = TRUE;

	ui.setupUi(this);
}

// Destructor
AtenVibrations::~AtenVibrations()
{
}

// Show window
void AtenVibrations::showWindow()
{
	show();
	if (shouldRefresh_) refresh();
}

// Refresh window contents
void AtenVibrations::refresh()
{
	msg.enter("AtenVibrations::refresh");
	if (!gui.vibrationsWindow->isVisible())
	{
		shouldRefresh_ = TRUE;
		msg.exit("AtenVibrations::refresh");
		return;
	}
	refreshing_ = TRUE;
	Model *m = aten.currentModelOrFrame();
	Dnchar text;
	ui.VibrationsList->clear();
	ui.DisplacementsTable->clear();
	int count = 0;
	for (Vibration *vib = m->vibrations(); vib != NULL; vib = vib->next)
	{
		text.print("%i. Freq=%f\n", ++count, vib->frequency());
		ui.VibrationsList->addItem(text.get());
	}
	ui.VibrationsList->setCurrentRow(0);
	refreshing_ = FALSE;
	refreshDisplacements();
	msg.exit("AtenVibrations::refresh");
}

// Refresh displacement data
void AtenVibrations::refreshDisplacements()
{
	if (refreshing_) return;
	ui.DisplacementsTable->clear();
	int row = ui.VibrationsList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Dnchar text;
	Vibration *vib = m->vibration(row);
	Atom *i = m->atoms();
	Vec3<double> *displacements = vib->displacements();
	QTableWidgetItem *item;
	for (int n=0; n<vib->nDisplacements(); ++n)
	{
		if (i == NULL) msg.print("Warning - More displacements defined in Vibration than there are atoms in the parent model.\n");
		item = new QTableWidgetItem();
		text.print("%i (%s)", n+1, i == NULL ? "NULL" : elements().symbol(i));
		item->setText(text.get());
		ui.DisplacementsTable->setItem(n, 0, item);
		item = new QTableWidgetItem();
		item->setText(ftoa(displacements[n].x));
		ui.DisplacementsTable->setItem(n, 1, item);
		item = new QTableWidgetItem();
		item->setText(ftoa(displacements[n].y));
		ui.DisplacementsTable->setItem(n, 2, item);
		item = new QTableWidgetItem();
		item->setText(ftoa(displacements[n].z));
		ui.DisplacementsTable->setItem(n, 3, item);
	}
}

void AtenVibrations::dialogFinished(int result)
{
	gui.mainWindow->ui.actionVibrationsWindow->setChecked(FALSE);
}
