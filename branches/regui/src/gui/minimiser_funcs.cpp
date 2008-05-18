/*
	*** Qt minimiser functions interface
	*** src/gui/minimise_funcs.cpp
	Copyright T. Youngs 2007,2008

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
#include "methods/mc.h"
#include "methods/sd.h"
#include "methods/cg.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/minimiser.h"

// Minimisation algorithms
enum MinimiserMethod { MM_STEEPEST, MM_CONJUGATE, MM_MONTECARLO, MM_SIMPLEX, MM_NITEMS };

// Constructor
AtenMinimiser::AtenMinimiser(QWidget *parent)
{
	ui.setupUi(this);
}

// Destructor
AtenMinimiser::~AtenMinimiser()
{
}

void AtenMinimiser::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

void AtenMinimiser::on_MinimiserMethodCombo_currentIndexChanged(int index)
{
	ui.MethodOptionsStack->setCurrentIndex(index);
}

void AtenMinimiser::on_MinimiseButton_clicked(bool checked)
{
	double econverge, fconverge, linetol;
	int maxcycles;
	// Get the convergence values from the window controls
	econverge = pow(10.0,ui.EnergyConvergeSpin->value());
	fconverge = pow(10.0,ui.ForceConvergeSpin->value());
	maxcycles = ui.MinimiseCyclesSpin->value();
	// Perform the minimisation
	switch (ui.MinimiserMethodCombo->currentIndex())
	{
		case (MM_STEEPEST):
			sd.setNCycles(maxcycles);
			sd.setTolerance(pow(10.0,ui.SDLineToleranceSpin->value()));
			sd.minimise(master.currentModel(),econverge,fconverge);
			break;
		case (MM_CONJUGATE):
			cg.setNCycles(maxcycles);
			cg.setTolerance(pow(10.0,ui.CGLineToleranceSpin->value()));
			cg.minimise(master.currentModel(),econverge,fconverge);
			break;
		case (MM_MONTECARLO):
			mc.setNCycles(maxcycles);
			mc.minimise(master.currentModel(),econverge,fconverge);
			break;
		case (MM_SIMPLEX):
			msg(Debug::None,"Simplex minimiser not yet written!\n");
			break;
	}
	gui.modelChanged();
}

void AtenMinimiser::dialogFinished(int result)
{
	gui.mainWindow->ui.actionMinimiserDialog->setChecked(FALSE);
}
