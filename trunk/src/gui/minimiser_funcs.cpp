/*
	*** Qt minimiser functions interface
	*** src/gui/minimise_funcs.cpp
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
#include "methods/mc.h"
#include "methods/sd.h"
#include "methods/cg.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/minimiser.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
AtenMinimiser::AtenMinimiser(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
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
	// Show/hide other controlsi?
	bool enabled = (index != AtenMinimiser::MopacMethod);
	ui.ConvergenceGroup->setEnabled(enabled);
	ui.MinimiseCyclesSpin->setEnabled(enabled);
	ui.MethodOptionsStack->setEnabled(enabled);
}

void AtenMinimiser::on_MinimiseButton_clicked(bool checked)
{
	doMinimisation();
	gui.update();
}

void AtenMinimiser::doMinimisation()
{
	// Set convergence criteria and get maxcycles data
	CommandNode::run(Command::Converge, "dd", pow(10.0,ui.EnergyConvergeSpin->value()), pow(10.0,ui.ForceConvergeSpin->value()));
	int maxcycles = ui.MinimiseCyclesSpin->value();
	Dnchar options;
	
	// Perform the minimisation
	switch (ui.MinimiserMethodCombo->currentIndex())
	{
		case (SimpleSteepestMethod):
			CommandNode::run(Command::LineTolerance, "d", pow(10.0,ui.SDLineToleranceSpin->value()));
			CommandNode::run(Command::SDMinimise, "ii", maxcycles, 1);
			break;
		case (SteepestMethod):
			CommandNode::run(Command::LineTolerance, "d", pow(10.0,ui.SDLineToleranceSpin->value()));
			CommandNode::run(Command::SDMinimise, "ii", maxcycles, 0);
			break;
		case (ConjugateMethod):
			CommandNode::run(Command::LineTolerance, "d", pow(10.0,ui.SDLineToleranceSpin->value()));
			CommandNode::run(Command::CGMinimise, "i", maxcycles);
			break;
		case (MonteCarloMethod):
			CommandNode::run(Command::MCMinimise, "i", maxcycles);
			break;
		case (MopacMethod):
			// Construct command string from GUI widget options
			
			CommandNode::run(Command::MopacMinimise, "c", options.get());
			break;
	}
	// Update the view
	gui.update(FALSE,FALSE,FALSE);
}

void AtenMinimiser::dialogFinished(int result)
{
	gui.mainWindow->ui.actionMinimiserWindow->setChecked(FALSE);
}
