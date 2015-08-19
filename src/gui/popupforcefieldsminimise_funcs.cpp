/*
	*** Popup Widget - Forcefields Minimise Functions
	*** src/gui/popupforcefieldsminimise_funcs.cpp
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

#include "gui/popupforcefieldsminimise.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
ForcefieldsMinimisePopup::ForcefieldsMinimisePopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Set initial values
	ui.EnergyConvergeSpin->setRange(true, 1.0e-10, false, 0.0);
	ui.EnergyConvergeSpin->setValue(1.0e-4);
	ui.ForceConvergeSpin->setRange(true, 1.0e-10, false, 0.0);
	ui.ForceConvergeSpin->setValue(1.0e-3);
}

// Update controls (before show()) (virtual)
void ForcefieldsMinimisePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool ForcefieldsMinimisePopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else if (methodName == "minimise")
	{
		// Perform the minimisation according to the selected method
		QString mopacOptions;
		switch (ui.MethodCombo->currentIndex())
		{
			case (SimpleSteepestMethod):
				CommandNode::run(Commands::SDMinimise, "idddi", ui.MaxCyclesSpin->value(), ui.EnergyConvergeSpin->value(), ui.ForceConvergeSpin->value(), ui.LineToleranceSpin->value(), 1);
				break;
			case (SteepestMethod):
				CommandNode::run(Commands::SDMinimise, "iddd", ui.MaxCyclesSpin->value(), ui.EnergyConvergeSpin->value(), ui.ForceConvergeSpin->value(), ui.LineToleranceSpin->value());
				break;
			case (ConjugateMethod):
				CommandNode::run(Commands::CGMinimise, "iddd", ui.MaxCyclesSpin->value(), ui.EnergyConvergeSpin->value(), ui.ForceConvergeSpin->value(), ui.LineToleranceSpin->value());
				break;
			case (MonteCarloMethod):
				CommandNode::run(Commands::MCMinimise, "i", ui.MaxCyclesSpin->value());
				break;
			case (MopacMethod):
				// Construct command string from GUI widget options
				mopacOptions = QString("BFGS %1 %2 %3 CHARGE=%4").arg(ui.MopacHFCombo->currentText(), ui.MopacHamiltonianCombo->currentText(), ui.MopacSpinCombo->currentText()).arg(ui.MopacChargeSpin->value());
				if (ui.MopacMozymeCheck->isChecked()) mopacOptions += " MOZYME";
				CommandNode::run(Commands::MopacMinimise, "c", qPrintable(mopacOptions));
				break;
		}

		// Update the view
		parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */
