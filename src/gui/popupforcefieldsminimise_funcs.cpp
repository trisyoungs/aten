/*
	*** Popup Widget - Forcefields Minimise Functions
	*** src/gui/popupforcefieldsminimise_funcs.cpp
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

#include "gui/popupforcefieldsminimise.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"
#include "templates/variantpointer.h"

ATEN_USING_NAMESPACE

// Constructor
ForcefieldsMinimisePopup::ForcefieldsMinimisePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
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
	// We want the stack page to change when the combo index changes, so don't set the refreshing_ flag
// 	refreshing_ = true;

	// Get currently-selected method (if any)
	int currentMethod = ui.MethodCombo->currentIndex();

	// Clear and repopulate the MethodCombo
	ui.MethodCombo->clear();
	ui.MethodCombo->addItem("Steepest Descent (Simple)");
	ui.MethodCombo->addItem("Steepest Descent (Line Minimised)");
	ui.MethodCombo->addItem("Conjugate Gradient");
	ui.MethodCombo->addItem("Monte Carlo (Molecular)");

	// Query plugin store to see if there are any optimisation method plugins to add to the list
	const RefList<MethodPluginInterface,KVMap>& optimisationPlugins = parent_.aten().pluginStore().methodPlugins(PluginTypes::OptimisationMethodPlugin);
	for (RefListItem<MethodPluginInterface,KVMap>* ri = optimisationPlugins.first(); ri != NULL; ri = ri->next)
	{
		MethodPluginInterface* plugin = ri->item;

		ui.MethodCombo->addItem(plugin->name(), VariantPointer<MethodPluginInterface>(plugin));
	}

// 	refreshing_ = false;
}

// Call named method associated to popup
bool ForcefieldsMinimisePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else if (methodName == "minimise")
	{
		// Perform the minimisation according to the selected method
		MethodPluginInterface* plugin;
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
			default:
				// An optimisation plugin - get the pointer to its class...
				plugin = VariantPointer<MethodPluginInterface>(ui.MethodCombo->currentData());
				if (!plugin)
				{
					printf("Couldn't cast data into MethodPluginInterface.\n");
					return false;
				}
				plugin = (MethodPluginInterface*) plugin->duplicate();
				plugin->setTargetModel(parent_.aten().currentModelOrFrame());
				plugin->executeMethod();
				break;
		}

		// Update the view
		parent_.updateWidgets(AtenWindow::AtomsTableTarget);
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
