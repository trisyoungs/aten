/*
	*** Popup Widget - Add Atom Functions
	*** src/gui/popupbuildaddatom_funcs.cpp
	Copyright T. Youngs 2007-2018

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

#include "gui/popupbuildaddatom.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
AddAtomPopup::AddAtomPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void AddAtomPopup::updateControls()
{
	refreshing_ = true;

	// Enable / disable Fractional checkbox
	Model* m = parent_.aten().currentModelOrFrame();
	ui.FractionalCheck->setEnabled(m ? m->isPeriodic() : false);

	refreshing_ = false;
}

// Call named method associated to popup
bool AddAtomPopup::callMethod(QString methodName, ReturnValue& rv)
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

void AddAtomPopup::on_AddButton_clicked(bool checked)
{
	// Run command
	if (ui.FractionalCheck->isChecked()) CommandNode::run(Commands::NewAtomFrac, "iddd", parent_.currentBuildElement(), ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	else CommandNode::run(Commands::NewAtom, "iddd", parent_.currentBuildElement(), ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());

	// Update display
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);

	// Hide popup
	done(false, UserAction::DrawAddHydrogenAction);
}

void AddAtomPopup::on_AddAgainButton_clicked(bool checked)
{
	// Run command
	if (ui.FractionalCheck->isChecked()) CommandNode::run(Commands::NewAtomFrac, "iddd", parent_.currentBuildElement(), ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	else CommandNode::run(Commands::NewAtom, "iddd", parent_.currentBuildElement(), ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());

	// Update display
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);

	// Don't hide popup, but set focus back to X coordinate
	ui.XSpin->setFocus();
}
