/*
	*** Popup Widget - Cell Spacegroup Functions
	*** src/gui/popupcellspacegroup_funcs.cpp
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

#include "gui/popupcellspacegroup.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "base/spacegroup.h"

ATEN_USING_NAMESPACE

// Constructor
CellSpacegroupPopup::CellSpacegroupPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void CellSpacegroupPopup::popup()
{
	// Update lengths in spin boxes
	refreshing_ = true;

	// Get current model
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
// 		Spacegroups[]; ATEN2 TODO
// 		ui.SpacegroupCombo->setCurrentIndex(model->cell()->spacegroupId());
	}

	// Enable / disable controls as necessary
	ui.SpacegroupCombo->setEnabled(model);

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool CellSpacegroupPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

// Adjust matrix of current model
void CellSpacegroupPopup::adjustCurrentMatrix(int lengthIndex, double value)
{
	// Get current model and set new angle in cell
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		UnitCell cell = (*model->cell());
		cell.setLength(lengthIndex, value);
		CommandNode::run(Commands::CellAxes, "ddddddddd", cell.parameter(UnitCell::CellAX), cell.parameter(UnitCell::CellAY), cell.parameter(UnitCell::CellAZ), cell.parameter(UnitCell::CellBX), cell.parameter(UnitCell::CellBY), cell.parameter(UnitCell::CellBZ),  cell.parameter(UnitCell::CellCX), cell.parameter(UnitCell::CellCY), cell.parameter(UnitCell::CellCZ)); 
	}
}

