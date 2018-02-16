/*
	*** Popup Widget - Cell Scale Functions
	*** src/gui/popupcellscale_funcs.cpp
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

#include "gui/popupcellscale.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
CellScalePopup::CellScalePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void CellScalePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool CellScalePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	if (methodName == "scale")
	{
	        if (ui.ScaleCogsCheck->isChecked()) CommandNode::run(Commands::ScaleMolecules, "ddd", ui.ScaleXSpin->value(), ui.ScaleYSpin->value(), ui.ScaleZSpin->value());
		else CommandNode::run(Commands::Scale, "ddd", ui.ScaleXSpin->value(), ui.ScaleYSpin->value(), ui.ScaleZSpin->value());

	}
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
