/*
	*** Popup Widget - Cell Replicate Functions
	*** src/gui/popupcellreplicate_funcs.cpp
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

#include "gui/popupcellreplicate.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
CellReplicatePopup::CellReplicatePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void CellReplicatePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool CellReplicatePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "replicate")
	{
		rv = CommandNode::run(Commands::Replicate, "dddddd", ui.NegativeXSpin->value(), ui.NegativeYSpin->value(), ui.NegativeZSpin->value(), ui.PositiveXSpin->value(), ui.PositiveYSpin->value(),  ui.PositiveZSpin->value()).asBool();
	}
	else if (methodName == "negativeVector")
	{
		rv.set(Vec3<double>(ui.NegativeXSpin->value(), ui.NegativeYSpin->value(), ui.NegativeZSpin->value()));
	}
	else if (methodName == "positiveVector")
	{
		rv.set(Vec3<double>(ui.PositiveXSpin->value(), ui.PositiveYSpin->value(),  ui.PositiveZSpin->value()));
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
