/*
	*** Popup Widget - Transform Flip Functions
	*** src/gui/popuptransformflip_funcs.cpp
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

#include "gui/popuptransformflip.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformFlipPopup::TransformFlipPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformFlipPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformFlipPopup::callMethod(QString methodName, ReturnValue& rv)
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

void TransformFlipPopup::on_FlipXButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Mirror, "i", 0);

	// Update window
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);

	// Hide popup
	hide();
}

void TransformFlipPopup::on_FlipYButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Mirror, "i", 1);

	// Update window
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);

	// Hide popup
	hide();
}

void TransformFlipPopup::on_FlipZButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Mirror, "i", 2);

	// Update window
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);

	// Hide popup
	hide();
}
