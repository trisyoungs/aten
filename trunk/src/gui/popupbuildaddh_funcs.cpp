/*
	*** Popup Widget - AddH Functions
	*** src/gui/popupbuildaddh_funcs.cpp
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

#include "gui/popupbuildaddh.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
AddHPopup::AddHPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void AddHPopup::popup()
{
	show();
}

/*
 * Widget Functions
 */

void AddHPopup::on_AddHSelectionButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SelectionAddHydrogen, "");

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);

	// Hide popup
	hide();
}

void AddHPopup::on_AddHModelButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::AddHydrogen, "");

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);

	// Hide popup
	hide();
}

