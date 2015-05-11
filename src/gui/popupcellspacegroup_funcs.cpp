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
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */
/*
void CellSpacegroupPopup::on_CellSpacegroupSetButton_clicked(bool checked)
{
	// Grab the current text of the line edit and determine spacegroup
	CommandNode::run(Commands::Spacegroup, "c", qPrintable(ui.CellSpacegroupEdit->text()));
	ui.CellSpacegroupEdit->setText("");
	// Set spacegroup label
	Model* m = parent_.aten().currentModelOrFrame();
	QString label;
	label.sprintf("%s (%i)\n", Spacegroups[m->cell()->spacegroupId()].name, m->cell()->spacegroupId());
	ui.SpacegroupLabel->setText(label);
}

void CellSpacegroupPopup::on_CellSpacegroupRemoveButton_clicked(bool checked)
{
	CommandNode::run(Commands::Spacegroup, "i", 0);

	// Set spacegroup label
	// ui.SpacegroupLabel->setText("None (0)"); ATEN2 TODO
}*/

void CellSpacegroupPopup::on_PackButton_clicked(bool checked)
{
	CommandNode::run(Commands::Pack, "");

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);

	done();
}

void CellSpacegroupPopup::on_SpacegroupEdit_XXX(double value)
{
	// ATEN2 TODO
}
