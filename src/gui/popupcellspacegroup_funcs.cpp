/*
	*** Popup Widget - Cell Spacegroup Functions
	*** src/gui/popupcellspacegroup_funcs.cpp
	Copyright T. Youngs 2007-2016

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
#include "sg/spacegroup.h"

ATEN_USING_NAMESPACE

// Constructor
CellSpacegroupPopup::CellSpacegroupPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Add spacegroups to SpacegroupCombo
	for (int n=0; n<231; ++n) ui.SpacegroupCombo->addItem(QString("%1. %2").arg(n).arg(Spacegroups[n].name));
}

// Update controls (before show()) (virtual)
void CellSpacegroupPopup::updateControls()
{
	refreshing_ = true;

	// Get current model
	Model* model = parent_.aten().currentModelOrFrame();
	bool enabled = (model ? model->isPeriodic() : false);

	// Enable / disable controls
	ui.SetButton->setEnabled(enabled);
	ui.RemoveButton->setEnabled(enabled);
	ui.PackButton->setEnabled(model && (model->cell().spacegroupId() != 0));

	ui.CurrentLabel->setText(model && (model->cell().spacegroupId() != 0) ? QString("%1 (%2)").arg(Spacegroups[model->cell().spacegroupId()].displayName).arg(model->cell().spacegroupId()) : "<None>"); 

	// Enable / disable controls as necessary
	ui.SpacegroupCombo->setEnabled(model);

	refreshing_ = false;
}

// Call named method associated to popup
bool CellSpacegroupPopup::callMethod(QString methodName, ReturnValue& rv)
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

void CellSpacegroupPopup::on_LookupEdit_textChanged(QString text)
{
	// Test for a plain integer
	bool ok;
	int sgid = text.toInt(&ok);
	if (ok)
	{
		ui.SpacegroupCombo->setCurrentIndex(sgid);
		return;
	}

	// Perhaps a spacegroup name? Look for an exact (case-insensitive) match
	for (int n=0; n<231; ++n)
	{
		QRegExp re(Spacegroups[n].name);
		re.setPatternSyntax(QRegExp::FixedString);
		re.setCaseSensitivity(Qt::CaseInsensitive);
		if (re.exactMatch(text))
		{
			ui.SpacegroupCombo->setCurrentIndex(n);
			return;
		}
	}

	// No idea!
}

void CellSpacegroupPopup::on_RemoveButton_clicked(bool checked)
{
	CommandNode::run(Commands::Spacegroup, "i", 0);

	updateControls();
}

void CellSpacegroupPopup::on_SetButton_clicked(bool checked)
{
	// Set the spacegroup based on integer index of the combo
	CommandNode::run(Commands::Spacegroup, "i", ui.SpacegroupCombo->currentIndex());

	updateControls();
}

void CellSpacegroupPopup::on_PackButton_clicked(bool checked)
{
	CommandNode::run(Commands::Pack, "");

	// Update display
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);

	// Hide popup
	done();
}

