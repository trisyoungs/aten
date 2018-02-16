/*
	*** Popup Widget - View ColourScheme Functions
	*** src/gui/popupviewcolourscheme_funcs.cpp
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

#include "gui/popupviewcolourscheme.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ViewColourSchemePopup::ViewColourSchemePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Add colour scheme tool buttons to their button group
	ui.ElementButton->setGroup("Colourschemes");
	ui.ChargeButton->setGroup("Colourschemes");
	ui.ForceButton->setGroup("Colourschemes");
	ui.VelocityButton->setGroup("Colourschemes");
	ui.BondsButton->setGroup("Colourschemes");
	ui.OwnButton->setGroup("Colourschemes");
}

// Update controls (before show()) (virtual)
void ViewColourSchemePopup::updateControls()
{
	Model* model = parent_.aten().currentModelOrFrame();
	if (model) return;

	refreshing_ = true;
	

	switch (model->colourScheme())
	{
		case (Prefs::ElementScheme):
			ui.ElementButton->setChecked(true);
			break;
		case (Prefs::ChargeScheme):
			ui.ChargeButton->setChecked(true);
			break;
		case (Prefs::ForceScheme):
			ui.ForceButton->setChecked(true);
			break;
		case (Prefs::VelocityScheme):
			ui.VelocityButton->setChecked(true);
			break;
		case (Prefs::BondsScheme):
			ui.BondsButton->setChecked(true);
			break;
		case (Prefs::OwnScheme):
			ui.OwnButton->setChecked(true);
			break;
		default:
			printf("Warning: Model has odd render style (%i)\n", model->colourScheme());
			break;
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool ViewColourSchemePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "updateButtonIcon")
	{
		if (!parentMenuButton()) return false;
		Prefs::ColouringScheme cs = Prefs::colouringScheme(rv.asString());
		switch (cs)
		{
			case (Prefs::ElementScheme):
				parentMenuButton()->setIcon(QIcon(":/colourscheme/icons/colourscheme_element.png"));
				break;
			case (Prefs::ChargeScheme):
				parentMenuButton()->setIcon(QIcon(":/colourscheme/icons/colourscheme_charge.png"));
				break;
			case (Prefs::ForceScheme):
				parentMenuButton()->setIcon(QIcon(":/colourscheme/icons/colourscheme_force.png"));
				break;
			case (Prefs::VelocityScheme):
				parentMenuButton()->setIcon(QIcon(":/colourscheme/icons/colourscheme_velocity.png"));
				break;
			case (Prefs::BondsScheme):
				parentMenuButton()->setIcon(QIcon(":/colourscheme/icons/colourscheme_bonds.png"));
				break;
			case (Prefs::OwnScheme):
				parentMenuButton()->setIcon(QIcon(":/colourscheme/icons/colourscheme_own.png"));
				break;
			default:
				printf("Warning: Odd colourscheme passed (%s) so can't set parent button's icon.\n", qPrintable(rv.asString()));
				break;
		}
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

void ViewColourSchemePopup::on_ElementButton_clicked(bool checked)
{
	Model* model = parent_.aten().currentModelOrFrame();

	if ((!model) || (!checked)) return;

	model->beginUndoState("Change model colour scheme");
	model->setColourScheme(Prefs::ElementScheme);
	model->endUndoState();

	// Set icon in button
	callMethodSimple("updateButtonIcon", Prefs::colouringScheme(Prefs::ElementScheme));

	parent_.updateWidgets();

	// Hide popup
	done();
}

void ViewColourSchemePopup::on_ChargeButton_clicked(bool checked)
{
	Model* model = parent_.aten().currentModelOrFrame();

	if ((!model) || (!checked)) return;

	model->beginUndoState("Change model colour scheme");
	model->setColourScheme(Prefs::ChargeScheme);
	model->endUndoState();

	// Set icon in button
	callMethodSimple("updateButtonIcon", Prefs::colouringScheme(Prefs::ChargeScheme));

	parent_.updateWidgets();

	// Hide popup
	done();
}

void ViewColourSchemePopup::on_ForceButton_clicked(bool checked)
{
	Model* model = parent_.aten().currentModelOrFrame();

	if ((!model) || (!checked)) return;

	model->beginUndoState("Change model colour scheme");
	model->setColourScheme(Prefs::ForceScheme);
	model->endUndoState();

	// Set icon in button
	callMethodSimple("updateButtonIcon", Prefs::colouringScheme(Prefs::ForceScheme));

	parent_.updateWidgets();

	// Hide popup
	done();
}

void ViewColourSchemePopup::on_VelocityButton_clicked(bool checked)
{
	Model* model = parent_.aten().currentModelOrFrame();

	if ((!model) || (!checked)) return;

	model->beginUndoState("Change model colour scheme");
	model->setColourScheme(Prefs::VelocityScheme);
	model->endUndoState();

	// Set icon in button
	callMethodSimple("updateButtonIcon", Prefs::colouringScheme(Prefs::VelocityScheme));

	parent_.updateWidgets();

	// Hide popup
	done();
}

void ViewColourSchemePopup::on_BondsButton_clicked(bool checked)
{
	Model* model = parent_.aten().currentModelOrFrame();

	if ((!model) || (!checked)) return;

	model->beginUndoState("Change model colour scheme");
	model->setColourScheme(Prefs::BondsScheme);
	model->endUndoState();

	// Set icon in button
	callMethodSimple("updateButtonIcon", Prefs::colouringScheme(Prefs::BondsScheme));

	parent_.updateWidgets();

	// Hide popup
	done();
}

void ViewColourSchemePopup::on_OwnButton_clicked(bool checked)
{
	Model* model = parent_.aten().currentModelOrFrame();

	if ((!model) || (!checked)) return;

	model->beginUndoState("Change model colour scheme");
	model->setColourScheme(Prefs::OwnScheme);
	model->endUndoState();

	// Set icon in button
	callMethodSimple("updateButtonIcon", Prefs::colouringScheme(Prefs::OwnScheme));

	parent_.updateWidgets();

	// Hide popup
	done();
}
