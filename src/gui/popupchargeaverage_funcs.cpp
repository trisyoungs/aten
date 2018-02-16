/*
	*** Popup Widget - Charge Average Functions
	*** src/gui/popupchargeaverage_funcs.cpp
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

#include "gui/popupchargeaverage.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ChargeAveragePopup::ChargeAveragePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void ChargeAveragePopup::updateControls()
{
	refreshing_ = true;

	Model* m = parent_.aten().currentModelOrFrame();
	ui.SmearSelectionButton->setEnabled(m && (m->nSelected() > 0));

	refreshing_ = false;
}

// Call named method associated to popup
bool ChargeAveragePopup::callMethod(QString methodName, ReturnValue& rv)
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

void ChargeAveragePopup::on_SmearSelectionButton_clicked(bool checked)
{
	// Calculate total charge in current selection
	Model* m = parent_.aten().currentModelOrFrame();
	if (m)
	{
		double totalCharge = 0.0;
		for (RefListItem<Atom,int>* ri = m->selection(); ri != NULL; ri = ri->next)
		{
			Atom* i = ri->item;
			totalCharge += i->charge();
		}
		Messenger::print("Total charge was %f - smeared charge per atom will be %f.\n", totalCharge, totalCharge / m->nSelected());
		totalCharge /= m->nSelected();
		m->beginUndoState("Smear charges in selection");
		for (RefListItem<Atom,int>* ri = m->selection(); ri != NULL; ri = ri->next)
		{
			Atom* i = ri->item;
			m->atomSetCharge(i, totalCharge);
		}
		m->endUndoState();
	}

	// Update display
	parent_.updateWidgets();

	// Hide popup
	done();
}
