/*
	*** Popup Widget - Rebond Functions
	*** src/gui/popupbuildrebond_funcs.cpp
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

#include "gui/popupbuildrebond.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
RebondPopup::RebondPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void RebondPopup::updateControls()
{
	refreshing_ = true;

	ui.ToleranceSpin->setValue(prefs.bondTolerance());
	ui.ToleranceDial->setValue(int(prefs.bondTolerance()*1000.0));

	refreshing_ = false;
}

// Call named method associated to popup
bool RebondPopup::callMethod(QString methodName, ReturnValue& rv)
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

void RebondPopup::on_RebondModelNoAugmentButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ReBond, "i", 0);

	// Update display
	parent_.updateWidgets();

	// Hide popup
	done();
}

void RebondPopup::on_RebondSelectionButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ReBondSelection, "i", 1);

	// Update display
	parent_.updateWidgets();

	// Hide popup
	done();
}

void RebondPopup::on_RebondSelectionNoAugmentButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ReBondSelection, "i", 0);

	// Update display
	parent_.updateWidgets();

	// Hide popup
	done();
}

void RebondPopup::on_RebondPatternsButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ReBondPatterns, "i", 1);

	// Update display
	parent_.updateWidgets();

	// Hide popup
	done();
}

void RebondPopup::on_RebondPatternsNoAugmentButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ReBondPatterns, "i", 0);

	// Update display
	parent_.updateWidgets();

	// Hide popup
	done();
}

void RebondPopup::on_ToleranceDial_valueChanged(int value)
{
	// Convert integer value to real value
	double tolerance = double(value)/1000.0;

	// Set spinbox
	ui.ToleranceSpin->setValue(tolerance);

	// Set prefs value
	prefs.setBondTolerance(tolerance);
}

void RebondPopup::on_ToleranceSpin_valueChanged(double value)
{
	// Convert integer value to real value
	int tolerance = int(value*1000.0);

	// Set slider
	ui.ToleranceDial->setValue(tolerance);

	// Set prefs value
	prefs.setBondTolerance(value);
}
