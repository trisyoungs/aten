/*
	*** Popup Widget - Grid Set Functions
	*** src/gui/popupgridset_funcs.cpp
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

#include "gui/popupgridset.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridSetPopup::GridSetPopup(AtenWindow& parent, TMenuButton* buttonParent, bool primary) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	primary_ = primary;
}

// Update controls (before show()) (virtual)
void GridSetPopup::updateControls()
{
	// Get current grid
	Grid* currentGrid;
	if (!parent_.aten().currentGrid(currentGrid)) return;

	refreshing_ = true;

	ui.GridMinimumLabel->setText(QString::number(currentGrid->minimum()));
	ui.GridMaximumLabel->setText(QString::number(currentGrid->maximum()));
	ui.GridNPointsLabel->setText(QString::number(currentGrid->nPoints()));
	ui.GridTotalPositiveSumLabel->setText(QString::number(currentGrid->totalPositiveSum()));
	ui.GridTotalNegativeSumLabel->setText(QString::number(currentGrid->totalNegativeSum()));
	ui.GridTotalAbsoluteLabel->setText(QString::number(currentGrid->totalAbsoluteSum()));
	if (primary_)
	{
		ui.ViewPercentageLabel->setText(QString::number(100.0*currentGrid->partialPrimarySum()/currentGrid->totalAbsoluteSum()) + "%");
		ui.ViewPercentageSpin->setValue(100.0*currentGrid->partialPrimarySum()/currentGrid->totalAbsoluteSum());
	}
	else
	{
		ui.ViewPercentageLabel->setText(QString::number(100.0*currentGrid->partialSecondarySum()/currentGrid->totalAbsoluteSum()) + "%");
		ui.ViewPercentageSpin->setValue(100.0*currentGrid->partialSecondarySum()/currentGrid->totalAbsoluteSum());
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool GridSetPopup::callMethod(QString methodName, ReturnValue& rv)
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

void GridSetPopup::on_SetDensityMultipleButton_clicked(bool checked)
{
	// Run command
	if (primary_) CommandNode::run(Commands::GridCutoff, "d", ui.NumberDensitySpin->value()*ui.MultipleSpin->value());
	else CommandNode::run(Commands::GridCutoffSecondary, "d", ui.NumberDensitySpin->value()*ui.MultipleSpin->value());

	// Update display
	parent_.updateWidgets(AtenWindow::GridsPanelTarget);
}

void GridSetPopup::on_ViewPercentageSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Run command
	ReturnValue rv;
	if (primary_) rv = CommandNode::run(Commands::GridViewPercentage, "d", ui.ViewPercentageSpin->value());
	else rv = CommandNode::run(Commands::GridViewPercentageSecondary, "d", ui.ViewPercentageSpin->value());

	// Set new density value (if the command was successful)
	if (rv.asBool())
	{
		updateControls();

		// Update display
		parent_.updateWidgets(AtenWindow::GridsPanelTarget);
	}
}
