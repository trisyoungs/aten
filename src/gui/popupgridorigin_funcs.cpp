/*
	*** Popup Widget - Grid Origin Functions
	*** src/gui/popupgridorigin_funcs.cpp
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

#include "gui/popupgridorigin.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "../base/grid.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridOriginPopup::GridOriginPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Set range limits on spin boxes
	ui.XSpin->setRange(false, 0.0, false, 0.0);
	ui.YSpin->setRange(false, 0.0, false, 0.0);
	ui.ZSpin->setRange(false, 0.0, false, 0.0);
}

// Update controls (before show()) (virtual)
void GridOriginPopup::updateControls()
{
	// Update angles in spin boxes
	refreshing_ = true;

	// Get current model
	Grid* grid = parent_.aten().current().g;
	if (grid)
	{
		ui.XSpin->setValue(grid->origin().x);
		ui.YSpin->setValue(grid->origin().y);
		ui.ZSpin->setValue(grid->origin().z);
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool GridOriginPopup::callMethod(QString methodName, ReturnValue& rv)
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

void GridOriginPopup::setCurrentOrigin()
{
	// Get current model and set new angle in cell
	Model* model = parent_.aten().currentModelOrFrame();
	if (!model) return;

	// Get the cell vectors from the GridOriginPopup widget
	CommandNode::run(Commands::GridOrigin, "ddd", ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
}

void GridOriginPopup::on_XSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentOrigin();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridOriginPopup::on_YSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentOrigin();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridOriginPopup::on_ZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentOrigin();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}
