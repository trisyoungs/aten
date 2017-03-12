/*
	*** Popup Widget - View Reset Functions
	*** src/gui/popupviewreset_funcs.cpp
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

#include "gui/popupviewreset.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ResetViewPopup::ResetViewPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void ResetViewPopup::updateControls()
{
	refreshing_ = true;

	bool periodic = (parent_.aten().currentModelOrFrame() ? parent_.aten().currentModelOrFrame()->isPeriodic() : false);

	// Set enabled state of Cell-based view buttons
	ui.CellNegativeXButton->setEnabled(periodic);
	ui.CellNegativeYButton->setEnabled(periodic);
	ui.CellNegativeZButton->setEnabled(periodic);
	ui.CellPositiveXButton->setEnabled(periodic);
	ui.CellPositiveYButton->setEnabled(periodic);
	ui.CellPositiveZButton->setEnabled(periodic);

	refreshing_ = false;
}

// Call named method associated to popup
bool ResetViewPopup::callMethod(QString methodName, ReturnValue& rv)
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

// Set view along cartesian axis supplied
void ResetViewPopup::setCartesianView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified axis
	parent_.aten().currentModelOrFrame()->viewAlong(x,y,z);

	parent_.updateWidgets();

	done();
}

// Set view along cell axis supplied
void ResetViewPopup::setCellView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified cell axis
	parent_.aten().currentModelOrFrame()->viewAlongCell(x,y,z);

	parent_.updateWidgets();

	done();
}

void ResetViewPopup::on_CartesianPositiveXButton_clicked(bool checked)
{
	 setCartesianView(1,0,0);
}

void ResetViewPopup::on_CartesianPositiveYButton_clicked(bool checked)
{
	 setCartesianView(0,1,0);
}

void ResetViewPopup::on_CartesianPositiveZButton_clicked(bool checked)
{
	 setCartesianView(0,0,1);
}

void ResetViewPopup::on_CartesianNegativeXButton_clicked(bool checked)
{
	 setCartesianView(-1,0,0);
}

void ResetViewPopup::on_CartesianNegativeYButton_clicked(bool checked)
{
	 setCartesianView(0,-1,0);
}

void ResetViewPopup::on_CartesianNegativeZButton_clicked(bool checked)
{
	 setCartesianView(0,0,-1);
}

void ResetViewPopup::on_CellNegativeXButton_clicked(bool checked)
{
	 setCellView(1,0,0);
}

void ResetViewPopup::on_CellNegativeYButton_clicked(bool checked)
{
	 setCellView(0,1,0);
}

void ResetViewPopup::on_CellNegativeZButton_clicked(bool checked)
{
	 setCellView(0,0,1);
}

void ResetViewPopup::on_CellPositiveXButton_clicked(bool checked)
{
	 setCellView(-1,0,0);
}

void ResetViewPopup::on_CellPositiveYButton_clicked(bool checked)
{
	 setCellView(0,-1,0);
}

void ResetViewPopup::on_CellPositiveZButton_clicked(bool checked)
{
	 setCellView(0,0,-1);
}
