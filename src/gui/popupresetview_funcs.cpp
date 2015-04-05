/*
	*** Popup Widget - ResetView Functions
	*** src/gui/popupresetview_funcs.cpp
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

#include "gui/popupresetview.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ResetViewPopup::ResetViewPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void ResetViewPopup::popup()
{
	show();
}

/*
 * Widget Functions
 */

// Set view along cartesian axis supplied
void ResetViewPopup::setCartesianView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified axis
	parent_.aten().currentModelOrFrame()->viewAlong(x,y,z);
	parent_.postRedisplay();
}

// Set view along cell axis supplied
void ResetViewPopup::setCellView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified cell axis
	parent_.aten().currentModelOrFrame()->viewAlongCell(x,y,z);
	parent_.postRedisplay();
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
