/*
	*** Colour Dialog
	*** src/gui/colourdialog_funcs.cpp
	Copyright T. Youngs 2016-2016

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

#include "gui/colourdialog.h"

// Constructor
ColourDialog::ColourDialog(QWidget* parent) : QDialog(parent)
{
	ui.setupUi(this);
}

/*
 * Widget Functions
 */

void ColourDialog::on_OKButton_clicked(bool checked)
{
	accept();
}

void ColourDialog::on_CancelButton_clicked(bool checked)
{
	reject();
}

// Execute dialog
bool ColourDialog::execute(QColor startingColour)
{
	ui.ColourWidget->setCurrentColour(startingColour);
	return exec();
}

/*
 * Signals / Slots
 */

