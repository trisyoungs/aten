/*
	*** Colour Dialog
	*** src/gui/colourdialog.h
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

#ifndef ATEN_COLOURDIALOG_H
#define ATEN_COLOURDIALOG_H

#include "gui/ui_colourdialog.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
/* none */

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Open Grid Dialog
class ColourDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	ColourDialog(QWidget* parent);
	// Main form declaration
	Ui::ColourDialog ui;


	/*
	 * Widget Functions
	 */
	private slots:
	void on_OKButton_clicked(bool checked);
	void on_CancelButton_clicked(bool checked);

	public:
	// Execute dialog
	bool execute(QColor startingColour);


	/*
	 * Signals / Slots
	 */
	private slots:
};

#endif
