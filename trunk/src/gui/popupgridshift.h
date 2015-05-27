/*
	*** Popup Widget - Grid Shift
	*** src/gui/popupgridshift.h
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

#ifndef ATEN_GRIDSHIFTPOPUP_H
#define ATEN_GRIDSHIFTPOPUP_H

#include "gui/ui_popupgridshift.h"
#include "gui/tmenubutton.hui"
#include "parser/returnvalue.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class ReturnValue;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Popup Widget - Grid Shift
class GridShiftPopup : public TMenuButtonPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	GridShiftPopup(AtenWindow& parent, TMenuButton* buttonParent);
	// Main form declaration
	Ui::GridShiftPopup ui;
	// Show popup, updating any controls as necessary beforehand
	void popup();
	// Call named method associated to popup
	bool callMethod(QString methodName, ReturnValue& rv);


	/*
	 * Reimplementations
	 */
	protected:
	void hideEvent(QHideEvent* event) { TMenuButtonPopupWidget::hideEvent(event); }


	/*
	 * Widget Functions
	 */
	private:
	// Alter grid shift amount
	void gridShiftChanged();

	private slots:
	void on_PositiveXButton_clicked(bool checked);
	void on_PositiveYButton_clicked(bool checked);
	void on_PositiveZButton_clicked(bool checked);
	void on_NegativeXButton_clicked(bool checked);
	void on_NegativeYButton_clicked(bool checked);
	void on_NegativeZButton_clicked(bool checked);
	void on_XSpin_valueChanged(int i);
	void on_YSpin_valueChanged(int i);
	void on_ZSpin_valueChanged(int i);
};

#endif
