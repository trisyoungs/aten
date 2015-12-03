/*
	*** Popup Widget - Element Table
	*** src/gui/popupelementtable.h
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

#ifndef ATEN_ELEMENTTABLEPOPUP_H
#define ATEN_ELEMENTTABLEPOPUP_H

#include "gui/ui_popupelementtable.h"
#include "gui/tpopupwidget.hui"
#include "base/namespace.h"
#include <QPushButton>
#include "parser/returnvalue.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class ReturnValue;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Popup Widget - Build/Element/Table
class ElementTablePopup : public TPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	ElementTablePopup(AtenWindow& parent, TMenuButton* buttonParent);
	// Main form declaration
	Ui::ElementTablePopup ui;
	// Update controls (before show()) (virtual)
	void updateControls();
	// Call named method associated to popup
	bool callMethod(QString methodName, ReturnValue& rv);


	/*
	 * Window Functions
	 */
	private slots:
	void ElementButton_clicked(bool checked);


	/*
	 * Local variables
	 */
	private:
	// Current element
	int currentElement_;
	// Array of element buttons
	QList<QPushButton*> elementButtons_;
};

#endif
