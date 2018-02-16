/*
	*** Popup Widget - Colour Select
	*** src/gui/popupcolour.h
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

#ifndef ATEN_COLOURPOPUP_H
#define ATEN_COLOURPOPUP_H

#include "gui/ui_popupcolour.h"
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

// Popup Widget - Colour
class ColourPopup : public TPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	ColourPopup(AtenWindow& parent, TMenuButton* buttonParent, int colourWidgetOptions = 0);
	// Main form declaration
	Ui::ColourPopup ui;
	// Update controls (before show()) (virtual)
	void updateControls();
	// Call named method associated to popup
	bool callMethod(QString methodName, ReturnValue& rv);


	/*
	 * Qt Functions
	 */
	private slots:
	// Colour in selection widget has changed
	void colourChanged(const QColor colour);


	/*
	 * Local Functions
	 */
	private:
	// Update parent button's icon
	void updateParentButtonIcon( QColor colour );

	public:
	// Set current colour
	void setCurrentColour(QColor color);
};

#endif
