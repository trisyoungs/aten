/*
	*** Popup Widget - Colour Select
	*** src/gui/popupcolour.h
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

#ifndef ATEN_COLOUREPOPUP_H
#define ATEN_COLOURPOPUP_H

#include "gui/ui_popupcolour.h"
#include "gui/tmenubutton.hui"
#include "base/namespace.h"
#include <QPushButton>
#include "parser/returnvalue.h"

// Forward Declarations (Qt)
class AtenWindow;
class QColorDialog;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class ReturnValue;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Popup Widget - Colour
class ColourPopup : public TMenuButtonPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	ColourPopup(AtenWindow& parent, TMenuButton* buttonParent);
	// Main form declaration
	Ui::ColourPopup ui;
	// Show popup, updating any controls as necessary beforehand
	void popup();
	// Call named method associated to popup
	bool callMethod(QString methodName, ReturnValue& rv);


	/*
	 * Qt Functions
	 */
	private slots:
	void on_RedSlider_valueChanged(int value);
	void on_RedSpin_valueChanged(int value);
	void on_GreenSlider_valueChanged(int value);
	void on_GreenSpin_valueChanged(int value);
	void on_BlueSlider_valueChanged(int value);
	void on_BlueSpin_valueChanged(int value);
	void on_HueSlider_valueChanged(int value);
	void on_HueSpin_valueChanged(int value);
	void on_SaturationSlider_valueChanged(int value);
	void on_SaturationSpin_valueChanged(int value);
	void on_ValueSlider_valueChanged(int value);
	void on_ValueSpin_valueChanged(int value);
	void on_AlphaSlider_valueChanged(int value);
	void on_AlphaSpin_valueChanged(int value);
	void on_Wheel_colourChanged(const QColor& colour);


	/*
	 * Local variables
	 */
	private:
	// Selected colour
	QColor currentColour_;

	private:
	// Update parent button's icon
	void updateParentButtonIcon();

	public:
	// Set current colour
	void setCurrentColour(QColor color);
};

#endif
