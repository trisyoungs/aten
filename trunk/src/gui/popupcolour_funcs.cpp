/*
	*** Popup Widget - Colour
	*** src/gui/popupcolour_funcs.cpp
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

#include "gui/mainwindow.h"
#include "gui/popupcolour.h"
#include <QColorDialog>
#include <QLayout>
#include <QPainter>

// Constructor
ColourPopup::ColourPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void ColourPopup::popup()
{
	refreshing_ = true;

	show();

	setCurrentColour(currentColour_);

	refreshing_ = false;
}

// Call named method associated to popup
bool ColourPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "colour")
	{
		double colour[4];
		colour[0] = currentColour_.redF();
		colour[1] = currentColour_.greenF();
		colour[2] = currentColour_.blueF();
		colour[3] = currentColour_.alphaF();
		rv.setArray(VTypes::DoubleData, colour, 4);
		return true;
	}
	else if (methodName == "setColour")
	{
		bool success;
		double colour[4];
		colour[0] = rv.asDouble(0, success);
		if (success) colour[1] = rv.asDouble(1, success);
		if (success) colour[2] = rv.asDouble(2, success);
		if (success) colour[3] = rv.asDouble(3, success);
		if (!success)
		{
			printf("Failed to get colour information from supplied ReturnValue.\n");
			return false;
		}
		currentColour_.setRgbF(colour[0], colour[1], colour[2], colour[3]);
		setCurrentColour(currentColour_);

		updateParentButtonIcon();

		return true;
	}
	else if (methodName == "hideEvent")
	{
		updateParentButtonIcon();
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

void ColourPopup::on_RedSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setRgb(value, ui.GreenSlider->value(), ui.BlueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_RedSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setRgb(value, ui.GreenSlider->value(), ui.BlueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_GreenSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setRgb(ui.RedSlider->value(), value, ui.BlueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_GreenSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setRgb(ui.RedSlider->value(), value, ui.BlueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_BlueSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setRgb(ui.RedSlider->value(), ui.GreenSlider->value(), value, ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_BlueSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setRgb(ui.RedSlider->value(), ui.GreenSlider->value(), value, ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_HueSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setHsv(value, ui.SaturationSlider->value(), ui.ValueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_HueSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setHsv(value, ui.SaturationSlider->value(), ui.ValueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_SaturationSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setHsv(ui.HueSlider->value(), value, ui.ValueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_SaturationSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setHsv(ui.HueSlider->value(), value, ui.ValueSlider->value(), ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_ValueSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setHsv(ui.HueSlider->value(), ui.SaturationSlider->value(), value, ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_ValueSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColor;
	newColor.setHsv(ui.HueSlider->value(), ui.SaturationSlider->value(), value, ui.AlphaSlider->value());
	setCurrentColour(newColor);
}

void ColourPopup::on_AlphaSlider_valueChanged(int value)
{
	if (refreshing_) return;

	currentColour_.setRgb(ui.RedSlider->value(), ui.GreenSlider->value(), ui.BlueSlider->value(), value);
}

void ColourPopup::on_AlphaSpin_valueChanged(int value)
{
	if (refreshing_) return;

	currentColour_.setRgb(ui.RedSlider->value(), ui.GreenSlider->value(), ui.BlueSlider->value(), value);
}

void ColourPopup::on_Wheel_colourChanged(const QColor& colour)
{
	if (refreshing_) return;

	setCurrentColour(colour);
}

/*
 * Local Variables
 */

// Update parent button's icon
void ColourPopup::updateParentButtonIcon()
{
	QPixmap pixmap(32,32);

	QPainter painter(&pixmap);

	// Grab colour and set brush
	painter.setBrush(currentColour_);

	// Set up pen
	QPen pen;
	pen.setWidth(2);
	pen.setColor(Qt::black);
	
	// Draw circle
	painter.drawRoundRect(0, 0, 30, 30, 10, 10);

	painter.end();

	parentMenuButton()->setIcon(QIcon(pixmap));
}

// Set current colour
void ColourPopup::setCurrentColour(QColor color)
{
	refreshing_ = true;

	currentColour_ = color;

	// Set slider and spin controls
	ui.RedSlider->setValue(color.red());
	ui.RedSpin->setValue(color.red());
	ui.GreenSlider->setValue(color.green());
	ui.GreenSpin->setValue(color.green());
	ui.BlueSlider->setValue(color.blue());
	ui.BlueSpin->setValue(color.blue());
	ui.HueSlider->setValue(color.hsvHue());
	ui.HueSpin->setValue(color.hsvHue());
	ui.SaturationSlider->setValue(color.hsvSaturation());
	ui.SaturationSpin->setValue(color.hsvSaturation());
	ui.ValueSlider->setValue(color.value());
	ui.ValueSpin->setValue(color.value());

	// Update wheel
	ui.Wheel->setColour(color);

	refreshing_ = false;
}
