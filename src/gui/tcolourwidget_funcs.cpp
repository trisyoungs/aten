/*
	*** Colour Selector Widget
	*** src/gui/tcolourwidget_funcs.cpp
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

#include "gui/tcolourwidget.hui"
#include "gui/tcolourwheel.hui"
#include <QGridLayout>
#include <QLabel>
#include <QSpinBox>
#include <QSlider>
#include <QLine>

// Constructor
TColourWidget::TColourWidget(QWidget* parent) : QWidget(parent)
{
	// Create widgets
	QGridLayout* gridLayout = new QGridLayout(this);
	int row = 0;

	// RGB Section
	redSpin_ = new QSpinBox(this);
	redSpin_->setRange(0, 255);
	redSlider_ = new QSlider(this);
	redSlider_->setRange(0, 255);
	gridLayout->addWidget(new QLabel("R"), row, 0);
	gridLayout->addWidget(redSlider_, row, 1);
	gridLayout->addWidget(redSpin_, row, 2);
	++row;
	greenSpin_ = new QSpinBox(this);
	greenSpin_->setRange(0, 255);
	greenSlider_ = new QSlider(this);
	greenSlider_->setRange(0, 255);
	gridLayout->addWidget(new QLabel("G"), row, 0);
	gridLayout->addWidget(greenSlider_, row, 1);
	gridLayout->addWidget(greenSpin_, row, 2);
	++row;
	blueSpin_ = new QSpinBox(this);
	blueSpin_->setRange(0, 255);
	blueSlider_ = new QSlider(this);
	blueSlider_->setRange(0, 255);
	gridLayout->addWidget(new QLabel("B"), row, 0);
	gridLayout->addWidget(blueSlider_, row, 1);
	gridLayout->addWidget(blueSpin_, row, 2);

	// HSV Section
	++row;
	hueSpin_ = new QSpinBox(this);
	hueSpin_->setRange(0, 359);
	hueSlider_ = new QSlider(this);
	hueSlider_->setRange(0, 359);
	gridLayout->addWidget(new QLabel("H"), row, 0);
	gridLayout->addWidget(hueSlider_, row, 1);
	gridLayout->addWidget(hueSpin_, row, 2);
	++row;
	saturationSpin_ = new QSpinBox(this);
	saturationSpin_->setRange(0, 255);
	saturationSlider_ = new QSlider(this);
	saturationSlider_->setRange(0, 255);
	gridLayout->addWidget(new QLabel("S"), row, 0);
	gridLayout->addWidget(saturationSlider_, row, 1);
	gridLayout->addWidget(saturationSpin_, row, 2);
	++row;
	valueSpin_ = new QSpinBox(this);
	valueSpin_->setRange(0, 255);
	valueSlider_ = new QSlider(this);
	valueSlider_->setRange(0, 255);
	gridLayout->addWidget(new QLabel("V"), row, 0);
	gridLayout->addWidget(valueSlider_, row, 1);
	gridLayout->addWidget(valueSpin_, row, 2);

	// Alpha Section
	++row;
	alphaSpin_ = new QSpinBox(this);
	alphaSpin_->setRange(0, 255);
	alphaSlider_ = new QSlider(this);
	alphaSlider_->setRange(0, 255);
	gridLayout->addWidget(new QLabel("A"), row, 0);
	gridLayout->addWidget(alphaSlider_, row, 1);
	gridLayout->addWidget(alphaSpin_, row, 2);

	// Colour Wheel
	colourWheel_ = new TColourWheel(this);
	gridLayout->addWidget(colourWheel_, 0, 3, 1, 9);

	// Tweak based on options
	if (options_&NoAlphaOption)
	{
// 		ui.AlphaLine->setVisible(false);
// 		alphaSlider_->setVisible(false);
// 		alphaSpin_->setVisible(false);
// 		alphaSpin_->setValue(255);
	}

	// Set basic default colour
	setCurrentColour(Qt::blue);
}

// Update controls (before show()) (virtual)
void TColourWidget::updateControls()
{
	refreshing_ = true;

	setCurrentColour(currentColour_);

	refreshing_ = false;
}

/*
 * Widget Functions
 */

void TColourWidget::on_RedSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(value, greenSlider_->value(), blueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_RedSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(value, greenSlider_->value(), blueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_GreenSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), value, blueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_GreenSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), value, blueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_BlueSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), greenSlider_->value(), value, alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_BlueSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), greenSlider_->value(), value, alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_HueSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(value, saturationSlider_->value(), valueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_HueSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(value, saturationSlider_->value(), valueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_SaturationSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(hueSlider_->value(), value, valueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_SaturationSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(hueSlider_->value(), value, valueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_ValueSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(hueSlider_->value(), saturationSlider_->value(), value, alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_ValueSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(hueSlider_->value(), saturationSlider_->value(), value, alphaSlider_->value());
	setCurrentColour(newColour);
}

void TColourWidget::on_AlphaSlider_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), greenSlider_->value(), blueSlider_->value(), value);
	setCurrentColour(newColour);
}

void TColourWidget::on_AlphaSpin_valueChanged(int value)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), greenSlider_->value(), blueSlider_->value(), value);
	setCurrentColour(newColour);
}

void TColourWidget::on_Wheel_colourChanged(const QColor& colour)
{
	if (refreshing_) return;

	setCurrentColour(colour);
}

/*
 * Local Variables
 */

// Set current colour
void TColourWidget::setCurrentColour(QColor color)
{
	refreshing_ = true;

	currentColour_ = color;

	// Set slider and spin controls
	redSlider_->setValue(color.red());
	redSpin_->setValue(color.red());
	greenSlider_->setValue(color.green());
	greenSpin_->setValue(color.green());
	blueSlider_->setValue(color.blue());
	blueSpin_->setValue(color.blue());
	hueSlider_->setValue(color.hsvHue());
	hueSpin_->setValue(color.hsvHue());
	saturationSlider_->setValue(color.hsvSaturation());
	saturationSpin_->setValue(color.hsvSaturation());
	valueSlider_->setValue(color.value());
	valueSpin_->setValue(color.value());
	alphaSlider_->setValue(color.alpha());
	alphaSpin_->setValue(color.alpha());

	// Update wheel
	colourWheel_->setColour(color);

	refreshing_ = false;
}
