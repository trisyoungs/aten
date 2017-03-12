/*
	*** Colour Selector Widget
	*** src/gui/tcolourwidget_funcs.cpp
	Copyright T. Youngs 2016-2017

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
#include <QFrame>

// Constructor
TColourWidget::TColourWidget(QWidget* parent) : QWidget(parent)
{
	// Create widgets
	QGridLayout* gridLayout = new QGridLayout(this);
	gridLayout->setSpacing(2);
	int row = 0;

	// RGB Section
	redSpin_ = new QSpinBox(this);
	redSpin_->setRange(0, 255);
	redSlider_ = new QSlider(this);
	redSlider_->setRange(0, 255);
	redSlider_->setOrientation(Qt::Horizontal);
	connect(redSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSpin(int)));
	connect(redSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSlider(int)));
	gridLayout->addWidget(new QLabel("R"), row, 0);
	gridLayout->addWidget(redSlider_, row, 1);
	gridLayout->addWidget(redSpin_, row, 2);
	++row;
	greenSpin_ = new QSpinBox(this);
	greenSpin_->setRange(0, 255);
	greenSlider_ = new QSlider(this);
	greenSlider_->setRange(0, 255);
	greenSlider_->setOrientation(Qt::Horizontal);
	connect(greenSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSpin(int)));
	connect(greenSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSlider(int)));
	gridLayout->addWidget(new QLabel("G"), row, 0);
	gridLayout->addWidget(greenSlider_, row, 1);
	gridLayout->addWidget(greenSpin_, row, 2);
	++row;
	blueSpin_ = new QSpinBox(this);
	blueSpin_->setRange(0, 255);
	blueSlider_ = new QSlider(this);
	blueSlider_->setRange(0, 255);
	blueSlider_->setOrientation(Qt::Horizontal);
	connect(blueSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSpin(int)));
	connect(blueSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSlider(int)));
	gridLayout->addWidget(new QLabel("B"), row, 0);
	gridLayout->addWidget(blueSlider_, row, 1);
	gridLayout->addWidget(blueSpin_, row, 2);

	// HSV Section
	++row;
	QFrame* line = new QFrame();
	line->setGeometry(QRect(0, 0, 1, 2));
	line->setFrameShape(QFrame::HLine);
	line->setFrameShadow(QFrame::Sunken);
	gridLayout->addWidget(line, row, 0, 1, 3);
	++row;
	hueSpin_ = new QSpinBox(this);
	hueSpin_->setRange(0, 359);
	hueSlider_ = new QSlider(this);
	hueSlider_->setRange(0, 359);
	hueSlider_->setOrientation(Qt::Horizontal);
	connect(hueSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromHsvSpin(int)));
	connect(hueSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromHsvSlider(int)));
	gridLayout->addWidget(new QLabel("H"), row, 0);
	gridLayout->addWidget(hueSlider_, row, 1);
	gridLayout->addWidget(hueSpin_, row, 2);
	++row;
	saturationSpin_ = new QSpinBox(this);
	saturationSpin_->setRange(0, 255);
	saturationSlider_ = new QSlider(this);
	saturationSlider_->setRange(0, 255);
	saturationSlider_->setOrientation(Qt::Horizontal);
	connect(saturationSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromHsvSpin(int)));
	connect(saturationSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromHsvSlider(int)));
	gridLayout->addWidget(new QLabel("S"), row, 0);
	gridLayout->addWidget(saturationSlider_, row, 1);
	gridLayout->addWidget(saturationSpin_, row, 2);
	++row;
	valueSpin_ = new QSpinBox(this);
	valueSpin_->setRange(0, 255);
	valueSlider_ = new QSlider(this);
	valueSlider_->setRange(0, 255);
	valueSlider_->setOrientation(Qt::Horizontal);
	connect(valueSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromHsvSpin(int)));
	connect(valueSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromHsvSlider(int)));
	gridLayout->addWidget(new QLabel("V"), row, 0);
	gridLayout->addWidget(valueSlider_, row, 1);
	gridLayout->addWidget(valueSpin_, row, 2);

	// Alpha Section
	++row;
	alphaLine_ = new QFrame();
	alphaLine_->setGeometry(QRect(0, 0, 1, 2));
	alphaLine_->setFrameShape(QFrame::HLine);
	alphaLine_->setFrameShadow(QFrame::Sunken);
	gridLayout->addWidget(alphaLine_, row, 0, 1, 3);
	++row;
	alphaSpin_ = new QSpinBox(this);
	alphaSpin_->setRange(0, 255);
	alphaSlider_ = new QSlider(this);
	alphaSlider_->setRange(0, 255);
	alphaSlider_->setOrientation(Qt::Horizontal);
	connect(alphaSpin_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSpin(int)));
	connect(alphaSlider_, SIGNAL(valueChanged(int)), this, SLOT(updateColourFromRgbSlider(int)));
	gridLayout->addWidget(new QLabel("A"), row, 0);
	gridLayout->addWidget(alphaSlider_, row, 1);
	gridLayout->addWidget(alphaSpin_, row, 2);

	// Colour Wheel
	colourWheel_ = new TColourWheel(this);
	gridLayout->addWidget(colourWheel_, 0, 3, 9, 1);
	connect(colourWheel_, SIGNAL(colourChanged(QColor)), this, SLOT(updateColourFromWheel(QColor)));

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

// Set options
void TColourWidget::setOptions(int options)
{
	// Tweak appearance based on options
	if (options&NoAlphaOption)
	{
		alphaLine_->setVisible(false);
		alphaSlider_->setVisible(false);
		alphaSpin_->setVisible(false);
		alphaSpin_->setValue(255);
	}
}

/*
 * Widget Functions
 */

void TColourWidget::updateColourFromRgbSpin(int dummy)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSpin_->value(), greenSpin_->value(), blueSpin_->value(), alphaSpin_->value());
	setCurrentColour(newColour);

	emit(colourChanged(currentColour_));
}

void TColourWidget::updateColourFromRgbSlider(int dummy)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setRgb(redSlider_->value(), greenSlider_->value(), blueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);

	emit(colourChanged(currentColour_));
}

void TColourWidget::updateColourFromHsvSpin(int)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(hueSpin_->value(), saturationSpin_->value(), valueSpin_->value(), alphaSpin_->value());
	setCurrentColour(newColour);

	emit(colourChanged(currentColour_));
}

void TColourWidget::updateColourFromHsvSlider(int)
{
	if (refreshing_) return;

	QColor newColour;
	newColour.setHsv(hueSlider_->value(), saturationSlider_->value(), valueSlider_->value(), alphaSlider_->value());
	setCurrentColour(newColour);

	emit(colourChanged(currentColour_));
}

void TColourWidget::updateColourFromWheel(const QColor colour)
{
	if (refreshing_) return;

	setCurrentColour(colour);

	emit(colourChanged(currentColour_));
}

/*
 * Local Variables
 */

// Return current colour
QColor TColourWidget::currentColour()
{
	return currentColour_;
}

// Set current colour
void TColourWidget::setCurrentColour(QColor colour)
{
	refreshing_ = true;

	currentColour_ = colour;

	// Set slider and spin controls
	redSlider_->setValue(currentColour_.red());
	redSpin_->setValue(currentColour_.red());
	greenSlider_->setValue(currentColour_.green());
	greenSpin_->setValue(currentColour_.green());
	blueSlider_->setValue(currentColour_.blue());
	blueSpin_->setValue(currentColour_.blue());
	hueSlider_->setValue(currentColour_.hsvHue());
	hueSpin_->setValue(currentColour_.hsvHue());
	saturationSlider_->setValue(currentColour_.hsvSaturation());
	saturationSpin_->setValue(currentColour_.hsvSaturation());
	valueSlider_->setValue(currentColour_.value());
	valueSpin_->setValue(currentColour_.value());
	alphaSlider_->setValue(currentColour_.alpha());
	alphaSpin_->setValue(currentColour_.alpha());

	// Update wheel
	colourWheel_->setColour(currentColour_);

	refreshing_ = false;
}
