/*
	*** Popup Widget - Grid Colour
	*** src/gui/popupgridcolour_funcs.cpp
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

#include "gui/mainwindow.h"
#include "gui/popupgridcolour.h"
#include <QColorDialog>
#include <QLayout>
#include <QPainter>

// Constructor
GridColourPopup::GridColourPopup(AtenWindow& parent, TMenuButton* buttonParent, bool primary, int colourWidgetOptions) : TPopupWidget(buttonParent), parent_(parent)
{
	ui.setupUi(this);

	connect(ui.ColourWidget, SIGNAL(colourChanged(QColor)), this, SLOT(colourChanged(QColor)));

	ui.ColourWidget->setOptions(colourWidgetOptions);

	setCurrentColour(Qt::blue);

	updateParentButtonIcon(Qt::blue);

	primary_ = primary;
}

// Update controls (before show()) (virtual)
void GridColourPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool GridColourPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "currentColour")
	{
		QColor newColor = ui.ColourWidget->currentColour();
		double colour[4];
		colour[0] = newColor.redF();
		colour[1] = newColor.greenF();
		colour[2] = newColor.blueF();
		colour[3] = newColor.alphaF();
		rv.setArray(VTypes::DoubleData, colour, 4);
		return true;
	}
	else if (methodName == "setCurrentColour")
	{
		bool success;
		QColor newColour;
		newColour.setRedF(rv.asDouble(0, success));
		if (success) newColour.setGreenF(rv.asDouble(1, success));
		if (success) newColour.setBlueF(rv.asDouble(2, success));
		if (success) newColour.setAlphaF(rv.asDouble(3, success));
		if (!success)
		{
			printf("Failed to get colour information from supplied ReturnValue.\n");
			return false;
		}
		setCurrentColour(newColour);

		updateParentButtonIcon(newColour);

		return true;
	}
	else if (methodName == "hideEvent")
	{
		updateParentButtonIcon(ui.ColourWidget->currentColour());
		return true;
	}
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Widget Functions
 */

// Colour in selection widget has changed
void GridColourPopup::colourChanged(const QColor colour)
{
	// Run command
	if (primary_) CommandNode::run(Commands::GridColour, "dddd", colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF());
	else CommandNode::run(Commands::GridColourSecondary, "dddd", colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF());

	// Update icon
	updateParentButtonIcon(colour);

	// Update display
	parent_.updateWidgets();
}

/*
 * Local Variables
 */

// Update parent button's icon
void GridColourPopup::updateParentButtonIcon(QColor colour)
{
	if (!parentMenuButton())
	{
		printf("GridColourPopup: No parent button set, so no icon to update.");
		return;
	}

	QPixmap pixmap(32,32);

	QPainter painter(&pixmap);
	QPen pen;

	// Clear pixmap with white background colour (so that transparent colours are displayed correctly)
// 	painter.setBrush(parentMenuButton()->palette().background());
	painter.setBrush(Qt::white);
	painter.setPen(Qt::NoPen);
	painter.drawRect(-1, -1, 32, 32);

	// Setup gradient - the top-left corner will be the actual opaque colour, and the bottom-right the alpha valued colour
	QLinearGradient linearGrad(QPointF(0, 0), QPointF(16, 16));
	linearGrad.setColorAt(0, QColor(colour.red(), colour.green(), colour.blue(), 255));
	linearGrad.setColorAt(1, colour);
	painter.setBrush(linearGrad);

	// Draw circle
	pen.setWidth(1);
	pen.setColor(Qt::black);
	painter.setPen(pen);
	painter.drawRoundRect(0, 0, 31, 31, 10, 10);

	painter.end();

	parentMenuButton()->setIcon(QIcon(pixmap));
}

// Set current colour
void GridColourPopup::setCurrentColour(QColor color)
{
	ui.ColourWidget->setCurrentColour(color);
}
