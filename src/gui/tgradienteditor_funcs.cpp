/*
	*** Gradient Editor
	*** src/gui/tgradienteditor_funcs.cpp
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

#include "gui/tgradienteditor.hui"
#include "gui/texponentialspin.hui"
#include "gui/colourdialog.h"
#include <QPainter>
#include <QStyle>
#include <QStyleOption>
#include <QMouseEvent>

int gradientBarWidth_ = 32;
int handleRadius_ = 4;

// Constructor
TGradientEditor::TGradientEditor(QWidget* parent) : QWidget(parent)
{
	// Private variables
	currentRegion_ = TGradientEditor::NoRegion;
	hoverColourScalePoint_ = NULL;
	currentColourScalePoint_ = NULL;
}

// Convert gradient bar position to colourscale value
double TGradientEditor::gradientBarValue(QPoint pos)
{
	if (colourScale_.nPoints() == 0) return 0.0;
	double frac = 1.0 - (pos.y() - gradientBarRegion_.boundingRect().top()) / double(gradientBarRegion_.boundingRect().height());
//  		printf("delta = %i, frac = %f\n", delta, delta / double(gradientBarRegion_.boundingRect().height()));
	return ((colourScale_.lastPoint()->value() - colourScale_.firstPoint()->value()) * frac + colourScale_.firstPoint()->value());
}

// Return ColourScalePoint corresponding to handle under mouse (if any)
ColourScalePoint* TGradientEditor::handleUnderMouse(QPoint pos)
{
	// Did we hit a handle?
	if (colourScale_.nPoints() > 0)
	{
		// Check x delta first
		if (abs(lastPos_.x() - handleRegion_.boundingRect().center().x()) > handleRadius_) return NULL;

		// Check distance from centre of handles...
		double zero = colourScale_.firstPoint()->value();
		double span = colourScale_.lastPoint()->value() - zero;
		for (ColourScalePoint* csp = colourScale_.firstPoint(); csp != NULL; csp = csp->next)
		{
			// Work out fractional position of colourscale value and get centre coordinates of handle
			double fracy = (1.0 - (csp->value() - zero) / span);
			int y = fracy * gradientBarRegion_.boundingRect().height() + gradientBarRegion_.boundingRect().top();
			printf("Current y = %i, handle %p y = %i\n", lastPos_.y(), csp, y);
			if (abs(y - pos.y()) <= handleRadius_) return csp;
		}
	}

	return NULL;
}

// Update QGradient for display
void TGradientEditor::updateGradient()
{
	// Setup QGradient - in ObjectBoundingMode, 0.0 = top of rectangle, and 1.0 is bottom
	gradient_ = QLinearGradient(0.0, 1.0, 0.0, 0.0);
	gradient_.setCoordinateMode(QGradient::ObjectBoundingMode);

	// Add points
	if (colourScale_.nPoints() == 0)
	{
		gradient_.setColorAt(0.0, QColor(0,0,0));
		gradient_.setColorAt(1.0, QColor(0,0,0));
	}
	else if (colourScale_.nPoints() == 1)
	{
		gradient_.setColorAt(0.0, colourScale_.firstPoint()->colourAsQColor());
		gradient_.setColorAt(1.0, colourScale_.firstPoint()->colourAsQColor());
	}
	else
	{
		double zero = colourScale_.firstPoint()->value();
		double span = colourScale_.lastPoint()->value() - zero;
		for (ColourScalePoint* csp = colourScale_.firstPoint(); csp != NULL; csp = csp->next) gradient_.setColorAt((csp->value() - zero) / span, csp->colourAsQColor());
	}
}

// Set local colourscale
void TGradientEditor::setColourScale(const ColourScale& colourScale)
{
	// If the source colourscale uses deltaHSV_, we must construct an interpolated gradient manually since QGradient doesn't support HSV interpolation
// 	if (colourScale.useHSV())
// 	{
// 		colourScale_.clear();
// 		colourScale_.setUseHSV(false);
// 		if (colourScale.nPoints() > 0)
// 		{
// 			const int nPoints = 101;
// 			double value = colourScale.firstPoint()->value();
// 			double delta = (colourScale.lastPoint()->value() - value) / nPoints;
// 			for (int n=0; n<nPoints; ++n)
// 			{
// 				colourScale_.addPoint(value, colourScale.colour(value));
// 				value += delta;
// 			}
// 		}
// 	}
// 	else colourScale_ = colourScale;
	colourScale_ = colourScale;

	updateGradient();

	repaint();
}

/*
 * Layout of widget is as follows:
 *   ___________________________
 *  |                    ___    |
 *  |       1.23456789 -|   | O |   =  MARGIN | TEXTLABEL | MARGIN | GRADIENTBAR | MARGIN | HANDLE | MARGIN 
 *  |                   |   |   |
 */

// Paint event callback
void TGradientEditor::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);

	// Get the size of the textrect we need
	QRect masterTextRect = style()->itemTextRect(fontMetrics(), QRect(), Qt::AlignRight | Qt::AlignVCenter, true, QString::number(-0.123456, 'e', 6));
	int margin = masterTextRect.height()*0.5;
	masterTextRect.setLeft(margin);
	masterTextRect.setRight(width() - gradientBarWidth_ - 3*margin - handleRadius_*2);

	// Set up the basic handle style option
	QStyleOption handleOption;
	handleOption.rect.setRight(width()-margin);
	handleOption.rect.setLeft(width()-margin-handleRadius_*2);
	handleOption.rect.setHeight(masterTextRect.height());

	// Draw gradient bar
	QBrush brush(gradient_);
	QPen pen(Qt::black);
	QRect gradientRect;
	gradientRect.setHeight(height()-2*margin);
	gradientRect.setWidth(gradientBarWidth_);
	gradientRect.moveTop(margin);
	gradientRect.moveRight(width() - 2*margin - handleRadius_*2);
	painter.setBrush(brush);
	painter.setPen(pen);
	painter.drawRect(gradientRect);

	// Define regions
	gradientBarRegion_ = QRegion(gradientRect);
	// -- Handle region spans top to bottom of the widget (i.e. excluding margins)
	handleRegion_ = QRegion(QRect(width() - margin - 2*handleRadius_, 0, 2*handleRadius_, height()));

	// Draw text and line elements
	if (colourScale_.nPoints() > 0)
	{
		double zero = colourScale_.firstPoint()->value();
		double span = colourScale_.lastPoint()->value() - zero;
		for (ColourScalePoint* csp = colourScale_.firstPoint(); csp != NULL; csp = csp->next)
		{
			int y = (1.0 - (csp->value() - zero) / span) * (height() - 2*margin) + margin;
			QString numberText = QString::number(csp->value(), 'e', 6);
			masterTextRect.moveBottom(y + 0.5*masterTextRect.height());
			painter.setPen(palette().text().color());
			style()->drawItemText(&painter, masterTextRect, Qt::AlignRight | Qt::AlignVCenter, palette(), true, numberText);
			painter.drawLine(gradientRect.left()-margin, y, gradientRect.right()+margin, y);

			// Draw handle for this point
			handleOption.rect.moveBottom(y + 0.5*masterTextRect.height());
			if (csp == currentColourScalePoint_) handleOption.state = QStyle::State_Enabled;
			else if (csp == hoverColourScalePoint_) handleOption.state = QStyle::State_MouseOver;
			else handleOption.state = 0;
			drawHandle(handleOption, painter);
		}
	}

	painter.end();
}

void TGradientEditor::drawHandle(QStyleOption& styleOption, QPainter& painter)
{
	// Set up colours and deltas for circles
	QColor colour1, colour2;
	if (!(styleOption.state & QStyle::State_Enabled))
	{
		colour1 = styleOption.palette.light().color();
		colour2 = styleOption.palette.mid().color();
	}
	else if (styleOption.state & QStyle::State_MouseOver)
	{
		colour1 = styleOption.palette.mid().color();
		colour2 = styleOption.palette.buttonText().color();
	}
	else
	{
		colour1 = styleOption.palette.mid().color();
		colour2 = styleOption.palette.dark().color();
	}

	// Draw the first circle (offset by 1px down and right)
	painter.translate(1, 1);
	painter.setBrush(colour1);
	painter.setPen(colour1);
	painter.drawEllipse(styleOption.rect.center(), handleRadius_, handleRadius_);

	// Draw second circle (without offset)
	painter.translate(-1, -1);
	painter.setBrush(colour2);
	painter.setPen(colour2);
	painter.drawEllipse(styleOption.rect.center(), handleRadius_, handleRadius_);
}

// Size hint
QSize TGradientEditor::sizeHint() const
{
	QRect masterTextRect = style()->itemTextRect(fontMetrics(), QRect(), Qt::AlignRight | Qt::AlignVCenter, true, QString::number(-0.123456, 'e', 6));
	int margin = masterTextRect.height()*0.5;

	return QSize(gradientBarWidth_ + masterTextRect.width() + handleRadius_*2 + 4*margin, 100);
}

// Minimum size hint
QSize TGradientEditor::minimumSizeHint() const
{
	QRect masterTextRect = style()->itemTextRect(fontMetrics(), QRect(), Qt::AlignRight | Qt::AlignVCenter, true, QString::number(-0.123456, 'e', 6));
	int margin = masterTextRect.height()*0.5;

	return QSize(gradientBarWidth_ + masterTextRect.width() + handleRadius_*2 + 4*margin, 100);
}


// Mouse pressed
void TGradientEditor::mousePressEvent(QMouseEvent* event)
{
	// Store position
	lastPos_ = event->pos();

	// Check mouse position
	if (gradientBarRegion_.contains(lastPos_))
	{
		currentRegion_ = TGradientEditor::GradientRegion;
		printf("Gradient region pressed.\n");
	}
	else if (handleRegion_.contains(lastPos_))
	{
		currentRegion_ = TGradientEditor::HandleRegion;
		currentColourScalePoint_ = handleUnderMouse(lastPos_);
		printf("Handle region pressed.\n");
	}
	else currentRegion_ = TGradientEditor::NoRegion;

	mouseDown_ = true;

	update();
}

// Mouse moved
void TGradientEditor::mouseMoveEvent(QMouseEvent* event)
{
	// Store mouse position
	lastPos_ = event->pos();

	// If no mouse button pressed, nothing to do
	if (!mouseDown_ ) return;

	// Check mouse position
	if (gradientBarRegion_.contains(lastPos_) && (currentRegion_ == TGradientEditor::GradientRegion))
	{
		gradientBarValue(lastPos_);
	}
	else if (currentColourScalePoint_)
	{
		// Move the colourscale point
		printf("New value for colourscale point = %f\n", gradientBarValue(lastPos_));
		colourScale_.setValue(currentColourScalePoint_, gradientBarValue(lastPos_));

		updateGradient();

		repaint();
	}
}

// Mouse released
void TGradientEditor::mouseReleaseEvent(QMouseEvent* event)
{
	mouseDown_ = false;
	currentRegion_ = TGradientEditor::NoRegion;
	currentColourScalePoint_ = NULL;
}

void TGradientEditor::mouseDoubleClickEvent(QMouseEvent* event)
{
	// Store mouse position
	lastPos_ = event->pos();

	// Check mouse position
	if (gradientBarRegion_.contains(lastPos_))
	{
		// Gradient bar was double-clicked, so add a new point at this position
		double clickedValue = gradientBarValue(lastPos_);
		QColor clickedColour = colourScale_.colourAsQColor(clickedValue);
		colourScale_.addPoint(clickedValue, clickedColour);
	}
	else if (handleRegion_.contains(lastPos_))
	{
		currentColourScalePoint_ = handleUnderMouse(lastPos_);
		printf("Double clicked a handle.\n");
		if (currentColourScalePoint_)
		{
			ColourDialog colourDialog(this);
			if (colourDialog.execute(currentColourScalePoint_->colourAsQColor()))
			{
				colourScale_.setColour(currentColourScalePoint_, colourDialog.selectedColour());

				updateGradient();

				repaint();
			}
		}
	}

	currentColourScalePoint_ = NULL;
}