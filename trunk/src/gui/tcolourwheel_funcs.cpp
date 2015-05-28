/*
	*** TColourWheel Functions
	*** src/gui/tcolourwheel_funcs.cpp
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

// Based on code from https://github.com/liuyanghejerry/Qt-Plus/tree/master/develop/ColorWheel

#include "gui/tcolourwheel.hui"
#include <QMouseEvent>
#include <QPainter>
#include <QStyleOption>

// Constructor
TColourWheel::TColourWheel(QWidget* parent) : QWidget(parent)
{
	mouseDown_ = false;
	margin_ = 0;
	wheelWidth_ = 16;
	currentRegion_ = TColourWheel::NoRegion;
	currentColour_ = Qt::red;
	currentColour_ = currentColour_.toHsv();
	setCursor(Qt::CrossCursor);

	// Construct conical gradient for wheel
	wheelGradient_ = QConicalGradient(0, 0, 0);
	wheelGradient_.setColorAt(0.0, Qt::red);
	wheelGradient_.setColorAt(60.0/360.0, Qt::yellow);
	wheelGradient_.setColorAt(120.0/360.0, Qt::green);
	wheelGradient_.setColorAt(180.0/360.0, Qt::cyan);
	wheelGradient_.setColorAt(240.0/360.0, Qt::blue);
	wheelGradient_.setColorAt(300.0/360.0, Qt::magenta);
	wheelGradient_.setColorAt(1.0, Qt::red);
}

/*
 * Qt Reimplementations
 */

// Mouse pressed
void TColourWheel::mousePressEvent(QMouseEvent* event)
{
	// Store position
	lastPos_ = event->pos();

	// Check mouse position
	if (wheelRegion_.contains(lastPos_))
	{
		currentRegion_ = TColourWheel::WheelRegion;
		currentColour_ = lastPosColour();
		hueChanged(currentColour_.hue());
	}
	else if (squareRegion_.contains(lastPos_))
	{
		currentRegion_ = TColourWheel::SquareRegion;
		svChanged(lastPosColour());
	}
	else currentRegion_ = TColourWheel::NoRegion;

	mouseDown_ = true;

	update();
}

// Mouse moved
void TColourWheel::mouseMoveEvent(QMouseEvent* event)
{
	// Store mouse position
	lastPos_ = event->pos();

	// If no mouse button pressed, nothing to do
	if (!mouseDown_ ) return;

	// Check mouse position
	if (wheelRegion_.contains(lastPos_) && (currentRegion_ == TColourWheel::WheelRegion))
	{
		QColor colour = lastPosColour();
		hueChanged(colour.hue());
	}
	else if (squareRegion_.contains(lastPos_) && (currentRegion_ == TColourWheel::SquareRegion))
	{
		svChanged(lastPosColour());
	}
	else
	{
        // TODO: due with cursor out of region after press
        //        int length = qMin(width(), height());
        //        QPoint center(length/2, length/2);
        //        int R = qSqrt(qPow(qAbs(lastPos_.x()), 2)
        //                      + qPow(qAbs(lastPos_.y()), 2));
        //        if(inWheel){
        //            int r =  length / 2;
        //            r += qSqrt(qPow(center.x(), 2) + qPow(center.y(), 2));
        //            int x0 = r/R * qAbs(lastPos_.x());
        //            int y0 = r/R * qAbs(lastPos_.y());
        //            QColor color = posColour(QPoint(x0, y0));
        //            hueChanged(color.hue());
        //        }else if(inSquare){
        //            //
        //        }
	}
}

// Mouse released
void TColourWheel::mouseReleaseEvent(QMouseEvent* event)
{
	mouseDown_ = false;
	currentRegion_ = TColourWheel::NoRegion;
}

// Widget resized
void TColourWheel::resizeEvent(QResizeEvent* event)
{
	// Blank wheel pixmap
	compositeImage_ = QPixmap(event->size());
	compositeImage_.fill(palette().background().color());

	// Calculate new positions etc.
	QSize newSize = event->size();
	outerRadius_ = qMin(newSize.width(), newSize.height()) / 2 - margin_;
	centre_.setX(newSize.width()  / 2);
	centre_.setY(newSize.height() / 2);
	squareSize_ = ( (outerRadius_ - wheelWidth_) / sqrt(2.0) ) * 2;

	// Update wheel and square images and regions following new size
	updateWheel(newSize);
	updateSquare(currentColour_.hue());
	updateComposite();

	update();
}

// Paint event
void TColourWheel::paintEvent(QPaintEvent* event)
{
	// Initialise QPainter and style option
	QPainter painter(this);
	QStyleOption opt;
	opt.initFrom(this);

	painter.drawPixmap(0, 0, compositeImage_);
	style()->drawPrimitive(QStyle::PE_Widget, &opt, &painter, this);
}

// Return size hint
QSize TColourWheel::sizeHint() const
{
	return QSize(height(),height());
}

// Return minimum size hint
QSize TColourWheel::minimumSizeHint() const
{
    return QSize(200,200);
}

/*
 * Layout / Control
 */

// Return colour associated to position on widget
QColor TColourWheel::lastPosColour()
{
	// Which region are we currently in?
	if (currentRegion_ == TColourWheel::WheelRegion)
	{
		qreal hue = 0;
// 		int r = qMin(width(), height()) / 2;
		if (lastPos_.x() > centre_.x())
		{
			if(lastPos_.y() < centre_.y())
			{
				//1
				hue = 90 - (atan2( (lastPos_.x() - centre_.x()) , (centre_.y() - lastPos_.y()) )  / 3.14159 / 2 * 360);
			}
			else
			{
				//4
				hue = 270 + (atan2( (lastPos_.x() - centre_.x()) , (lastPos_.y() - centre_.y() ) )  / 3.14159 / 2 * 360);
			}
		}
		else
		{
			if (lastPos_.y() < centre_.y())
			{
				//2
				hue =  90 + (atan2( (centre_.x() - lastPos_.x()) , (centre_.y() - lastPos_.y()) )  / 3.14159 / 2 * 360);
			}
			else
			{
				//3
				hue =  270 - (atan2( (centre_.x() - lastPos_.x()) , (lastPos_.y() - centre_.y() ))  / 3.14159 / 2 * 360);
			}
		}
		hue = hue>359?359:hue;
		hue = hue<0?0:hue;
		return QColor::fromHsv(hue, currentColour_.saturation(), currentColour_.value(), currentColour_.alpha());
	}
	else if (currentRegion_ == TColourWheel::SquareRegion)
	{
		QPoint p(lastPos_.x() - centre_.x() + squareSize_/2, lastPos_.y() - centre_.y() + squareSize_/2); 
		return QColor::fromHsvF(currentColour_.hueF(), p.x()/double(squareSize_), p.y()/double(squareSize_), currentColour_.alphaF());
	}

	return QColor();
}

// Update wheel image and region
void TColourWheel::updateWheel(const QSize widgetSize)
{
	// Initialise style
	QStyleOption option;
	option.initFrom(this);
	//    QStyle::State state = option.state;

	// Set background brush
	QBrush background = option.palette.window();

	// Initialise new image for the wheel
	wheelImage_ = QImage(widgetSize, QImage::Format_ARGB32_Premultiplied);
	wheelImage_.fill(background.color());

	// Draw the wheel
	QPainter painter(&wheelImage_);
	painter.setRenderHint(QPainter::Antialiasing);

	// -- Large circle (gradient)
	painter.translate(centre_);
	QBrush brush(wheelGradient_);
	painter.setPen(Qt::NoPen);
	painter.setBrush(brush);
	painter.drawEllipse(QPoint(0,0), outerRadius_, outerRadius_);

	// -- Small circle (background colour)
	painter.setBrush(background);
	painter.drawEllipse(QPoint(0,0), outerRadius_ -wheelWidth_, outerRadius_ -wheelWidth_);

	// Setup region for wheel
	wheelRegion_ = QRegion(centre_.x(), centre_.y(), outerRadius_*2, outerRadius_*2, QRegion::Ellipse);
	wheelRegion_.translate(-centre_.x(), -centre_.y());
	QRegion subRe = QRegion(centre_.x()+wheelWidth_, centre_.y()+wheelWidth_, (outerRadius_-wheelWidth_)*2, (outerRadius_-wheelWidth_)*2, QRegion::Ellipse);
	subRe.translate(-centre_.x(), -centre_.y());
	wheelRegion_ -= subRe;
}

// Update square (saturation/value) image and region
void TColourWheel::updateSquare(const int currentHue)
{
	// Create a square large enough to contain all 255x255 colour combinations, and set the pixel colours
	QImage square(255, 255, QImage::Format_ARGB32_Premultiplied);
	QColor color;
	QRgb vv;
	for(int i=0;i<255;++i)
	{
		for(int j=0;j<255;++j)
		{
			color = QColor::fromHsv(currentHue, i, j);
			vv = qRgb(color.red(),color.green(),color.blue());
			square.setPixel(i, j, vv);
		}
	}
	squareImage_ = square.scaled(squareSize_, squareSize_);

	// Setup region
	squareRegion_ = QRegion(centre_.x() - squareSize_/2, centre_.y() - squareSize_/2, squareSize_, squareSize_);
}

// Update composite image
void TColourWheel::updateComposite()
{
	const int indicatorSize = 5;
	const int penWidth = 3;

	// Combine wheel and square images into one, and add on indicator marks
	QPainter painter(&compositeImage_);
	painter.drawImage(0, 0, wheelImage_);
	painter.drawImage(squareRegion_.boundingRect().topLeft(), squareImage_);

	// Initialise painter and pen, and translate drawing position to the centre of the widget
	painter.setRenderHint(QPainter::Antialiasing);
	painter.setBrush(Qt::NoBrush);
	QPoint point;
	QPen pen = painter.pen();
	pen.setWidth(penWidth);
	painter.translate(centre_.x(), centre_.y());

	// Draw indicator for saturation / value
	pen.setColor( ((currentColour_.saturation() > 30) || (currentColour_.value() < 50)) ? Qt::white : Qt::black ); 
	painter.setPen(pen);
	point.setX(currentColour_.saturationF()*squareSize_ - (indicatorSize+penWidth)/2 - squareSize_/2);
	point.setY(currentColour_.valueF()*squareSize_ - (indicatorSize+penWidth)/2 - squareSize_/2);
	painter.drawEllipse(point, indicatorSize, indicatorSize);

	// Draw indicator for hue
	pen.setColor( ((currentColour_.hue() > 20) && (currentColour_.hue() < 200)) ? Qt::black : Qt::white );
	painter.rotate(-currentColour_.hue());
	painter.drawEllipse(QPointF(outerRadius_-wheelWidth_/2 , 0.0), indicatorSize, indicatorSize);

	painter.end();
}

// Hue changed, so update
void TColourWheel::hueChanged(const int hue)
{
	// Check for illegal value
	if ((hue < 0) || (hue > 359))
	{
		printf("Illegal hue value (%i) given to TColourWheel::hueChanged().\n", hue);
		return;
	}

	// Update current colour
	int s = currentColour_.saturation();
	int v = currentColour_.value();
	currentColour_.setHsv(hue, s, v, currentColour_.alpha());

	// Update square (saturation / value) image and repaint widget
	if (isVisible())
	{
		updateSquare(hue);
		updateComposite();

// 		repaint();
		update();
	}

	emit colourChanged(currentColour_);
}

// Saturation / value changed, so update
void TColourWheel::svChanged(const QColor newcolor)
{
	// Update colour
	currentColour_.setHsv(currentColour_.hue(), newcolor.saturation(), newcolor.value(), currentColour_.alpha());

	// Update widget
	if (isVisible())
	{
		updateComposite();
// 		repaint();
		update();
	}

	emit colourChanged(currentColour_);
}

/*
 * Colour
 */

// Return colour selected in widget
QColor TColourWheel::currentColour()
{
	return currentColour_;
}

// Set colour selected in widget
void TColourWheel::setColour(const QColor colour)
{
	if (colour == currentColour_) return;

	// Update hue and sv if necessaey
	if (colour.hue() != currentColour_.hue()) hueChanged(colour.hue());
	if (colour.saturation() != currentColour_.saturation() || colour.value() != currentColour_.value()) svChanged(colour);

	// Copy alpha value of source colour, since it will not be correct
	currentColour_.setAlpha(colour.alpha());

	// Redraw widget
	update();

	// Signal that the colour has been changed
	emit colourChanged(currentColour_);
}
