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
TColourWheel::TColourWheel(QWidget* parent) :
    QWidget(parent),
    initSize(200,200),
    mouseDown(false),
    margin(0),
    wheelWidth(16),
    current(Qt::red),
    inWheel(false),
    inSquare(false)
{
    //    resize(initSize);
    current = current.toHsv();
//    setMinimumSize(200,200);
    setCursor(Qt::CrossCursor);
}

QColor TColourWheel::colour()
{
    return current;
}

void TColourWheel::setColour(const QColor& colour)
{
    if(colour == current) return;
    if(colour.hue() != current.hue()){
        hueChanged(colour.hue());
    }

    if( colour.saturation() != current.saturation()
            || colour.value() != current.value() ){
        svChanged(colour);
    }

    update();
    emit colourChanged(colour);
}


QColor TColourWheel::posColour(const QPoint &point)
{
    if( ! wheel.rect().contains(point) ) return QColor();
    if(inWheel){
        qreal hue = 0;
        int r = qMin(width(), height()) / 2;
        if( point.x() > r ){
            if(point.y() < r ){
                //1
                hue = 90 - (atan2( (point.x() - r) , (r - point.y()) )  / 3.14 / 2 * 360);
            }else{
                //4
                hue = 270 + (atan2( (point.x() - r) , (point.y() - r ) )  / 3.14 / 2 * 360);
            }
        }else{
            if(point.y() < r ){
                //2
                hue =  90 + (atan2( (r - point.x()) , (r - point.y()) )  / 3.14 / 2 * 360);
            }else{
                //3
                hue =  270 - (atan2( (r - point.x()) , (point.y() - r ))  / 3.14 / 2 * 360);
            }
        }
        hue = hue>359?359:hue;
        hue = hue<0?0:hue;
        return QColor::fromHsv(hue,
                               current.saturation(),
                               current.value());
    }
    if(inSquare){
        // region of the widget
        int w = qMin(width(), height());
        // radius of outer circle
        qreal r = w/2-margin;
        // radius of inner circle
        qreal ir = r-wheelWidth;
        // left corner of square
        qreal m = w/2.0-ir/sqrt(2);
        QPoint p = point - QPoint(m, m);
        qreal SquareWidth = 2*ir/sqrt(2);
        return QColor::fromHsvF( current.hueF(),
                                 p.x()/SquareWidth,
                                 p.y()/SquareWidth);
    }
    return QColor();
}

QSize TColourWheel::sizeHint () const
{
    return QSize(height(),height());
}
QSize TColourWheel::minimumSizeHint () const
{
    return initSize;
}

void TColourWheel::mousePressEvent(QMouseEvent *event)
{
    lastPos = event->pos();
    if(wheelRegion.contains(lastPos)){
        inWheel = true;
        inSquare = false;
        QColor color = posColour(lastPos);
        hueChanged(color.hue());
    }else if(squareRegion.contains(lastPos)){
        inWheel = false;
        inSquare = true;
        QColor color = posColour(lastPos);
        svChanged(color);
    }
    mouseDown = true;
}

void TColourWheel::mouseMoveEvent(QMouseEvent *event)
{
    lastPos = event->pos();
    if( !mouseDown ) return;
    if(wheelRegion.contains(lastPos) && inWheel){
        QColor color = posColour(lastPos);
        hueChanged(color.hue());
    }else if(squareRegion.contains(lastPos) && inSquare){
        QColor color = posColour(lastPos);
        svChanged(color);
    }else{
        // TODO: due with cursor out of region after press
        //        int length = qMin(width(), height());
        //        QPoint center(length/2, length/2);
        //        int R = qSqrt(qPow(qAbs(lastPos.x()), 2)
        //                      + qPow(qAbs(lastPos.y()), 2));
        //        if(inWheel){
        //            int r =  length / 2;
        //            r += qSqrt(qPow(center.x(), 2) + qPow(center.y(), 2));
        //            int x0 = r/R * qAbs(lastPos.x());
        //            int y0 = r/R * qAbs(lastPos.y());
        //            QColor color = posColour(QPoint(x0, y0));
        //            hueChanged(color.hue());
        //        }else if(inSquare){
        //            //
        //        }
    }
}

void TColourWheel::mouseReleaseEvent(QMouseEvent *)
{
    mouseDown = false;
    inWheel = false;
    inSquare = false;
}

void TColourWheel::resizeEvent(QResizeEvent *event)
{
    wheel = QPixmap(event->size());
    wheel.fill(palette().background().color());
    drawWheelImage(event->size());
    drawSquareImage(current.hue());
    update();
}

void TColourWheel::paintEvent(QPaintEvent *)
{
    QPainter painter(this);
    QStyleOption opt;
    opt.initFrom(this);
    composeWheel();
    painter.drawPixmap(0,0,wheel);
    style()->drawPrimitive(QStyle::PE_Widget, &opt, &painter, this);
}

void TColourWheel::drawWheelImage(const QSize &newSize)
{

    int r = qMin(newSize.width(), newSize.height());

    QStyleOption option;
    option.initFrom(this);
    //    QStyle::State state = option.state;

    QBrush background = option.palette.window();

    wheelImage = QImage(newSize, QImage::Format_ARGB32_Premultiplied);
    wheelImage.fill(background.color());
    QPainter painter(&wheelImage);
    painter.setRenderHint(QPainter::Antialiasing);

    QConicalGradient conicalGradient(0, 0, 0);
    conicalGradient.setColorAt(0.0, Qt::red);
    conicalGradient.setColorAt(60.0/360.0, Qt::yellow);
    conicalGradient.setColorAt(120.0/360.0, Qt::green);
    conicalGradient.setColorAt(180.0/360.0, Qt::cyan);
    conicalGradient.setColorAt(240.0/360.0, Qt::blue);
    conicalGradient.setColorAt(300.0/360.0, Qt::magenta);
    conicalGradient.setColorAt(1.0, Qt::red);

    /* outer circle */
    painter.translate(r / 2, r / 2);

    QBrush brush(conicalGradient);
    painter.setPen(Qt::NoPen);
    painter.setBrush(brush);
    painter.drawEllipse(QPoint(0,0),r/2-margin,r/2-margin);
    /* inner circle */
    painter.setBrush(background);
    painter.drawEllipse(QPoint(0,0),r/2-margin-wheelWidth,r/2-margin-wheelWidth);

    //caculate wheel region
    wheelRegion = QRegion(r/2, r/2, r-2*margin, r-2*margin, QRegion::Ellipse);
    wheelRegion.translate(-(r-2*margin)/2, -(r-2*margin)/2);

    int tmp = 2*(margin+wheelWidth);
    QRegion subRe( r/2, r/2, r-tmp, r-tmp, QRegion::Ellipse );
    subRe.translate(-(r-tmp)/2, -(r-tmp)/2);
    wheelRegion -= subRe;
}

void TColourWheel::drawSquareImage(const int &hue)
{
//    QPainter painter(&squarePixmap);
//    painter.setRenderHint(QPainter::Antialiasing);

    // region of the widget
    int w = qMin(width(), height());
    // radius of outer circle
    qreal r = w/2-margin;
    // radius of inner circle
    qreal ir = r-wheelWidth;
    // left corner of square
    qreal m = w/2.0-ir/sqrt(2);
    //painter.translate(m, m);
    //painter.setPen(Qt::NoPen);
    QImage square(255,255, QImage::Format_ARGB32_Premultiplied);
    QColor color;
    QRgb vv;
    for(int i=0;i<255;++i){
        for(int j=0;j<255;++j){
            color = QColor::fromHsv(hue,i,j);
            vv = qRgb(color.red(),color.green(),color.blue());
            square.setPixel(i,j,vv);
        }
    }
    qreal SquareWidth = 2*ir/sqrt(2);
    squareImage = square.scaled(SquareWidth,SquareWidth);
//    painter.drawImage(0,0,square);

    //    QPainter painter2(&wheel);
    //    painter2.drawImage(0,0,source);

    squareRegion = QRegion(m, m, SquareWidth, SquareWidth);
}

void TColourWheel::drawIndicator(const int &hue)
{
    QPainter painter(&wheel);
    painter.setRenderHint(QPainter::Antialiasing);
    if(hue > 20 && hue < 200){
        painter.setPen(Qt::black);
    }else{
        painter.setPen(Qt::white);
    }
    painter.setBrush(Qt::NoBrush);

    QPen pen = painter.pen();
    pen.setWidth(3);
    painter.setPen(pen);
    qreal r = qMin(height(), width()) / 2.0;
    painter.translate(r, r);
    painter.rotate( -hue );
    r = qMin(height(), width()) / 2.0 - margin - wheelWidth/2;
    painter.drawEllipse(QPointF(r,0.0),5,5);
}

void TColourWheel::drawPicker(const QColor &color)
{
    QPainter painter(&wheel);
    painter.setRenderHint(QPainter::Antialiasing);
    QPen pen;

    // region of the widget
    int w = qMin(width(), height());
    // radius of outer circle
    qreal r = w/2-margin;
    // radius of inner circle
    qreal ir = r-wheelWidth;
    // left corner of square
    qreal m = w/2.0-ir/sqrt(2);
    painter.translate(m-5, m-5);
    qreal SquareWidth = 2*ir/sqrt(2);
    qreal S = color.saturationF()*SquareWidth;
    qreal V = color.valueF()*SquareWidth;

    if(color.saturation() > 30 ||color.value() < 50){
        pen.setColor(Qt::white);
    }
    pen.setWidth(3);
    painter.setPen(pen);
    painter.drawEllipse(S,V,10,10);
}

void TColourWheel::composeWheel()
{
    QPainter composePainter(&wheel);
    composePainter.drawImage(0, 0, wheelImage);
    composePainter.drawImage(squareRegion.boundingRect().topLeft(), squareImage);
    composePainter.end();
    drawIndicator(current.hue());
    drawPicker(current);
}

void TColourWheel::hueChanged(const int &hue)
{
    if( hue<0 ||hue>359)return;
    int s = current.saturation();
    int v = current.value();
    current.setHsv(hue, s, v);
    if(!isVisible()) return;
    //drawWheel(size());
    drawSquareImage(hue);
    //drawIndicator(hue);
    //drawPicker(current);
    repaint();
    emit colourChanged(current);
}

void TColourWheel::svChanged(const QColor &newcolor)
{
    int hue = current.hue();
    current.setHsv(hue, newcolor.saturation(),
                   newcolor.value());
    if(!isVisible()) return;
    //drawWheel(size());
    //drawSquare(hue);
    //drawIndicator(hue);
    //drawPicker(newcolor);
    repaint();
    emit colourChanged(current);
}