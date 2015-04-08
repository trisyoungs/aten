/*
	*** TMenuButton Functions
	*** src/gui/tmenubutton_funcs.cpp
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

#include "gui/tmenubutton.hui"
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QApplication>
#include <QtWidgets/QStyle>
#include <QtWidgets/QStylePainter>
#include <QtWidgets/QStyleOptionToolButton>

/*
 * TMenuButtonPopupWidget
 */

// Constructor
TMenuButtonPopupWidget::TMenuButtonPopupWidget(TMenuButton* parent) : QWidget(parent, Qt::FramelessWindowHint | Qt::Popup)
{
	parentMenuButton_ = parent;
}

void TMenuButtonPopupWidget::hideEvent(QHideEvent* event)
{
// 	printf("HIDEEVENT\n");
	parentMenuButton_->popupDone();

	event->accept();
}

/*
 * TMenuButton
 */

// Constructor
TMenuButton::TMenuButton(QWidget* parent) : QToolButton(parent)
{
	// Nullify popup widget to start with
	popupWidget_ = NULL;

	// Set popup timer delay, style, and connect slot
	popupTimer_.setSingleShot(true);
	popupTimer_.setInterval(QApplication::style()->styleHint(QStyle::SH_ToolButton_PopupDelay));
	connect(&popupTimer_, SIGNAL(timeout()), this, SLOT(popup()));

	// Connect signals for pressed / released
	connect(this, SIGNAL(pressed()), this, SLOT(buttonPressed()));
	connect(this, SIGNAL(released()), this, SLOT(buttonReleased()));

}

// Set popup widget for button
void TMenuButton::setPopupWidget(TMenuButtonPopupWidget* widget)
{
	popupWidget_ = widget;
}

void TMenuButton::popupDone()
{
	// Emit the 'released()' signal
	if (!checkedBeforePressed_) setDown(false);
	else emit(released());
}

// Notify button that popup is done
void TMenuButton::paintEvent(QPaintEvent* event)
{
	QStylePainter painter(this);

// 	QPainterPath path;
// 	// Set pen to start point, which is lower right-hand corner of widget area
// 	path.moveTo(width(), height());
// 	// Create triangle...
// 	path.lineTo(width()/2, height()-1);
// 	path.lineTo(width()-1, height()/2);
// 	path.lineTo(width()-1, height()-1);
// 
// 	// Don't draw any lines
// 	painter.setPen(Qt::NoPen);

// 	// Setup QGradient for fill
// 	QRadialGradient gradient(width()/2, height()/2, width()/2, width(), height());
// 	painter.fillPath(path, QBrush(gradient));

	QStyleOptionToolButton opt;
	initStyleOption(&opt);
	painter.drawComplexControl(QStyle::CC_ToolButton, opt);
}

void TMenuButton::popup()
{
	if (!popupWidget_) return;

// 	printf("POPUP signalled (wasChecked = %i).\n", checkedBeforePressed_);

	// Show the popup widget - call the widget's popup() method, so controls are set correctly
	popupWidget_->popup();

	// 	printf("Cursor = %i %i\n", QCursor::pos().x(), QCursor::pos().y());
	if (!parentWidget())
	{
		printf("No parent widget in TMenuButton - can't position popupWidget_.\n");
		return;
	}

	// Reposition popup widget to sit flush left with the tool button, and immediately underneath it
	QPoint toolPos = parentWidget()->mapToGlobal(pos()+QPoint(0,height()));
	popupWidget_->move(toolPos);
}

void TMenuButton::buttonPressed()
{
// 	printf("PRESSED [%s] (isDown=%i, isChecked=%i)\n", qPrintable(text()), isDown(), isChecked());
	// Store current checked state of button
	checkedBeforePressed_ = isChecked();

	// Start popup timer
	if (popupWidget_) popupTimer_.start();
}

void TMenuButton::buttonReleased()
{
// 	printf("RELEASED [%s]\n", qPrintable(text()));
	// Make sure timer is stopped - it will have either already popped, or this was a single-click and we don't want the popup
	popupTimer_.stop();
}
