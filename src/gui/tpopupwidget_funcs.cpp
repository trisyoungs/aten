/*
	*** TPopupWidget Functions
	*** src/gui/tpopupwidget_funcs.cpp
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

#include "gui/tpopupwidget.hui"
#include "gui/mainwindow.h"
#include <QHideEvent>

// Static members
TPopupWidget* TPopupWidget::lastPopup_ = NULL;
QTime TPopupWidget::lastPopupHideTime_;

// Constructor
TPopupWidget::TPopupWidget(TMenuButton* parent) : QWidget(parent, Qt::FramelessWindowHint | Qt::Popup)
{
	parentMenuButton_ = parent;
	widgetDone_ = false;
}

/*
 * Protected Functions
 */

// Local function called when the widget should be closed after a button has been selceted
void TPopupWidget::done(bool setParentButtonDown, UserAction::Action userActionToEnable)
{
	if (parentMenuButton_) parentMenuButton_->popupDone(setParentButtonDown, userActionToEnable);
	else Messenger::print("Internal Error: No parent button set in TPopupWidget::done().\n");

	widgetDone_ = true;

	hide();
}

// Notify parent button that one of our widgets has changed
void TPopupWidget::changed(int data)
{
	if (parentMenuButton_) parentMenuButton_->popupWidgetChanged(data);
}

/*
 * Public
 */

// Show popup, updating any controls as necessary beforehand
void TPopupWidget::popup()
{
	// Check time of last popup - if it was too short, then we assume this was a click on the same source button
	if ((lastPopup_ == this) && (lastPopupHideTime_.msecsTo(QTime::currentTime()) < 50)) return;

	updateControls();

	show();

	lastPopup_ = this;
}

// Call named method associated to popup (without ReturnValue)
bool TPopupWidget::callMethodSimple(QString methodName, QString value)
{
	ReturnValue rv(value);
	return callMethod(methodName, rv);
}

// Return parent TMenuButton
TMenuButton* TPopupWidget::parentMenuButton()
{
	return parentMenuButton_;
}

/*
 * Virtual Reimplementations
 */

void TPopupWidget::hideEvent(QHideEvent* event)
{
	// Call the parent's popupDone() function, unless the widgetDone_ flag is set
	if (parentMenuButton_ && (!widgetDone_)) parentMenuButton_->popupDone(false);

	// Call the 'hide' method of the widget, to perform any post-hide functions
	ReturnValue rv;
	callMethod("hideEvent", rv);

	// Reset the widgetDone_ flag
	widgetDone_ = false;

	// Set the hide time
	lastPopupHideTime_ = QTime::currentTime();

	event->accept();

	// Notify the parent button that we have been hidden
	if (parentMenuButton_) parentMenuButton_->popupHidden();
}

