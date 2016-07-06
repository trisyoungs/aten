/*
	*** TMenuButton Functions
	*** src/gui/tmenubutton_funcs.cpp
	Copyright T. Youngs 2007-2016

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
#include "gui/tpopupwidget.hui"
#include "gui/mainwindow.h"
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QApplication>
#include <QtWidgets/QStyle>
#include <QtWidgets/QStylePainter>
#include <QtWidgets/QStyleOptionToolButton>
#include <QButtonGroup>

// Static Singletons
List<TMenuButtonGroup> TMenuButton::groups_;
AtenWindow* TMenuButton::atenWindow_ = NULL;

// Constructor
TMenuButton::TMenuButton(QWidget* parent) : QToolButton(parent)
{
	// Nullify popup widget to start with
	popupWidget_ = NULL;
	instantPopup_ = false;
	group_ = NULL;

	// Set popup timer delay, style, and connect slot
	popupTimer_.setSingleShot(true);
	popupTimer_.setInterval(QApplication::style()->styleHint(QStyle::SH_ToolButton_PopupDelay));
	connect(&popupTimer_, SIGNAL(timeout()), this, SLOT(popup()));

	// Connect signals for pressed / released
	connect(this, SIGNAL(pressed()), this, SLOT(buttonPressed()));
	connect(this, SIGNAL(released()), this, SLOT(buttonReleased()));
}

// Link to AtenWindow

// Set pointer to AtenWindow
void TMenuButton::setAtenWindow(AtenWindow* atenWindow)
{
	atenWindow_ = atenWindow;
}

// Button Data

// Return user-assigned index of button
int TMenuButton::index()
{
	return index_;
}

// Widget for popup

// Set popup widget for button
void TMenuButton::setPopupWidget(TPopupWidget* widget, bool instantPopup)
{
	popupWidget_ = widget;
	instantPopup_ = instantPopup;
}

// Return popup widget set for button
TPopupWidget* TMenuButton::popupWidget()
{
	return popupWidget_;
}

// Call named method in associated popup widget
bool TMenuButton::callPopupMethod(QString methodName, ReturnValue& rv)
{
	if (popupWidget_) return popupWidget_->callMethod(methodName, rv);
	else printf("No popup set on button with text '%s', so method '%s' cannot be called.\n", qPrintable(text()), qPrintable(methodName));

	return false;
}

// Notify button that popup is done
void TMenuButton::popupDone(bool setButtonDown, UserAction::Action userActionToEnable)
{
	// What we do here exactly depends on the current state of the button
	if ((!setButtonDown) && (userActionToEnable != atenWindow_->selectedMode()))
	{
		setDown(false);
		setChecked(false);
	}
	else if (group_ && (!isDown())) group_->setCurrentButton(this);

	if (userActionToEnable != UserAction::NoAction) atenWindow_->setSelectedMode(userActionToEnable);

// 	// Click button if requested
// 	if (clickButton && (!setButtonDown)) emit(click());
}

// Notify button that the popup has been hidden
void TMenuButton::popupWidgetHidden()
{
	emit(popupHidden());
}

// Notify button that a widget on the popup has been changed
void TMenuButton::popupWidgetChanged(int data)
{
	emit(popupChanged(data));
}

// Protected functions

// Draw the button
void TMenuButton::paintEvent(QPaintEvent* event)
{
	// Initialise a stylepainter and the style option
	QStylePainter painter(this);
	QStyleOptionToolButton opt;
	initStyleOption(&opt);

	// Draw the control
	QRect buttonRect, menuarea;
	buttonRect = style()->subControlRect(QStyle::CC_ToolButton, &opt, QStyle::SC_ToolButton, this);
	menuarea = style()->subControlRect(QStyle::CC_ToolButton, &opt, QStyle::SC_ToolButtonMenu, this);

	QStyle::State bflags = opt.state & ~QStyle::State_Sunken;

	if (bflags & QStyle::State_AutoRaise) 
	{
		if (!(bflags & QStyle::State_MouseOver) || !(bflags & QStyle::State_Enabled)) bflags &= ~QStyle::State_Raised;
	}
	QStyle::State mflags = bflags;
	if (opt.state & QStyle::State_Sunken)
	{
		if (opt.activeSubControls & QStyle::SC_ToolButton) bflags |= QStyle::State_Sunken;
		mflags |= QStyle::State_Sunken;
	}

	// Draw button border
	QStyleOption tool(0);
	tool.palette = opt.palette;
	if (opt.subControls & QStyle::SC_ToolButton)
	{
		if (bflags & (QStyle::State_Sunken | QStyle::State_On | QStyle::State_Raised))
		{
			tool.rect = buttonRect;
			tool.state = bflags;
			painter.drawPrimitive(QStyle::PE_PanelButtonTool, opt);
// 				style()->drawPrimitive(QStyle::PE_PanelButtonTool, &tool, &painter, this);
		}
	}

	// Draw on focus rect if necessary
	if (opt.state & QStyle::State_HasFocus)
	{
		QStyleOptionFocusRect fr;
		fr.QStyleOption::operator=(opt);
		fr.rect.adjust(3, 3, -3, -3);
		if (opt.features & QStyleOptionToolButton::MenuButtonPopup)
		fr.rect.adjust(0, 0, -style()->pixelMetric(QStyle::PM_MenuButtonIndicator, &opt, this), 0);
		style()->drawPrimitive(QStyle::PE_FrameFocusRect, &fr, &painter, this);
	}

	// Draw the main label, icon, and arrow on the tool button
	// Based on code from drawControl() in qcommonstyle.cpp (Qt 5.4.1)
	QStyleOptionToolButton inner = opt;
	inner.state = bflags;
	int fw = style()->pixelMetric(QStyle::PM_DefaultFrameWidth, &opt, this);
	inner.rect = buttonRect.adjusted(fw, fw, -fw, -fw);

	// Define inner rectangle for elements
	QRect rect = inner.rect;
	int shiftX = 0;
	int shiftY = 0;
	if (inner.state & (QStyle::State_Sunken | QStyle::State_On))
	{
		shiftX = style()->pixelMetric(QStyle::PM_ButtonShiftHorizontal, &inner, this);
		shiftY = style()->pixelMetric(QStyle::PM_ButtonShiftVertical, &inner, this);
	}

	// Arrow type always overrules and is always shown
	bool hasArrow = inner.features & QStyleOptionToolButton::Arrow;
	if (((!hasArrow && inner.icon.isNull()) && !inner.text.isEmpty()) || inner.toolButtonStyle == Qt::ToolButtonTextOnly)
	{
		int alignment = Qt::AlignCenter | Qt::TextShowMnemonic;
		if (!style()->styleHint(QStyle::SH_UnderlineShortcut, &inner, this))
		alignment |= Qt::TextHideMnemonic;
		rect.translate(shiftX, shiftY);
		painter.setFont(inner.font);
		style()->drawItemText(&painter, rect, alignment, inner.palette, inner.state & QStyle::State_Enabled, inner.text, QPalette::ButtonText);
	}
	else
	{
		if (inner.toolButtonStyle == Qt::ToolButtonTextUnderIcon)
		{
			// Draw arrow (if there is an associated popupwidget)
			QRect arrowRect = rect.translated(0, -2);
			arrowRect.translate(shiftX, shiftY);
			arrowRect.setTop(arrowRect.bottom() - 6);
			if (popupWidget_)
			{
				QStyleOption arrowOpt;
				arrowOpt.rect = arrowRect;
				arrowOpt.palette = inner.palette;
				arrowOpt.state = inner.state;
				if (!instantPopup_) drawEllipsis(arrowOpt, painter);
				else style()->drawPrimitive(QStyle::PE_IndicatorArrowDown, &arrowOpt, &painter, this);
			}

			// Draw text
			int alignment = Qt::AlignCenter | Qt::TextShowMnemonic | Qt::AlignBottom;
			if (!style()->styleHint(QStyle::SH_UnderlineShortcut, &inner, this)) alignment |= Qt::TextHideMnemonic;
			QRect textRect = arrowRect.translated(0, -8);
			textRect.setTop(textRect.bottom()-1);
			textRect = style()->itemTextRect(inner.fontMetrics, textRect, alignment, inner.state & QStyle::State_Enabled, inner.text);
			painter.setFont(inner.font);
			style()->drawItemText(&painter, textRect, alignment, inner.palette, inner.state & QStyle::State_Enabled, inner.text, QPalette::ButtonText);

			// Draw icon (filling remaining space in button)
			QRect iconRect(0,rect.top(),1,textRect.top() - rect.top() - 4);
			iconRect.setTop(rect.top()+8);
			iconRect.setLeft(rect.left() + (rect.width()-iconRect.height())/2);
			iconRect.setWidth(iconRect.height());
			QSize pixmapSize(iconRect.width(), iconRect.height());
			QIcon::State state = inner.state & QStyle::State_On ? QIcon::On : QIcon::Off;
			QIcon::Mode mode;
			if (!(inner.state & QStyle::State_Enabled)) mode = QIcon::Disabled;
			else if ((inner.state & QStyle::State_MouseOver) && (inner.state & QStyle::State_AutoRaise)) mode = QIcon::Active;
			else mode = QIcon::Normal;
			QPixmap pm = inner.icon.pixmap(pixmapSize, mode, state);
			pixmapSize = pm.size() / pm.devicePixelRatio();
			style()->drawItemPixmap(&painter, iconRect, Qt::AlignCenter, pm);
		}
		else if (inner.toolButtonStyle == Qt::ToolButtonTextBesideIcon)
		{
			// Draw icon on the left-hand side of the button
			QRect iconRect(0, rect.top(), rect.height(), rect.height());
			iconRect = iconRect.adjusted(2,2,-2,-2);
// 			iconRect.setTop(rect.top()+8);
// 			iconRect.setLeft(rect.left() + (rect.width()-iconRect.height())/2);
// 			iconRect.setWidth(iconRect.height());
			QSize pixmapSize(iconRect.width(), iconRect.height());
			QIcon::State state = inner.state & QStyle::State_On ? QIcon::On : QIcon::Off;
			QIcon::Mode mode;
			if (!(inner.state & QStyle::State_Enabled)) mode = QIcon::Disabled;
			else if ((inner.state & QStyle::State_MouseOver) && (inner.state & QStyle::State_AutoRaise)) mode = QIcon::Active;
			else mode = QIcon::Normal;
			QPixmap pm = inner.icon.pixmap(pixmapSize, mode, state);
			pixmapSize = pm.size() / pm.devicePixelRatio();
			style()->drawItemPixmap(&painter, iconRect, Qt::AlignCenter, pm);

			// Draw text
			int alignment = Qt::AlignLeft | Qt::TextShowMnemonic;
			if (!style()->styleHint(QStyle::SH_UnderlineShortcut, &inner, this)) alignment |= Qt::TextHideMnemonic;
			QRect textRect = rect.adjusted(iconRect.width(), 0, 0, 0);
// 			textRect.setTop(textRect.bottom()-1);
			textRect = style()->itemTextRect(inner.fontMetrics, textRect, alignment, inner.state & QStyle::State_Enabled, inner.text);
			painter.setFont(inner.font);
			style()->drawItemText(&painter, textRect, alignment, inner.palette, inner.state & QStyle::State_Enabled, inner.text, QPalette::ButtonText);
		}
		else if ((inner.toolButtonStyle == Qt::ToolButtonIconOnly) && (!inner.icon.isNull()))
		{
			// Button type is icon only - don't draw any arrow indicating the presence of a popup widget
			rect.translate(shiftX, shiftY);

			// Generate pixmap for button
			QPixmap pm;
			QSize pmSize(inner.iconSize.width(), inner.iconSize.height());
			
			QIcon::State state = inner.state & QStyle::State_On ? QIcon::On : QIcon::Off;
			QIcon::Mode mode;
			if (!(inner.state & QStyle::State_Enabled)) mode = QIcon::Disabled;
			else if ((inner.state & QStyle::State_MouseOver) && (inner.state & QStyle::State_AutoRaise)) mode = QIcon::Active;
			else mode = QIcon::Normal;
			pm = inner.icon.pixmap(inner.rect.size().boundedTo(inner.iconSize), mode, state);
			pmSize = pm.size() / pm.devicePixelRatio();

			style()->drawItemPixmap(&painter, rect, Qt::AlignCenter, pm);
		}
	}
}

void TMenuButton::drawEllipsis(QStyleOption& styleOption, QPainter& painter)
{
	if (!(styleOption.state & QStyle::State_Enabled))
	{
		painter.translate(1, 1);
		painter.setBrush(styleOption.palette.light().color());
		painter.setPen(styleOption.palette.light().color());
		painter.drawEllipse(styleOption.rect.center(), 2, 2);
		painter.drawEllipse(styleOption.rect.center()-QPoint(styleOption.rect.width()/6,0), 2, 2);
		painter.drawEllipse(styleOption.rect.center()+QPoint(styleOption.rect.width()/6,0), 2, 2);
		painter.translate(-1, -1);
		painter.setBrush(styleOption.palette.mid().color());
		painter.setPen(styleOption.palette.mid().color());
	}
	else if (styleOption.state & QStyle::State_MouseOver)
	{
		painter.translate(1, 1);
		painter.setBrush(styleOption.palette.mid().color());
		painter.setPen(styleOption.palette.mid().color());
		painter.drawEllipse(styleOption.rect.center(), 2, 2);
		painter.drawEllipse(styleOption.rect.center()-QPoint(styleOption.rect.width()/6,0), 2, 2);
		painter.drawEllipse(styleOption.rect.center()+QPoint(styleOption.rect.width()/6,0), 2, 2);
		painter.translate(-1, -1);
		painter.setBrush(styleOption.palette.buttonText().color());
		painter.setPen(styleOption.palette.buttonText().color());
	}
	else
	{
		painter.translate(1, 1);
		painter.setBrush(styleOption.palette.light().color());
		painter.setPen(styleOption.palette.light().color());
		painter.drawEllipse(styleOption.rect.center(), 2, 2);
		painter.drawEllipse(styleOption.rect.center()-QPoint(styleOption.rect.width()/6,0), 2, 2);
		painter.drawEllipse(styleOption.rect.center()+QPoint(styleOption.rect.width()/6,0), 2, 2);
		painter.translate(-1, -1);
		painter.setBrush(styleOption.palette.mid().color());
		painter.setPen(styleOption.palette.mid().color());
	}
	painter.drawEllipse(styleOption.rect.center(), 2, 2);
	painter.drawEllipse(styleOption.rect.center()-QPoint(styleOption.rect.width()/6,0), 2, 2);
	painter.drawEllipse(styleOption.rect.center()+QPoint(styleOption.rect.width()/6,0), 2, 2);
}

// Call the popup widget
void TMenuButton::popup()
{
	if (!popupWidget_) return;

// 	printf("POPUP signalled (wasChecked = %i, down = %i).\n", checkedBeforePressed_, popupWidget_->shown());

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

// Return whether popup (if there is one) is visible
bool TMenuButton::popupVisible()
{
	if (popupWidget_) return popupWidget_->isVisible();
	else return false;
}

// Mouse button was pressed on the button
void TMenuButton::buttonPressed()
{
// 	printf("PRESSED [%s] (isDown=%i, isChecked=%i)\n", qPrintable(text()), isDown(), isChecked());

	// Store current checked state of button
	checkedBeforePressed_ = isChecked();

	// Start popup timer (if not an instantPopup_)
	if (popupWidget_)
	{
		if (instantPopup_) emit(popup());
		else popupTimer_.start();
	}
}

// Mouse button was released
void TMenuButton::buttonReleased()
{
// 	printf("RELEASED [%s]\n", qPrintable(text()));
	// Make sure timer is stopped - it will have either already popped, or this was a single-click and we don't want the popup
	popupTimer_.stop();

	// Check the mouse coordinates at the point of release
	QPoint point = mapFromGlobal(QCursor::pos());
	if ((point.x() >= width()) || (point.y() >= height()) || (point.x() < 0) || (point.y() < 0))
	{
		return;
	}
	else
	{
		// Signal the group of this button (if there is one) that the button has been properly clicked
		if (group_) group_->setCurrentButton(this);
	}
}

/*
 * Group
 */

// Add this button to the named group
void TMenuButton::setGroup(QString groupName, int index)
{
	// Search for this group...
	TMenuButtonGroup* group;
	for (group = groups_.first(); group != NULL; group = group->next) if (group->name() == groupName) break;
	if (group == NULL)
	{
		group = groups_.add();
		group->setName(groupName);
	}

	group_ = group;
	group_->addButton(this);
	index_ = index;
}

// Check specified button in specified group
bool TMenuButton::setGroupButtonChecked(QString groupName, QString buttonText)
{
	// First, find named group
	TMenuButtonGroup* group;
	for (group = groups_.first(); group != NULL; group = group->next) if (group->name() == groupName) break;
	if (!group)
	{
		Messenger::print("Internal error: No TMenuButton group named '%s'\n", qPrintable(groupName));
		return false;
	}

	return group->setCurrentButton(buttonText);
}

// Check specified button in specified group
bool TMenuButton::setGroupButtonChecked(QString groupName, int buttonIndex)
{
	// First, find named group
	TMenuButtonGroup* group;
	for (group = groups_.first(); group != NULL; group = group->next) if (group->name() == groupName) break;
	if (!group)
	{
		Messenger::print("Internal error: No TMenuButton group named '%s'\n", qPrintable(groupName));
		return false;
	}

	return group->setCurrentButton(buttonIndex);
}
