/*
	*** TTabWidget Functions
	*** src/gui/ttabwidget_funcs.cpp
	Copyright T. Youngs 2007-2018

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

#include "gui/ttabwidget.hui"
#include <QtWidgets/QStyle>
#include <QtWidgets/QStylePainter>
#include <QtWidgets/QStyleOptionTabBarBaseV2>
#include <QWheelEvent>

// Constructor
TTabWidget::TTabWidget(QWidget* parent) : QTabWidget(parent)
{
}

// Draw the button
void TTabWidget::paintEvent(QPaintEvent* event)
{
// 	printf("Here in PaintEvent\n");
// 	// Initialise a stylepainter and the style option
// 	QStylePainter painter(this);
// 	QStyleOptionToolButton opt;
// 	initStyleOption(&opt);
// 
// 	// Draw the control
// // 	if (const QStyleOptionToolButton *toolbutton = qstyleoption_cast<const QStyleOptionToolButton *>(&opt))
// 	{
// 		QRect buttonRect, menuarea;
// 		buttonRect = style()->subControlRect(QStyle::CC_ToolButton, &opt, QStyle::SC_ToolButton, this);
// 		menuarea = style()->subControlRect(QStyle::CC_ToolButton, &opt, QStyle::SC_ToolButtonMenu, this);
// 
// 		QStyle::State bflags = opt.state & ~QStyle::State_Sunken;
// 
// 		if (bflags & QStyle::State_AutoRaise) 
// 		{
// 			if (!(bflags & QStyle::State_MouseOver) || !(bflags & QStyle::State_Enabled)) bflags &= ~QStyle::State_Raised;
// 		}
// 		QStyle::State mflags = bflags;
// 		if (opt.state & QStyle::State_Sunken)
// 		{
// 			if (opt.activeSubControls & QStyle::SC_ToolButton) bflags |= QStyle::State_Sunken;
// 			mflags |= QStyle::State_Sunken;
// 		}
// 
// 		// Draw button border
// 		QStyleOption tool(0);
// 		tool.palette = opt.palette;
// 		if (opt.subControls & QStyle::SC_ToolButton)
// 		{
// 			if (bflags & (QStyle::State_Sunken | QStyle::State_On | QStyle::State_Raised))
// 			{
// 				tool.rect = buttonRect;
// 				tool.state = bflags;
// 				painter.drawPrimitive(QStyle::PE_PanelButtonTool, opt);
// // 				style()->drawPrimitive(QStyle::PE_PanelButtonTool, &tool, &painter, this);
// 			}
// 		}
// 
// 		// Draw on focus rect if necessary
// 		if (opt.state & QStyle::State_HasFocus)
// 		{
// 			QStyleOptionFocusRect fr;
// 			fr.QStyleOption::operator=(opt);
// 			fr.rect.adjust(3, 3, -3, -3);
// 			if (opt.features & QStyleOptionToolButton::MenuButtonPopup)
// 			fr.rect.adjust(0, 0, -style()->pixelMetric(QStyle::PM_MenuButtonIndicator, &opt, this), 0);
// 			style()->drawPrimitive(QStyle::PE_FrameFocusRect, &fr, &painter, this);
// 		}
// 
// 		// Draw the main label, icon, and arrow on the tool button
// 		// Based on code from drawControl() in qcommonstyle.cpp (Qt 5.4.1)
// 		QStyleOptionToolButton inner = opt;
// 		inner.state = bflags;
// 		int fw = style()->pixelMetric(QStyle::PM_DefaultFrameWidth, &opt, this);
// 		inner.rect = buttonRect.adjusted(fw, fw, -fw, -fw);
// 
// 		// Define inner rectangle for elements
// 		QRect rect = inner.rect;
// 		int shiftX = 0;
// 		int shiftY = 0;
// 		if (inner.state & (QStyle::State_Sunken | QStyle::State_On))
// 		{
// 			shiftX = style()->pixelMetric(QStyle::PM_ButtonShiftHorizontal, &inner, this);
// 			shiftY = style()->pixelMetric(QStyle::PM_ButtonShiftVertical, &inner, this);
// 		}
// 
// 		// Arrow type always overrules and is always shown
// 		bool hasArrow = inner.features & QStyleOptionToolButton::Arrow;
// 		if (((!hasArrow && inner.icon.isNull()) && !inner.text.isEmpty()) || inner.toolButtonStyle == Qt::ToolButtonTextOnly)
// 		{
// 			int alignment = Qt::AlignCenter | Qt::TextShowMnemonic;
// 			if (!style()->styleHint(QStyle::SH_UnderlineShortcut, &inner, this))
// 			alignment |= Qt::TextHideMnemonic;
// 			rect.translate(shiftX, shiftY);
// 			painter.setFont(inner.font);
// 			style()->drawItemText(&painter, rect, alignment, inner.palette, inner.state & QStyle::State_Enabled, inner.text, QPalette::ButtonText);
// 		}
// 		else
// 		{
// 			if (inner.toolButtonStyle != Qt::ToolButtonIconOnly)
// 			{
// 				// Draw arrow (if there is an associated popupwidget)
// 				QRect arrowRect = rect.translated(0, -2);
// 				arrowRect.translate(shiftX, shiftY);
// 				arrowRect.setTop(rect.bottom() - 6);
// 				if (popupWidget_)
// 				{
// 					QStyleOption arrowOpt;
// 					arrowOpt.rect = arrowRect;
// 					arrowOpt.palette = inner.palette;
// 					arrowOpt.state = inner.state;
// 					style()->drawPrimitive(QStyle::PE_IndicatorArrowDown, &arrowOpt, &painter, this);
// 				}
// 
// 				// Draw text
// 				int alignment = Qt::AlignCenter | Qt::TextShowMnemonic | Qt::AlignBottom;
// 				if (!style()->styleHint(QStyle::SH_UnderlineShortcut, &inner, this)) alignment |= Qt::TextHideMnemonic;
// 				QRect textRect = arrowRect.translated(0, -8);
// 				textRect.setTop(textRect.bottom()-1);
// 				textRect = style()->itemTextRect(inner.fontMetrics, textRect, alignment, inner.state & QStyle::State_Enabled, inner.text);
// 				painter.setFont(inner.font);
// 				style()->drawItemText(&painter, textRect, alignment, inner.palette, inner.state & QStyle::State_Enabled, inner.text, QPalette::ButtonText);
// 
// 				// Draw icon (filling remaining space in button)
// 				QRect iconRect(0,rect.top(),1,textRect.top() - rect.top());
// 				iconRect.setTop(rect.top()+4);
// 				iconRect.setLeft(rect.left() + (rect.width()-iconRect.height())/2);
// 				iconRect.setWidth(iconRect.height());
// 				QSize pixmapSize(iconRect.width(), iconRect.height());
// 				QIcon::State state = inner.state & QStyle::State_On ? QIcon::On : QIcon::Off;
// 				QIcon::Mode mode;
// 				if (!(inner.state & QStyle::State_Enabled)) mode = QIcon::Disabled;
// 				else if ((inner.state & QStyle::State_MouseOver) && (inner.state & QStyle::State_AutoRaise)) mode = QIcon::Active;
// 				else mode = QIcon::Normal;
// 				QPixmap pm = inner.icon.pixmap(pixmapSize, mode, state);
// 				pixmapSize = pm.size() / pm.devicePixelRatio();
// 				style()->drawItemPixmap(&painter, iconRect, Qt::AlignCenter, pm);
// 			}
// 			else if (!inner.icon.isNull())
// 			{
// 				// Button type is icon only - don't draw any arrow indicating the presence of a popup widget
// 				rect.translate(shiftX, shiftY);
// 				// Generate pixmap for button
// 				QPixmap pm;
// 				QSize pmSize(inner.iconSize.width(), inner.iconSize.height());
// 				
// 				QIcon::State state = inner.state & QStyle::State_On ? QIcon::On : QIcon::Off;
// 				QIcon::Mode mode;
// 				if (!(inner.state & QStyle::State_Enabled)) mode = QIcon::Disabled;
// 				else if ((inner.state & QStyle::State_MouseOver) && (inner.state & QStyle::State_AutoRaise)) mode = QIcon::Active;
// 				else mode = QIcon::Normal;
// 				pm = inner.icon.pixmap(inner.rect.size().boundedTo(inner.iconSize), mode, state);
// 				pmSize = pm.size() / pm.devicePixelRatio();
// 
// 				style()->drawItemPixmap(&painter, rect, Qt::AlignCenter, pm);
// 			}
// 		}
// 	}
}

void TTabWidget::wheelEvent(QWheelEvent* event)
{
	int index = currentIndex() + (event->delta() > 0 ? 1 : -1);
	if (index < 0) index = count()-1;
	else if (index >= count()) index = 0;

	setCurrentIndex(index);
}