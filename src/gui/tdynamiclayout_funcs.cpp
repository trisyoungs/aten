/*
	*** TDynamicLayout Functions
	*** src/gui/tdynamiclayout_funcs.cpp
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

#include "gui/tdynamiclayout.hui"
#include "gui/tmenubutton.hui"
#include "base/messenger.h"
#include <QLayout>
#include <QStyle>

// Static Members
// -- TMenuButton Icon Sizes
int TDynamicLayout::menuButtonIconSizeNormal_ = 30;
int TDynamicLayout::menuButtonIconSizeSmall_ = 18;
int TDynamicLayout::menuButtonIconSizeSmallest_ = 10;

// Constructors
TDynamicLayout::TDynamicLayout(QWidget *parent, int margin, int hSpacing, int vSpacing) : QLayout(parent)
{
	setContentsMargins(margin, margin, margin, margin);
	horizontalSpacing_ = hSpacing;
	verticalSpacing_ = vSpacing;
	preferredSize_= QSize(2*margin, 2*margin);
}

TDynamicLayout::TDynamicLayout(int margin, int hSpacing, int vSpacing) : QLayout(NULL)
{
	setContentsMargins(margin, margin, margin, margin);
	horizontalSpacing_ = hSpacing;
	verticalSpacing_ = vSpacing;
}

// Destructor
TDynamicLayout::~TDynamicLayout()
{
	QLayoutItem *item;
	while ((item = takeAt(0))) delete item;
}

/*
 * Virtual Reimplementations
 */

void TDynamicLayout::addItem(QLayoutItem *item)
{
	items_.append(item);

	// Attempt to cast the child into a TMenuButton
	TMenuButton* button = qobject_cast<TMenuButton*> (item->widget());
	if (button) printf("Found TMenuButton '%s'\n", qPrintable(button->text()));
	else
	{
		printf("Internal Error: TDynamicLayout was given a widget of an unsuitable type.\n");
		return;
	}

	// First item? If so, work out a few other constants from it
	if (items_.count() == 1)
	{
		preferredSize_ += button->minimumSize();
		twoPerRowHeight_ = (button->minimumHeight() - verticalSpacing_) / 2;
		threePerRowHeight_ = (button->minimumHeight() - 2*verticalSpacing_) / 3;
	}
	else preferredSize_ += QSize(button->minimumWidth() + horizontalSpacing_, 0);
	printf("Preferred size now %i,%i\n", preferredSize_.width(), preferredSize_.height());
}

int TDynamicLayout::count() const
{
	return items_.size();
}

QLayoutItem* TDynamicLayout::itemAt(int index) const
{
	return items_.value(index);
}

QLayoutItem* TDynamicLayout::takeAt(int index)
{
	if (index >= 0 && index < items_.size()) return items_.takeAt(index);
	else return 0;
}

// Return expanding directions
Qt::Orientations TDynamicLayout::expandingDirections() const
{
	return 0;
}

// Return minimum possible size for layout
QSize TDynamicLayout::minimumSize() const
{
	// Calculate minimum space required by the TMenuButtons
	QSize size;
	// TEST
	return QSize(preferredSize_;
}

// Set new geometry for the layout
void TDynamicLayout::setGeometry(const QRect &rect)
{
	QLayout::setGeometry(rect);
	layoutWidgets(rect);
}

// Return size hint
QSize TDynamicLayout::sizeHint() const
{
	return preferredSize_;
}

/*
 * Internal Data
 */

// Layout widgets
bool TDynamicLayout::layoutWidgets(QRect area, bool calculateOnly)
{
	int left, top, right, bottom;
	getContentsMargins(&left, &top, &right, &bottom);
	QRect effectiveRect = area.adjusted(+left, +top, -right, -bottom);
	int x = effectiveRect.x();
	int y = effectiveRect.y();
	int lineHeight = 0;
	QLayoutItem *item;
	foreach (item, items_) {
	QWidget *wid = item->widget();
	int spaceX = horizontalSpacing_;
	if (spaceX == -1) spaceX = wid->style()->layoutSpacing(QSizePolicy::PushButton, QSizePolicy::PushButton, Qt::Horizontal);
	int spaceY = verticalSpacing_;
	if (spaceY == -1) spaceY = wid->style()->layoutSpacing(QSizePolicy::PushButton, QSizePolicy::PushButton, Qt::Vertical);
	int nextX = x + item->sizeHint().width() + spaceX;
	if (nextX - spaceX > effectiveRect.right() && lineHeight > 0)
	{
		x = effectiveRect.x();
		y = y + lineHeight + spaceY;
		nextX = x + item->sizeHint().width() + spaceX;
		lineHeight = 0;
	}

	if (!calculateOnly)
		item->setGeometry(QRect(QPoint(x, y), item->sizeHint()));

	x = nextX;
	lineHeight = qMax(lineHeight, item->sizeHint().height());
	}
    return y + lineHeight - area.y() + bottom;
}
