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

// Constructors
TDynamicLayout::TDynamicLayout(QWidget *parent, int margin, int hSpacing, int vSpacing) : QLayout(parent)
{
	setContentsMargins(margin, margin, margin, margin);
	horizontalSpacing_ = (hSpacing == -1 ? 4 : hSpacing);
	verticalSpacing_ = (vSpacing == -1 ? 4 : vSpacing);
	currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
}

TDynamicLayout::TDynamicLayout(int margin, int hSpacing, int vSpacing) : QLayout(NULL)
{
	setContentsMargins(margin, margin, margin, margin);
	horizontalSpacing_ = (hSpacing == -1 ? 4 : hSpacing);
	verticalSpacing_ = (vSpacing == -1 ? 4 : vSpacing);
	currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
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
	return sizeHints_[TDynamicLayout::IconStackLayout];
}

// Set new geometry for the layout
void TDynamicLayout::setGeometry(const QRect &rect)
{
	QLayout::setGeometry(rect);
	printf("SETGEOMETRY was called: available width = %i, available height = %i, right = %i\n", rect.width(), rect.height(), rect.right());

	QSize usedSize;
	// Let us see what will fit!
	// Is the supplied rect (in particular the width) big enough to fulfil the sizeHint()
	if (rect.width() >= sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].width())
	{
		// Layout buttons at full size
		usedSize = layoutWidgets(TDynamicLayout::FullSizeHorizontalLayout);
		currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
	}
	else
	{
		// First try the stacked approach. If this is still too wide, go for single icon stack
		usedSize = layoutWidgets(TDynamicLayout::ButtonStackLayout);
		currentLayoutStyle_ = TDynamicLayout::ButtonStackLayout;
		if (usedSize.width() > rect.width())
		{
			usedSize = layoutWidgets(TDynamicLayout::IconStackLayout);
			currentLayoutStyle_ = TDynamicLayout::IconStackLayout;
		}
	}

	// Store new size for current layout
	sizeHints_[currentLayoutStyle_] = usedSize;
	// Set geometry of parent to ideal size
// 	parentWidget()->setGeometry(parentWidget()->x(), parentWidget()->y(), usedSize.width(), usedSize.height());
}

// Return size hint
QSize TDynamicLayout::sizeHint() const
{
	// If we already have a valid size for the current layout, return that?
	if (sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].isValid())
	{
		printf("sizehint is returning %ix%i\n", sizeHints_[FullSizeHorizontalLayout].width(), sizeHints_[FullSizeHorizontalLayout].height());
		return sizeHints_[FullSizeHorizontalLayout];
	}

	// Otherwise, work out maximum size based on current widget geometries
	QSize size;
	for (int n=0; n<count(); ++n)
	{
		if (n == 0) size = itemAt(n)->sizeHint();
		else size += QSize(itemAt(n)->sizeHint().width() + horizontalSpacing_, 0);
	}
	printf("sizehint calculated and returned %ix%i\n", size.width(), size.height());
	return size;
}

/*
 * Internal Data
 */

// Layout widgets to the specified style, returning the size used
QSize TDynamicLayout::layoutWidgets(LayoutStyle layoutStyle)
{
	// Get margins and adjust available area
	int left, top, right, bottom;
	getContentsMargins(&left, &top, &right, &bottom);
	printf("contents margins = %i %i %i %i\n", left, top, right, bottom);
	int w = 0;
	int h = 0;

	// Construct our list of TMenuButtons for convenience
	QLayoutItem *item;
	RefList<TMenuButton,int> buttons;
	foreach (item, items_)
	{
		// Grab the widget and cast it into a TMenuButton
		QWidget *wid = item->widget();
		TMenuButton* button = qobject_cast<TMenuButton*>(wid);
		if (!button)
		{
			printf("Internal Error: Couldn't cast TDynamicLayout child widget into a TMenuButton.\n");
			return QSize();
		}
		buttons.add(button);
	}

	int requiredWidth = 0, fixedHeight = (66-2*verticalSpacing_)/3;
	switch (layoutStyle)
	{
		// Full-size (normal) buttons
		case (TDynamicLayout::FullSizeHorizontalLayout):
			for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
			{
				TMenuButton* button = ri->item;

				// Set button style: ToolButtonTextUnderIcon, height = 66px, preferred width
				button->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
				button->setMinimumSize(24, 66);
				button->setMaximumSize(256, 66);
				button->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

				button->setGeometry(QRect(QPoint(left+w, top), button->sizeHint()));

				w += button->sizeHint().width() + horizontalSpacing_;
				h = 66;
			}
			break;
		case (TDynamicLayout::ButtonStackLayout):
			// First, set button style and get maximum hinted width over all buttons
			requiredWidth = 0;
			for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
			{
				TMenuButton* button = ri->item;
				button->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
				if (button->sizeHint().width() > requiredWidth) requiredWidth = button->sizeHint().width();
			}

			// Now set button geometries
			for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
			{
				TMenuButton* button = ri->item;

				// Set button style: ToolButtonTextBesideIcon, fixed height, horizontal width to maximum required for buttons
				button->setMinimumSize(requiredWidth, fixedHeight);
				button->setMaximumSize(requiredWidth, fixedHeight);
				button->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);

				button->setGeometry(QRect(left, top+h, requiredWidth, fixedHeight));

				h += fixedHeight + verticalSpacing_;
				w = requiredWidth;
			}
			break;
		case (TDynamicLayout::IconStackLayout):
			for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
			{
				TMenuButton* button = ri->item;

				// Set button style: ToolButtonIconOnly, fixed height == fixed width
				button->setToolButtonStyle(Qt::ToolButtonIconOnly);
				button->setMaximumHeight(fixedHeight);
				button->setMaximumWidth(fixedHeight);
				button->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);

				button->setGeometry(QRect(left, top+h, fixedHeight, fixedHeight));

				h += fixedHeight + verticalSpacing_;
				w = fixedHeight;
			}
			break;
	}
// 		printf("Item %p, x = %i, itemWidth = %i\n", item, left+w, item->sizeHint().width());
// 
// 		// Set geometry of item (actually, just position), and get position of next widget
// 		if (layoutStyle == TDynamicLayout::FullSizeHorizontalLayout)
// 		{
// 			item->setGeometry(QRect(QPoint(left+w, top), item->sizeHint()));
// 
// 			w += item->sizeHint().width() + horizontalSpacing_;
// 			h = top + item->sizeHint().height();
// 		}
// 		else
// 		{
// 			item->setGeometry(QRect(left, top+h, 64, 20));
// 
// 			h += item->sizeHint().height() + verticalSpacing_;
// 			if (item->sizeHint().width() > w) w = item->sizeHint().width();
// 		}
// 	}

	// Return total size used by layout (including any contents margin)
	return QSize(w+left+right, h+top+bottom);
}
