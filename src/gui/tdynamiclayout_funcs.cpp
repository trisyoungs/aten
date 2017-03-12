/*
	*** TDynamicLayout Functions
	*** src/gui/tdynamiclayout_funcs.cpp
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
	upperLayoutStyle_ = TDynamicLayout::IconStackLayout;
}

TDynamicLayout::TDynamicLayout(int margin, int hSpacing, int vSpacing) : QLayout(NULL)
{
	setContentsMargins(margin, margin, margin, margin);
	horizontalSpacing_ = (hSpacing == -1 ? 4 : hSpacing);
	verticalSpacing_ = (vSpacing == -1 ? 4 : vSpacing);
	currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
	upperLayoutStyle_ = TDynamicLayout::IconStackLayout;
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

	// Attempt to cast the child into a QToolButton
	QToolButton* button = qobject_cast<QToolButton*>(item->widget());
	if (button)
	{
		Messenger::print(Messenger::Verbose, "Dynamic layout '%s' now contains QToolButton '%s'\n", qPrintable(objectName()), qPrintable(button->text()));
		buttons_.add(button);
	}
	else
	{
		printf("Internal Error: TDynamicLayout was given a widget that isn't a QToolButton.\n");
		return;
	}

	// Adjust upper layout style?
	if (button->toolButtonStyle() == Qt::ToolButtonTextUnderIcon) upperLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
	else if (button->toolButtonStyle() == Qt::ToolButtonTextBesideIcon)
	{
		if (upperLayoutStyle_ != TDynamicLayout::FullSizeHorizontalLayout) upperLayoutStyle_ = TDynamicLayout::ButtonStackLayout;
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
	if (sizeHints_[TDynamicLayout::IconStackLayout].isValid()) return sizeHints_[TDynamicLayout::IconStackLayout];
	else return QSize(30,66);
}

// Set new geometry for the layout
void TDynamicLayout::setGeometry(const QRect &rect)
{
	QLayout::setGeometry(rect);

	// Get margins and adjust available area
	int left, top, right, bottom;
	getContentsMargins(&left, &top, &right, &bottom);
	QRect availableRect = rect;
	availableRect.adjust(left, top, -right, -bottom);

	// Calculate some basic quantities
	int buttonHeight = (availableRect.height() - verticalSpacing_) / 2;
	int nColumns = buttons_.nItems()/2;
	if (buttons_.nItems()%2 == 1) ++nColumns;

	// Let us see what will fit!
	// Is the supplied rect (specifically the width) big enough to fulfil the sizeHint()
	if ((upperLayoutStyle_ == TDynamicLayout::FullSizeHorizontalLayout) && (availableRect.width() >= sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].width()))
	{
		// Draw buttons at full size.
		int x = 0;
		for (RefListItem<QToolButton,int>* ri = buttons_.first(); ri != NULL; ri = ri->next)
		{
			QToolButton* button = ri->item;

			// Set button style: ToolButtonTextUnderIcon, height = 66px, preferred width
			button->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
			button->setIconSize(QSize(30, 30));
			button->setMinimumSize(24, availableRect.height());
			button->setMaximumSize(256, availableRect.height());
			button->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

			button->setGeometry(QRect(QPoint(left+x, top), button->sizeHint()));

			x += button->sizeHint().width() + horizontalSpacing_;
		}
		currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;

		// Update the full size hint while we are here.
		sizeHints_[TDynamicLayout::FullSizeHorizontalLayout] = QSize(x, availableRect.height()+top+bottom);
	}
	else if ((upperLayoutStyle_ >= TDynamicLayout::ButtonStackLayout) && (availableRect.width() >= sizeHints_[TDynamicLayout::ButtonStackLayout].width()))
	{
		// Draw widgets as a stack of buttons (text beside icon)
		// Draw each pair of widgets as a stack, dividing any extra space in the given rect() between each stack
		// We'll use the refitem->data member to store the width of the column the button is in

		// Set styles of menu buttons to text beside icon, and put required widths in data members
		for (RefListItem<QToolButton,int>* ri = buttons_.first(); ri != NULL; ri = ri->next)
		{
			QToolButton* button = ri->item;

			// Set button style: ToolButtonTextBesideIcon, fixed height, horizontal width to maximum required for buttons
			button->setIconSize(QSize(14, 14));
			button->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
			button->setMinimumSize(20, buttonHeight);
			button->setMaximumSize(availableRect.width(), buttonHeight);
			button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
			ri->data = button->sizeHint().width();
		}

		// Now, set column (pair widths) and calculate maximum width required
		int usedWidth = 0;
		for (int n=0; n<buttons_.nItems(); n+=2)
		{
			// Is there a second button in this pair?
			if ((n+1) < buttons_.nItems())
			{
				if (buttons_[n]->data > buttons_[n+1]->data) buttons_[n+1]->data = buttons_[n]->data;
				else buttons_[n]->data = buttons_[n+1]->data;
			}

			usedWidth += buttons_[n]->data;
			if (n > 0) usedWidth += horizontalSpacing_;
		}

		// Distribute extra space between columns
		int extraWidth = (availableRect.width() - usedWidth) / nColumns;
		if (extraWidth < 0) extraWidth = 0;

		// OK - all done - set geometries of the buttons
		int x = 0, y = 0;
		int minimumWidth = (nColumns-1) * horizontalSpacing_;
		QToolButton* button;
		for (int n=0; n<buttons_.nItems(); n+=2)
		{
			// First button
			button = buttons_[n]->item;
			button->setGeometry(QRect(left+x, top+y, buttons_[n]->data + extraWidth, buttonHeight));
			y += buttonHeight + verticalSpacing_;
			minimumWidth += buttons_[n]->data;

			// Second button (if there is one)
			if ((n+1) < buttons_.nItems())
			{
				button = buttons_[n+1]->item;
				button->setGeometry(QRect(left+x, top+y, buttons_[n+1]->data + extraWidth, buttonHeight));
				x += buttons_[n+1]->data + extraWidth;
				if (n > 0) x += horizontalSpacing_;
				y = 0;
			}
		}

		currentLayoutStyle_ = TDynamicLayout::ButtonStackLayout;

		// Update the size hint
		sizeHints_[TDynamicLayout::ButtonStackLayout] = QSize(minimumWidth, availableRect.height()+top+bottom);

		// If the required width for these buttons is more than that available to the layout, re-layout the widgets
		if (minimumWidth > availableRect.width()) setGeometry(availableRect);
	}
	else if ((upperLayoutStyle_ >= TDynamicLayout::IconStackLayout) && (availableRect.width() >= sizeHints_[TDynamicLayout::IconStackLayout].width()))
	{
		// Draw widgets as stacks of icon-only buttons
		// All buttons will have the same width
		int buttonWidth = (availableRect.width() - (nColumns-1)*horizontalSpacing_) / nColumns;

		// Set button styles
		for (RefListItem<QToolButton,int>* ri = buttons_.first(); ri != NULL; ri = ri->next)
		{
			QToolButton* button = ri->item;

			// Set button style: ToolButtonTextBesideIcon, fixed height, horizontal width to maximum required for buttons
			button->setIconSize(QSize(14, 14));
			button->setToolButtonStyle(Qt::ToolButtonIconOnly);
			button->setMinimumSize(20, buttonHeight);
			button->setMaximumSize(availableRect.width(), buttonHeight);
			button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
		}

		// OK - all done - set geometries of the buttons
		int x = 0, y = 0;
		int minimumWidth = (nColumns-1) * horizontalSpacing_;
		QToolButton* button;
		for (int n=0; n<buttons_.nItems(); n+=2)
		{
			// First button
			button = buttons_[n]->item;
			button->setGeometry(QRect(left+x, top+y, buttonWidth, buttonHeight));
			y += buttonHeight + verticalSpacing_;
			minimumWidth += button->sizeHint().width();

			// Second button (if there is one)
			if ((n+1) < buttons_.nItems())
			{
				button = buttons_[n+1]->item;
				button->setGeometry(QRect(left+x, top+y, buttonWidth, buttonHeight));
				x += buttonWidth;
				if (n > 0) x += horizontalSpacing_;
				y = 0;
			}
		}

		currentLayoutStyle_ = TDynamicLayout::IconStackLayout;

		// Update the size hint
		sizeHints_[TDynamicLayout::IconStackLayout] = QSize(minimumWidth, availableRect.height()+top+bottom);
	}
}

// Return size hint
QSize TDynamicLayout::sizeHint() const
{
	// If our maximum size hint is valid, return it. Otherwise, calculate it first
	if (sizeHints_[upperLayoutStyle_].isValid()) return sizeHints_[upperLayoutStyle_];

	// Loop over items
	QSize size;
	for (int n=0; n<count(); ++n)
	{
		if (n == 0) size = itemAt(n)->sizeHint();
		else size += QSize(itemAt(n)->sizeHint().width() + horizontalSpacing_, 0);
	}

	return size;
}
