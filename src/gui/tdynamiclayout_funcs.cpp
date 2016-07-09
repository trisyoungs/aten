/*
	*** TDynamicLayout Functions
	*** src/gui/tdynamiclayout_funcs.cpp
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
	if (sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].isValid()) return QSize(30,sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].height());
	else return QSize(30,66);
}

// Set new geometry for the layout
void TDynamicLayout::setGeometry(const QRect &rect)
{
	QLayout::setGeometry(rect);

	// Get margins and adjust available area
	int left, top, right, bottom;
	getContentsMargins(&left, &top, &right, &bottom);
	rect.adjusted(left, top, -right, -bottom);
	int w = 0;
	int h = 0;

	// Construct a list of TMenuButtons for convenience
	RefList<TMenuButton,int> buttons;
	QLayoutItem* item;
	foreach (item, items_)
	{
		// Grab the widget and cast it into a TMenuButton
		QWidget* wid = item->widget();
		TMenuButton* button = qobject_cast<TMenuButton*>(wid);
		if (!button)
		{
			printf("Internal Error: Couldn't cast TDynamicLayout child widget into a TMenuButton.\n");
			return;
		}
		buttons.add(button);
	}

	QSize usedSize;
	// Let us see what will fit!
	// Is the supplied rect (specifically the width) big enough to fulfil the sizeHint()
	if (rect.width() >= sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].width())
	{
		// Draw buttons at full size.
		for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
		{
			TMenuButton* button = ri->item;

			// Set button style: ToolButtonTextUnderIcon, height = 66px, preferred width
			button->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
			button->setIconSize(QSize(30, 30));
			button->setMinimumSize(24, rect.height());
			button->setMaximumSize(256, rect.height());
			button->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

			button->setGeometry(QRect(QPoint(left+w, top), button->sizeHint()));

			w += button->sizeHint().width() + horizontalSpacing_;
		}
		currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;

		// Update the full size hint while we are here.
		sizeHints_[TDynamicLayout::FullSizeHorizontalLayout] = QSize(w,rect.height()+top+bottom);
	}
	else if (rect.width() >= sizeHints_[TDynamicLayout::ButtonStackLayout].width())
	{
		// Draw widgets as a stack of buttons (text beside icon)
		int requiredWidth = 0;
		int buttonHeight = (rect.height() - (buttons.nItems()-1)*verticalSpacing_) / buttons.nItems();
		for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
		{
			TMenuButton* button = ri->item;

			// Set button style: ToolButtonTextBesideIcon, fixed height, horizontal width to maximum required for buttons
			button->setIconSize(QSize(14, 14));
			button->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
			button->setMinimumSize(20, buttonHeight);
			button->setMaximumSize(rect.width(), buttonHeight);
			button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
			if (button->sizeHint().width() > requiredWidth) requiredWidth = button->sizeHint().width();
		}
		for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
		{
			TMenuButton* button = ri->item;
			button->setGeometry(QRect(left, top+h, rect.width(), buttonHeight));
			h += buttonHeight + verticalSpacing_;
		}
		currentLayoutStyle_ = TDynamicLayout::ButtonStackLayout;

		// Update the full size hint while we are here.
		sizeHints_[TDynamicLayout::ButtonStackLayout] = QSize(requiredWidth, rect.height()+top+bottom);

		// If the required width for these buttons is more than that available to the layout, re-layout the widgets
		if (requiredWidth > rect.width()) setGeometry(rect);
	}
	else if (rect.width() >= sizeHints_[TDynamicLayout::IconStackLayout].width())
	{
		// Draw widgets as a stack of buttons (text beside icon)
		int requiredWidth = 0;
		int buttonHeight = (rect.height() - (buttons.nItems()-1)*verticalSpacing_) / buttons.nItems();
		for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
		{
			TMenuButton* button = ri->item;

			// Set button style: ToolButtonTextBesideIcon, fixed height, horizontal width to maximum required for buttons
			button->setIconSize(QSize(14, 14));
			button->setToolButtonStyle(Qt::ToolButtonIconOnly);
			button->setMinimumSize(20, buttonHeight);
			button->setMaximumSize(rect.width(), buttonHeight);
			button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
			if (button->sizeHint().width() > requiredWidth) requiredWidth = button->sizeHint().width();
		}
		for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
		{
			TMenuButton* button = ri->item;
			button->setGeometry(QRect(left, top+h, rect.width(), buttonHeight));
			h += buttonHeight + verticalSpacing_;
		}
		currentLayoutStyle_ = TDynamicLayout::IconStackLayout;

		// Update the full size hint while we are here.
		sizeHints_[TDynamicLayout::IconStackLayout] = QSize(requiredWidth, rect.height()+top+bottom);
	}
}

// Return size hint
QSize TDynamicLayout::sizeHint() const
{
	// If our fullSizeHint_ is valid, return it. Otherwise, calculate it first
	if (sizeHints_[TDynamicLayout::FullSizeHorizontalLayout].isValid()) return sizeHints_[TDynamicLayout::FullSizeHorizontalLayout];

	// Loop over items
	QSize size;
	for (int n=0; n<count(); ++n)
	{
		if (n == 0) size = itemAt(n)->sizeHint();
		else size += QSize(itemAt(n)->sizeHint().width() + horizontalSpacing_, 0);
	}

	return size;
}
