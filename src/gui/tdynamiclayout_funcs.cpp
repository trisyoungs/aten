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
	setSizeConstraint(QLayout::SetNoConstraint);
	currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
}

TDynamicLayout::TDynamicLayout(int margin, int hSpacing, int vSpacing) : QLayout(NULL)
{
	setContentsMargins(margin, margin, margin, margin);
	horizontalSpacing_ = (hSpacing == -1 ? 4 : hSpacing);
	verticalSpacing_ = (vSpacing == -1 ? 4 : vSpacing);
	setSizeConstraint(QLayout::SetNoConstraint);
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
	if (fullSizeHint_.isValid()) return QSize(30,fullSizeHint_.height());
	else return QSize(30,66);
}

// Set new geometry for the layout
void TDynamicLayout::setGeometry(const QRect &rect)
{
	QLayout::setGeometry(rect);
	printf("SETGEOMETRY was called: available width = %i, available height = %i, right = %i\n", rect.width(), rect.height(), rect.right());

	// Get margins and adjust available area
	int left, top, right, bottom;
	getContentsMargins(&left, &top, &right, &bottom);
	rect.adjusted(left, top, -right, -bottom);
// 	printf("contents margins = %i %i %i %i\n", left, top, right, bottom);
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
	if (rect.width() >= fullSizeHint_.width())
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

			printf("FULLSIZE '%s' - hint = %ix%i\n", qPrintable(button->text()), button->sizeHint().width(), button->sizeHint().height());
			w += button->sizeHint().width() + horizontalSpacing_;
		}
		currentLayoutStyle_ = TDynamicLayout::FullSizeHorizontalLayout;
		// Reset the full size hint while we are here.
		fullSizeHint_ = QSize(w,rect.height()+top+bottom);
	}
	else
	{
		// Need to draw widgets as a stack of some form
		// First, set button style and get maximum hinted width over all buttons
		int requiredWidth = 0;
		for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
		{
			TMenuButton* button = ri->item;
			button->setIconSize(QSize(14, 14));
			button->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
			if (button->sizeHint().width() > requiredWidth) requiredWidth = button->sizeHint().width();
		}

		int buttonHeight = (rect.height() - (buttons.nItems()-1)*verticalSpacing_) / buttons.nItems();
		if (rect.width() >= requiredWidth)
		{
			// Can draw as full buttons (text+icon)
			for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
			{
				TMenuButton* button = ri->item;

				// Set button style: ToolButtonTextBesideIcon, fixed height, horizontal width to maximum required for buttons
				button->setMinimumSize(20, buttonHeight);
				button->setMaximumSize(rect.width(), buttonHeight);
				button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);

				button->setGeometry(QRect(left, top+h, rect.width(), buttonHeight));
				printf("STACK '%s' - hint = %ix%i\n", qPrintable(button->text()), button->sizeHint().width(), button->sizeHint().height());

				h += buttonHeight + verticalSpacing_;
			}
		}
		else
		{
			// Draw as icon buttons only
			for (RefListItem<TMenuButton,int>* ri = buttons.first(); ri != NULL; ri = ri->next)
			{
				TMenuButton* button = ri->item;

				// Set button style: ToolButtonIconOnly, fixed height == fixed width
				button->setToolButtonStyle(Qt::ToolButtonIconOnly);
				button->setIconSize(QSize(14, 14));
				button->setMinimumSize(20, buttonHeight);
				button->setMaximumSize(rect.width(), buttonHeight);
				button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);

				button->setGeometry(QRect(left, top+h, rect.width(), buttonHeight));
				printf("ICONS '%s' - hint = %ix%i\n", qPrintable(button->text()), button->sizeHint().width(), button->sizeHint().height());

				h += buttonHeight + verticalSpacing_;
			}
		}
	}

	// Store new size for current layout
// 	sizeHints_[currentLayoutStyle_] = usedSize;
// 	printf("STORED SIZE FOR LAYOUT TYPE %i : %ix%i\n", currentLayoutStyle_, sizeHints_[currentLayoutStyle_].width(), sizeHints_[currentLayoutStyle_].height());

	// Set geometry of parent to ideal size
// 	parentWidget()->setGeometry(parentWidget()->x(), parentWidget()->y(), usedSize.width(), usedSize.height());
// 	parentWidget()->updateGeometry();
}

// Return size hint
QSize TDynamicLayout::sizeHint() const
{
	// If our fullSizeHint_ is valid, return it. Otherwise, calculate it first
	if (fullSizeHint_.isValid())
	{
		printf("sizehint is returning %ix%i\n", fullSizeHint_.width(), fullSizeHint_.height());
		return fullSizeHint_;
	}

	// Loop over items
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

}
