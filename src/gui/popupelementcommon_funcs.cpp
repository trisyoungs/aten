/*
	*** Popup Widget - Common Elements
	*** src/gui/popupelementcommon_funcs.cpp
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

#include "gui/mainwindow.h"
#include "gui/popupelementcommon.h"

// Constructor
ElementCommonPopup::ElementCommonPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	ui.setupUi(this);

	currentElement_ = -1;

	// Create grid layout for widget
	QGridLayout* gl = new QGridLayout;
	gl->setMargin(0);

	// Parse element list
	LineParser parser;
	parser.getArgsDelim(0, prefs.commonElements());
	QPushButton* button;
	double* colour;
	int z, pos = 0;
	QPalette palette = this->palette();
	for (int n=0; n<parser.nArgs(); n++)
	{
		z = Elements().find(parser.argc(n));
		if (z > 0)
		{
			// Create button
			button = new QPushButton(this);
			button->setText(Elements().symbol(z));
			button->setMinimumSize(24,24);
			button->setMaximumSize(24,24);
			button->setToolTip(QString("%1 (%2)").arg(Elements().name(n), Elements().symbol(n)));
			colour = Elements().colour(z);
			button->setStyleSheet(QString("background-color:%1;").arg(QColor(int(colour[0]*255),int(colour[1]*255),int(colour[2]*255)).name(QColor::HexRgb)));
			QObject::connect(button, SIGNAL(clicked(bool)), this, SLOT(ElementButton_clicked(bool)));

			// Add it to the layout
			gl->addWidget(button, 0, pos++);
		}
		else Messenger::print("Unrecognised element '%s' not added to common elements list.", qPrintable(parser.argc(n)));
	}
	setLayout(gl);
}

// Update controls (before show()) (virtual)
void ElementCommonPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool ElementCommonPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "currentElement")
	{
		rv = currentElement_;
		return true;
	}
	else if (methodName == "setCurrentElement")
	{
		currentElement_ = rv.asInteger();

		// Set icon on parent button
		parentMenuButton()->setIcon(Elements().icon(currentElement_));

		return true;
	}
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

// Return clicked element value
void ElementCommonPopup::ElementButton_clicked(bool checked)
{
        // Cast sender
        QPushButton* button = qobject_cast<QPushButton*> (sender());
        if (!button)
        {
                printf("ElementCommonPopup::ElementButton_clicked - Sender was not a QPushButton.\n");
                currentElement_ = -1;
		done();
        }

	currentElement_ = Elements().find(button->text());

	// Set icon on parent button
	parentMenuButton()->setIcon(Elements().icon(currentElement_));

	done(true);
}
