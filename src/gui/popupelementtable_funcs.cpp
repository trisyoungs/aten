/*
	*** Popup Widget - Element Table
	*** src/gui/popupelementtable_funcs.cpp
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

#include "gui/mainwindow.h"
#include "gui/popupelementtable.h"

// Constructor
ElementTablePopup::ElementTablePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	ui.setupUi(this);

	currentElement_ = -1;

	// Create grid layout for widget
	QGridLayout* gl = new QGridLayout;
	gl->setMargin(0);

	// Create periodic table buttons
	QLabel* label;
	int n, m, z;
	double* colour;

	// Create element button array (and buttons)
	QPushButton* button;
	QPalette palette = this->palette();
	for (n=0; n<Elements().nElements(); ++n)
	{
		button = new QPushButton(this);
		elementButtons_ << button;
		button->setText(Elements().symbol(n));
		button->setMinimumSize(24,24);
		button->setMaximumSize(24,24);
		button->setToolTip(QString("%1 (%2)").arg(Elements().name(n), Elements().symbol(n)));
		colour = Elements().colour(n);
		button->setStyleSheet(QString("background-color:%1;").arg(QColor(int(colour[0]*255),int(colour[1]*255),int(colour[2]*255)).name(QColor::HexRgb)));
		
		QObject::connect(button, SIGNAL(clicked(bool)), this, SLOT(ElementButton_clicked(bool)));
	}

	// Hide 'XX' button
	elementButtons_[0]->hide();

	// Now add buttons to gridlayout
	// First row - Group Number
	for (n=1; n<19; n++)
	{
		label = new QLabel(this);
		label->setText(QString::number(n));
		gl->addWidget(label, 0, n);
	}

	// First column - Period number
	for (n=1; n<8; n++)
	{
		label = new QLabel(this);
		label->setText(QString::number(n));
		gl->addWidget(label, n, 0);
	}

	// H, He
	gl->addWidget(elementButtons_[1],1,1);
	gl->addWidget(elementButtons_[2],1,18);

	// Groups 1-2 (periods 1-6) [s]
	z = 3;
	for (n=0; n<6; n++)
	{
		gl->addWidget(elementButtons_[z],n+2,1);
		gl->addWidget(elementButtons_[z+1],n+2,2);
		z += 8;
		if (n > 1) z += 10;
		if (n > 3) z += 14;
	}

	// Groups 13-18 (periods 1-6) [p]
	z = 5;
	for (n=0; n<6; n++)
	{
		for (m=0; m<6; m++) gl->addWidget(elementButtons_[z+m],n+2,13+m);
		z += 8;
		if (n > 0) z += 10;
		if (n > 2) z += 14;
	}

	// Groups 3-8 (periods 3-6) [p]
	z = 21;
	for (n=0; n<4; n++)
	{
		for (m=0; m<10; m++) gl->addWidget(elementButtons_[z+m],n+4,3+m);
		if (n == 1) z += 14;
		z += 18;
		if (n > 1) z += 14;
	}

	label = new QLabel(this);
	label->setText("Ln");
	gl->addWidget(label, 9, 0);
	label = new QLabel(this);
	label->setText("An");
	gl->addWidget(label, 10, 0);

	// Lanthanoids and Actinoids
	z = 57;
	for (n=0; n<14; n++)
	{
		gl->addWidget(elementButtons_[z+n],9,3+n);
		gl->addWidget(elementButtons_[z+n+32],10,3+n);
	}

	setLayout(gl);
}

// Update controls (before show()) (virtual)
void ElementTablePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool ElementTablePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
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
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Widget Functions
 */

// Return clicked element value
void ElementTablePopup::ElementButton_clicked(bool checked)
{
        // Cast sender
        QPushButton* button = qobject_cast<QPushButton*> (sender());
        if (!button)
        {
                printf("ElementTablePopup::ElementButton_clicked - Sender was not a QPushButton.\n");
		currentElement_ = -1;
		done();
        }

	currentElement_ = Elements().find(button->text());

	// Set icon on parent button
	parentMenuButton()->setIcon(Elements().icon(currentElement_));

	done(true);
}
