/*
	*** Qt GUI: Select element functions
	*** src/gui/selectelement_funcs.cpp
	Copyright T. Youngs 2007-2009

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

#include "gui/selectelement.h"
#include "base/parser.h"
#include "base/sysfunc.h"

// Constructor
AtenSelectElement::AtenSelectElement(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);
}

// Set controls
void AtenSelectElement::setControls()
{
}

// Finalise GUI
void AtenSelectElement::finaliseUi()
{
	// Create periodic table buttons
	QGridLayout *gl = new QGridLayout;
	QLabel *label;
	int n, m, z;
	GLfloat *colour;

	// Create element button array (and buttons)
	elementButtons_ = new QPushButton*[elements().nElements()];
	for (n=1; n<elements().nElements(); n++)
	{
		elementButtons_[n] = new QPushButton(this);
		elementButtons_[n]->setText(elements().symbol(n));
		elementButtons_[n]->setMinimumSize(24,24);
		elementButtons_[n]->setMaximumSize(24,24);
		colour = elements().ambientColour(n);
		elementButtons_[n]->setPalette(QPalette(qRgb(int(colour[0]*255),int(colour[1]*255),int(colour[2]*255))));
		QObject::connect(elementButtons_[n], SIGNAL(clicked(bool)), this, SLOT(ElementButton_clicked(bool)));
	}

	// Nnow add buttons to gridlayout
	// First row - Group Number
	for (n=1; n<19; n++)
	{
		label = new QLabel(this);
		label->setText(itoa(n));
		gl->addWidget(label, 0, n);
		//cs_label(itoa(n),table,n,n+1,0,1);
	}

	// First column - Period number
	for (n=1; n<8; n++)
	{
		label = new QLabel(this);
		label->setText(itoa(n));
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

	ui.PeriodicTableGroup->setLayout(gl);

	// Create common element buttons....
	QHBoxLayout *hbox = new QHBoxLayout(ui.CommonGroup);
	//ui.CommonGroup->setLayout()
	// Parse prefs value 
	parser.getArgsDelim(prefs.commonElements(), Parser::Defaults);
	for (n=0; n<parser.nArgs(); n++)
	{
		z = elements().find(parser.argc(n));
		if (z > 0)
		{
			// Create button
			QPushButton *button = addCommonButton(z);
			commonButtons_.add(button, z);
			// Add it to the layout
			hbox->addWidget(button);
		}
		else msg.print( "Unrecognised element '%s' not added to common elements list.\n", parser.argc(n));
	}

}

QPushButton *AtenSelectElement::addCommonButton(int el)
{
	QPushButton *button = new QPushButton(this);
	button->setText(elements().symbol(el));
	button->setMinimumSize(24,24);
	button->setMaximumSize(24,24);
	GLfloat *colour = elements().ambientColour(el);
	button->setPalette(QPalette(qRgb(int(colour[0]*255),int(colour[1]*255),int(colour[2]*255))));
	QObject::connect(button, SIGNAL(clicked(bool)), this, SLOT(CommonElementButton_clicked(bool)));
	return button;
}

// Cancel dialog
void AtenSelectElement::on_CancelButton_clicked(bool checked)
{
	reject();
}

// Return clicked element value
void AtenSelectElement::ElementButton_clicked(bool checked)
{
        // Cast sender
        QPushButton *button = qobject_cast<QPushButton*> (sender());
        if (!button)
        {
                printf("AtenSelectElement::ElementButton_clicked - Sender was not a QPushButton.\n");
                reject();
		return;
        }

	int result;
	for (result=1; result<elements().nElements(); result++) if (elementButtons_[result] == button) break;
	selectedElement_ = (result != elements().nElements() ? result : -1);
	accept();
}

// Return clicked common element value
void AtenSelectElement::CommonElementButton_clicked(bool checked)
{
	// Cast sender
	QPushButton *button = qobject_cast<QPushButton*> (sender());
	if (!button)
	{
		printf("AtenSelectElement::CommonElementButton_clicked - Sender was not a QPushButton.\n");
		reject();
		return;
	}

	Refitem<QPushButton, int> *ri;
	for (ri = commonButtons_.first(); ri != NULL; ri = ri->next) if (ri->item == button) break;
	selectedElement_ = (ri != NULL ? ri->data : -1);
	accept();
}

// Select an element
int AtenSelectElement::selectElement()
{
	// Execute the dialog and check on the result
	return (exec() == 1 ? selectedElement_ : -1);
}
