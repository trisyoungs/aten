/*
	*** Qt GUI: About window functions
	*** src/gui/about_funcs.cpp
	Copyright T. Youngs 2007,2008

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

#include "base/master.h"
#include "gui/about.h"
#include <QtGui/QLabel>

// Constructor
AtenAbout::AtenAbout(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);

	// Setup label contents
	ui.VersionLabel->setText(ATENVERSION);
	ui.DateLabel->setText(ATENDATE);
	ui.UrlLabel->setText(ATENURL);
	char s[150];
	sprintf(s,"<a href=\"http://www.projectaten.org/doc/%s/manual.html\">http://www.projectaten.org/doc/%s/manual.html</a>",ATENVERSION,ATENVERSION);
	ui.ManualLabel->setText(s);
}

// Destructor
AtenAbout::~AtenAbout()
{
}

void AtenAbout::on_AboutCloseButton_clicked(bool checked)
{
	hide();
}

void AtenAbout::showWindow()
{
	show();
}
