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

#include "main/aten.h"
#include "gui/about.h"

// Constructor
AtenAbout::AtenAbout(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);

	// Setup label contents
	char s[150];
	sprintf(s, "%s (from revision %s)", ATENVERSION, ATENREVISION);
	ui.VersionLabel->setText(s);
	ui.DateLabel->setText(ATENDATE);
	ui.UrlLabel->setText(ATENURL);
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
