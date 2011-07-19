/*
	*** Qt GUI: About window functions
	*** src/gui/about_funcs.cpp
	Copyright T. Youngs 2007-2011

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

#include "main/version.h"
#include "gui/about.h"
#include "base/dnchar.h"
#include <cstdio>

// Constructor
AtenAbout::AtenAbout(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);

	// Setup label contents
	Dnchar label;
	label.sprintf("%s (from revision %s)", ATENVERSION, ATENREVISION);
	ui.VersionLabel->setText(label.get());
	ui.DateLabel->setText(ATENDATE);
	ui.UrlLabel->setText(ATENURL);
}

void AtenAbout::on_AboutCloseButton_clicked(bool checked)
{
	hide();
}

void AtenAbout::showWindow()
{
	show();
}
