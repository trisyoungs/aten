/*
	*** Qt atomlist functions interface
	*** src/gui/atomlist_funcs.cpp
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

#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/glyphs.h"
#include "model/model.h"
#include "base/aten.h"

// Constructor
AtenGlyphs::AtenGlyphs(QWidget *parent)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Destructor
AtenGlyphs::~AtenGlyphs()
{
}

void AtenGlyphs::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

void AtenGlyphs::dialogFinished(int result)
{
//	gui.mainWindow->ui.actionGlyphsDialog->setChecked(FALSE);
}
