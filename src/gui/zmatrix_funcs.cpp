/*
	*** Qt GUI: ZMatrix window functions
	*** src/gui/zmatrix_funcs.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/zmatrix.h"
#include "gui/gui.h"
#include "base/messenger.h"

// Constructor
AtenZMatrix::AtenZMatrix(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
	shouldRefresh_ = TRUE;
}

// Destructor
AtenZMatrix::~AtenZMatrix()
{
}

void AtenZMatrix::showWindow()
{
	if (shouldRefresh_) refresh();
	show();
}

// Refresh the zmatrix
void AtenZMatrix::refresh(bool forceupdate)
{
	msg.enter("AtenZMatrix::refresh");
	// If the atom list page is not visible, don't do anything
	if (!gui.zmatrixWindow->isVisible())
	{
		shouldRefresh_ = TRUE;
		msg.exit("AtenZMatrix::refresh");
		return;
	}
	Model *m = aten.currentModelOrFrame();
	msg.exit("AtenZMatrix::refresh");
}
