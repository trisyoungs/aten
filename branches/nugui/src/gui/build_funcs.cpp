/*
	*** Build Dock Widget
	*** src/gui/build_funcs.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/build.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
BuildWidget::BuildWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
BuildWidget::~BuildWidget()
{
}

// Show window
void BuildWidget::showWidget()
{
	show();
}

void BuildWidget::on_AddAtomButton_clicked(bool on)
{
	if (ui.AddAtomFractionalCheck->isChecked())
	{
		CommandNode::run(Command::NewAtomFrac, "iddd", gui.mainWidget->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	else
	{
		CommandNode::run(Command::NewAtom, "iddd", gui.mainWidget->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	gui.update();
}

