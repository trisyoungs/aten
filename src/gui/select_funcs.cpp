/*
	*** Qt select window functions
	*** src/gui/select_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/select.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
AtenSelect::AtenSelect(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
AtenSelect::~AtenSelect()
{
}

// Show window
void AtenSelect::showWindow()
{
	show();
	refresh();
}

void AtenSelect::on_SelectAllButton_clicked(bool on)
{
	CommandNode::run(Command::SelectAll, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenSelect::on_SelectNoneButton_clicked(bool on)
{
	CommandNode::run(Command::SelectNone, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenSelect::on_SelectionExpandButton_clicked(bool on)
{
	CommandNode::run(Command::Expand, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenSelect::on_SelectionInvertButton_clicked(bool on)
{
	CommandNode::run(Command::Invert, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenSelect::on_SelectButton_clicked(bool on)
{
	CommandNode::run(Command::Select, "c", qPrintable(ui.SelectionCombo->currentText()));
	gui.update(TRUE,FALSE,FALSE);
}

void AtenSelect::on_DeselectButton_clicked(bool on)
{
	CommandNode::run(Command::DeSelect, "c", qPrintable(ui.SelectionCombo->currentText()));
	gui.update(TRUE,FALSE,FALSE);
}

void AtenSelect::refresh()
{
	// If the select window is not visible, don't do anything
	if (!gui.selectWindow->isVisible()) return;

	Model *m = aten.currentModelOrFrame();

	// Update selection text details
	ui.SelectionText->clear();

	// First line, total number of selected atoms.
	Dnchar text;
	text.print("Total selected : %i\n", m->nSelected());
	ui.SelectionText->append(text.get());
	// Next follows empirica formula of selection
	m->selectionEmpirical(text, FALSE);
	ui.SelectionText->append(text.get());
}

void AtenSelect::dialogFinished(int result)
{
	gui.mainWindow->ui.actionSelectWindow->setChecked(FALSE);
}
