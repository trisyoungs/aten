/*
	*** Select Dock Widget
	*** src/gui/select_funcs.cpp
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
#include "gui/select.h"
#include "gui/toolbox.h"
#include "gui/selectelement.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
SelectWidget::SelectWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

// Show window
void SelectWidget::showWidget()
{
	show();
	refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.SelectButton->setChecked(TRUE);
}

void SelectWidget::on_SelectAllButton_clicked(bool on)
{
	CommandNode::run(Command::SelectAll, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
}

void SelectWidget::on_SelectNoneButton_clicked(bool on)
{
	CommandNode::run(Command::SelectNone, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
}

void SelectWidget::on_SelectionExpandButton_clicked(bool on)
{
	CommandNode::run(Command::Expand, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
}

void SelectWidget::on_SelectionInvertButton_clicked(bool on)
{
	CommandNode::run(Command::Invert, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
}

void SelectWidget::on_SelectButton_clicked(bool on)
{
	CommandNode::run(Command::Select, "c", qPrintable(ui.SelectionCombo->currentText()));
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
}

void SelectWidget::on_DeselectButton_clicked(bool on)
{
	CommandNode::run(Command::DeSelect, "c", qPrintable(ui.SelectionCombo->currentText()));
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
}

void SelectWidget::on_TypeSelectElementButton_clicked(bool on)
{
	// Call the select element dialog...
	int newel = gui.selectElementDialog->selectElement();
	if (newel != -1) ui.TypeElementEdit->setText( elements().symbol(newel) );
}

void SelectWidget::on_SelectTypeButton_clicked(bool on)
{
	// Make sure we have a valid element
	int el = elements().find(qPrintable(ui.TypeElementEdit->text()));
	if (el == 0) msg.print("Invalid element '%s'\n", qPrintable(ui.TypeElementEdit->text()));
	else
	{
		CommandNode::run(Command::SelectType, "ic", el, qPrintable(ui.TypeNetaCombo->currentText()));
		gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
	}
}

void SelectWidget::on_DeselectTypeButton_clicked(bool on)
{
	// Make sure we have a valid element
	int el = elements().find(qPrintable(ui.TypeElementEdit->text()));
	if (el == 0) msg.print("Invalid element '%s'\n", qPrintable(ui.TypeElementEdit->text()));
	else
	{
		CommandNode::run(Command::DeSelectType, "ic", el, qPrintable(ui.TypeNetaCombo->currentText()));
		gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget);
	}
}

void SelectWidget::refresh()
{
	// If the select window is not visible, don't do anything
	if (!gui.selectWidget->isVisible()) return;

	Model *m = aten.currentModelOrFrame();

	// Update selection text details
	// First label, total number of selected atoms.
	Dnchar text(-1,"Total selected : %i\n", m->nSelected());
	ui.SelectionLabel1->setText(text.get());
	// Second label contains empirical formula of selection
	m->selectionEmpirical(text, FALSE, TRUE);
	ui.SelectionLabel2->setText(text.get());
}

void SelectWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.SelectButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();
	event->accept();
}
