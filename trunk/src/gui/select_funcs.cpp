/*
	*** Select Dock Widget
	*** src/gui/select_funcs.cpp
	Copyright T. Youngs 2007-2015

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

#include <QCloseEvent>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/select.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
SelectWidget::SelectWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	// Private variables
	refreshing_ = false;

	ui.setupUi(this);
}

// Show window
void SelectWidget::showWidget()
{
	show();
	refresh();
}

void SelectWidget::setHistories(QStringList select, QStringList forlist, QStringList netalist)
{
	refreshing_ = true;
	ui.SelectCombo->addItems(select);
	ui.SelectForCombo->addItems(forlist);
	ui.SelectNetaCombo->addItems(netalist);
	ui.SelectCombo->setCurrentIndex(-1);
	ui.SelectForCombo->setCurrentIndex(-1);
	ui.SelectNetaCombo->setCurrentIndex(-1);
	refreshing_ = false;
}

void SelectWidget::on_SelectAllButton_clicked(bool on)
{
	CommandNode::run(Commands::SelectAll, "");
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_SelectNoneButton_clicked(bool on)
{
	CommandNode::run(Commands::SelectNone, "");
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_SelectionExpandButton_clicked(bool on)
{
	CommandNode::run(Commands::Expand, "");
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_SelectionInvertButton_clicked(bool on)
{
	CommandNode::run(Commands::Invert, "");
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_SelectButton_clicked(bool on)
{
	CommandNode::run(Commands::Select, "c", qPrintable(ui.SelectCombo->currentText()));
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_DeselectButton_clicked(bool on)
{
	CommandNode::run(Commands::DeSelect, "c", qPrintable(ui.SelectCombo->currentText()));
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_TypeSelectElementButton_clicked(bool on)
{
// 	AtenSelectElement elementSelect(parent_);
// 	int newel = elementSelect.selectElement();
// 	if (newel != -1) ui.TypeElementEdit->setText( Elements().symbol(newel) );
	// ATEN2 TODO
}

void SelectWidget::on_SelectCombo_currentIndexChanged(int n)
{
	if (refreshing_) return;
}

void SelectWidget::on_SelectTypeButton_clicked(bool on)
{
	// Make sure we have a valid element
	int el = Elements().find(qPrintable(ui.TypeElementEdit->text()));
	if (el == 0) Messenger::print("Invalid element '%s'", qPrintable(ui.TypeElementEdit->text()));
	else
	{
		CommandNode::run(Commands::SelectType, "ic", el, qPrintable(ui.SelectNetaCombo->currentText()));
		parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
	}
}

void SelectWidget::on_DeselectTypeButton_clicked(bool on)
{
	// Make sure we have a valid element
	int el = Elements().find(qPrintable(ui.TypeElementEdit->text()));
	if (el == 0) Messenger::print("Invalid element '%s'", qPrintable(ui.TypeElementEdit->text()));
	else
	{
		CommandNode::run(Commands::DeSelectType, "ic", el, qPrintable(ui.SelectNetaCombo->currentText()));
		parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
	}
}

void SelectWidget::on_SelectForButton_clicked(bool on)
{
	CommandNode::run(Commands::SelectType, "c", qPrintable(ui.SelectForCombo->currentText()));
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::on_DeselectForButton_clicked(bool on)
{
	CommandNode::run(Commands::DeSelectType, "c", qPrintable(ui.SelectForCombo->currentText()));
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void SelectWidget::refresh()
{
	Model* m = parent_.aten().currentModelOrFrame();

	// Update selection text details
	// First label, total number of selected atoms.
	ui.SelectionLabel1->setText("Total selected : " + (m ? QString::number(m->nSelected()) : "0"));
	
	// Second label contains empirical formula of selection
	if (m) ui.SelectionLabel2->setText(m ? m->selectionEmpirical(false, true) : "--");
}

void SelectWidget::closeEvent(QCloseEvent* event)
{
	event->accept();
}
