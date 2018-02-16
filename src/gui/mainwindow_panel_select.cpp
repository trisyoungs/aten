/*
	*** Main Window - Select Panel Functions
	*** src/gui/mainwindow_panel_select.cpp
	Copyright T. Youngs 2007-2018

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
#include "main/aten.h"
#include "base/neta.h"
#include "base/neta_parser.h"

// Update select panel
void AtenWindow::updateSelectPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateSelectPanel");

	// Change text of selection type combo to reflect last auto-determined type
	int previousIndex = ui.SelectIntelligentTypeCombo->currentIndex();
	QStringList items;
	switch (lastSelectionType_)
	{
		case (RangeSelectType):
			items << "Auto (Range Selection)";
			break;
		case (NETASelectType):
			items << "Auto (NETA Description)";
			break;
		case (LoopSelectType):
			items << "Auto (Code Loop Selection)";
			break;
		default:
			items << "Auto (<unknown>)";
	}
	items << "Range Selection (1-10, C-Ag, 4+, +21, etc.)";
	items << "NETA Description";
	items << "Code Loop Selection";
	items << "Type Name Selection";
	ui.SelectIntelligentTypeCombo->clear();
	ui.SelectIntelligentTypeCombo->addItems(items);
	ui.SelectIntelligentTypeCombo->setCurrentIndex(previousIndex);

	Messenger::exit("AtenWindow::updateSelectPanel");
}

/*
 * Basic
 */

void AtenWindow::on_SelectBasicAllButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SelectAll, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_SelectBasicNoneButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SelectNone, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_SelectBasicInvertButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Invert, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_SelectBasicExpandButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Expand, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}

/*
 * Intelligent Select
 */

void AtenWindow::on_SelectIntelligentTargetCombo_currentTextChanged(const QString& text)
{
	// Empty text?
	if (text.isEmpty())
	{
		lastSelectionType_ = AtenWindow::nSelectTargetTypes;
		updateWidgets(AtenWindow::SelectPanelTarget);
		return;
	}

	// If the current selection type is not automatic, test specifically for that type of target. Otherwise, try to guess...
	bool valid = false;
	if (ui.SelectIntelligentTypeCombo->currentIndex() == AtenWindow::AutoSelectType) 
	{
		// See if we can determine what sort of text has been entered.
		Program program;
		Neta neta;

		// First, try code (since it is the most complicated)
		valid = true;
		if (program.generateFromString(QString("Atom i; %1").arg(text), "SelectionCode", "Selection Code", true, true, true)) lastSelectionType_ = AtenWindow::LoopSelectType;
		else if (NetaParser::createNeta(&neta, text, NULL, true) && neta.description()->innerNeta()) lastSelectionType_ = AtenWindow::NETASelectType;
		else if (CommandNode::run(Commands::TestSelect, "c", qPrintable(text)).asBool()) lastSelectionType_ = AtenWindow::RangeSelectType;
		else
		{
			lastSelectionType_ = AtenWindow::nSelectTargetTypes;
			valid = false;
		}
	}
	else if (ui.SelectIntelligentTypeCombo->currentIndex() == AtenWindow::RangeSelectType)
	{
		valid = CommandNode::run(Commands::TestSelect, "c", qPrintable(text)).asBool();
	}
	else if (ui.SelectIntelligentTypeCombo->currentIndex() == AtenWindow::NETASelectType)
	{
		Neta neta;
		valid = NetaParser::createNeta(&neta, text, NULL, true);
	}
	else if (ui.SelectIntelligentTypeCombo->currentIndex() == AtenWindow::LoopSelectType)
	{
		Program program;
		valid = program.generateFromString("Atom i; " + text, "SelectionCode", "Selection Code");
	}
	else if (ui.SelectIntelligentTypeCombo->currentIndex() == AtenWindow::NameSelectType)
	{
		valid = true;
	}

	// Change color of text to red if an unrecognised target
	QPalette palette = ui.SelectIntelligentTypeCombo->palette();
	if (!valid) palette.setColor(QPalette::Text, Qt::red);
	ui.SelectIntelligentTargetCombo->setPalette(palette);

	updateWidgets(AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_SelectIntelligentTypeCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;

	// Trigger currentTextChanged() signal to check text in combo box
	on_SelectIntelligentTargetCombo_currentTextChanged(ui.SelectIntelligentTargetCombo->currentText());
}

void AtenWindow::on_SelectIntelligentAddButton_clicked(bool checked)
{
	// Which selection are we going to try to perform?
	AtenWindow::SelectTargetType selectType = (AtenWindow::SelectTargetType) ui.SelectIntelligentTypeCombo->currentIndex();
	if (selectType == AtenWindow::AutoSelectType) selectType = lastSelectionType_;

	ReturnValue rv;

	switch (selectType)
	{
		case (AtenWindow::RangeSelectType):
			CommandNode::run(Commands::Select, "c", qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
		case (AtenWindow::NETASelectType):
			// Get element
			if (!ui.SelectIntelligentElementButton->callPopupMethod("currentElement", rv)) return;
			if (rv.asInteger() == 0) Messenger::print("Invalid element.'");
			else CommandNode::run(Commands::SelectType, "ic", rv.asInteger(), qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
		case (AtenWindow::LoopSelectType):
			CommandNode::run(Commands::SelectCode, "c", qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
		case (AtenWindow::NameSelectType):
			CommandNode::run(Commands::SelectName, "c", qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
    default:
      break;
	}

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_SelectIntelligentRemoveButton_clicked(bool checked)
{
	// Which selection are we going to try to perform?
	AtenWindow::SelectTargetType selectType = (AtenWindow::SelectTargetType) ui.SelectIntelligentTypeCombo->currentIndex();
	if (selectType == AtenWindow::AutoSelectType) selectType = lastSelectionType_;

	ReturnValue rv;

	switch (selectType)
	{
		case (AtenWindow::RangeSelectType):
			CommandNode::run(Commands::DeSelect, "c", qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
		case (AtenWindow::NETASelectType):
			// Get element
			if (!ui.SelectIntelligentElementButton->callPopupMethod("currentElement", rv)) return;
			if (rv.asInteger() == 0) Messenger::print("Invalid element.'");
			else CommandNode::run(Commands::DeSelectType, "ic", rv.asInteger(), qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
		case (AtenWindow::LoopSelectType):
			CommandNode::run(Commands::DeSelectCode, "c", qPrintable(ui.SelectIntelligentTargetCombo->currentText()));
			break;
    default:
      break;
	}

	updateWidgets(AtenWindow::AtomsTableTarget);
}
