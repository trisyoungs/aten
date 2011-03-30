/*
	*** Model Actions
	*** src/gui/modelactions.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Rename model
void AtenForm::on_actionModelRename_triggered(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		CommandNode::run(Command::SetName, "c", qPrintable(text));
		updateWindowTitle();
	}
}

// Fold atoms in model
void AtenForm::on_actionModelFoldAtoms_triggered(bool checked)
{
	CommandNode::run(Command::Fold, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Fold molecules in model
void AtenForm::on_actionModelFoldMolecules_triggered(bool checked)
{
	CommandNode::run(Command::FoldMolecules, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Move to next model in list
void AtenForm::on_actionModelNext_triggered(bool checked)
{
	// If multiple models are visible, step along to next visible model. Otherwise, just next in list
	if (aten.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		Refitem<Model,int> *ri;
		for (ri = aten.visibleModels(); ri != NULL; ri = ri->next) if (ri->item == aten.currentModel()) break;
		if (ri == NULL)
		{
			printf("Internal Error : Failed to find current model in visible models list.\n");
			return;
		}
		aten.setCurrentModel(ri->next == NULL ? aten.visibleModels()->item : ri->next->item);
	}
	else
	{
		Model *m = aten.currentModel();
		aten.setCurrentModel(m->next == NULL ? aten.models() : m->next);
	}
	gui.update(GuiQt::CanvasTarget+GuiQt::AllTarget-GuiQt::ModelsTarget);
}

// Move to previous model in list
void AtenForm::on_actionModelPrevious_triggered(bool checked)
{
	// If multiple models are visible, step back to previous visible model. Otherwise, just previous in list
	if (aten.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		Refitem<Model,int> *ri;
		for (ri = aten.visibleModels(); ri != NULL; ri = ri->next) if (ri->item == aten.currentModel()) break;
		if (ri == NULL)
		{
			printf("Internal Error : Failed to find current model in visible models list.\n");
			return;
		}
		aten.setCurrentModel(ri->prev == NULL ? aten.visibleModels()->item : ri->prev->item);
	}
	else
	{
		Model *m = aten.currentModel();
		aten.setCurrentModel(m->prev == NULL ? aten.models() : m->prev);
	}
	gui.update(GuiQt::CanvasTarget+GuiQt::AllTarget-GuiQt::ModelsTarget);
}

// Show all atoms in current model
void AtenForm::on_actionModelShowAll_triggered(bool checked)
{
	CommandNode::run(Command::ShowAll, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// List all measurements in model
void AtenForm::on_actionListMeasurements_triggered(bool on)
{
	aten.currentModelOrFrame()->listMeasurements();
}
