/*
	*** Edit Actions
	*** src/gui/editactions.cpp
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
#include "gui/customdialog.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "parser/commandnode.h"

/*
// Editing Actions
*/

void AtenForm::on_actionEditUndo_triggered(bool checked)
{
	CommandNode::run(Command::Undo, "");
	gui.mainWidget->postRedisplay();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionEditRedo_triggered(bool checked)
{
	CommandNode::run(Command::Redo, "");
	gui.mainWidget->postRedisplay();
	gui.update(GuiQt::CanvasTarget);
}

void AtenForm::on_actionEditCut_triggered(bool checked)
{
	CommandNode::run(Command::Cut, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionEditCopy_triggered(bool checked)
{
	CommandNode::run(Command::Copy, "");
	gui.update(GuiQt::CanvasTarget);
}

void AtenForm::on_actionEditPaste_triggered(bool checked)
{
	CommandNode::run(Command::Paste, "");
	gui.mainWidget->postRedisplay();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionEditPasteTranslated_triggered(bool checked)
{
	// Static forest containing a single tree with variables and dialog control definitions
	static Tree dialog("Paste Translated", "option('Center of geometry of pasted atoms:', 'label', 'labelspan=6', 'left'); option('X', 'doublespin', -1e6, 1e6, 0.0, 1.0, 'newline'); option('Y', 'doublespin', -1e6, 1e6, 0.0, 1.0); option('Z', 'doublespin', -1e6, 1e6, 0.0, 1.0);");
	// Run the custom dialog
	if (dialog.executeCustomDialog())
	{
		Vec3<double> r = dialog.widgetValue3d("X", "Y", "Z");
		CommandNode::run(Command::Paste, "ddd", r.x, r.y, r.z);
		gui.mainWidget->postRedisplay();
		gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
	}
}

void AtenForm::on_actionEditDelete_triggered(bool checked)
{
	CommandNode::run(Command::Delete, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

/*
// Selection
*/

void AtenForm::on_actionSelectionAll_triggered(bool checked)
{
	CommandNode::run(Command::SelectAll, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionSelectionNone_triggered(bool checked)
{
	CommandNode::run(Command::SelectNone, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionSelectionInvert_triggered(bool checked)
{
	CommandNode::run(Command::Invert, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionSelectionExpand_triggered(bool on)
{
	CommandNode::run(Command::Expand, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}
