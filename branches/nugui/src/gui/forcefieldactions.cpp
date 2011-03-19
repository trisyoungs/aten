/*
	*** Forcefield Menu Actions
	*** src/gui/forcefieldactions.cpp
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
#include "gui/forcefields.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Open forcefield file
void AtenForm::on_actionFileOpenForcefield_triggered(bool checked)
{
	// Call routine in forcefields window...
	gui.forcefieldsWidget->loadForcefield();
}

// Create patterns for model
void AtenForm::on_actionModelCreatePatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->autocreatePatterns();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Remove patterns from model
void AtenForm::on_actionModelRemovePatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->clearPatterns();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// List patterns in model
void AtenForm::on_actionModelListPatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->printPatterns();
}

// Perform forcefield typing in model
void AtenForm::on_actionModelFFType_triggered(bool checked)
{
	aten.currentModelOrFrame()->typeAll();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// // Remove typing from model
void AtenForm::on_actionModelFFUntype_triggered(bool checked)
{
	aten.currentModelOrFrame()->removeTyping();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Create energy expression for model
void AtenForm::on_actionModelCreateExpression_triggered(bool checked)
{
	aten.currentModelOrFrame()->createExpression();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

