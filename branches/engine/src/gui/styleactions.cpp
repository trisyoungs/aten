/*
	*** Qt draw style action functions
	*** src/gui/styleactions.cpp
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
#include "gui/gui.h"
#include "model/model.h"

/*
// Draw style Actions
*/

void AtenForm::on_StyleToolbar_actionTriggered(QAction *action)
{
	// If the source action is not checked, ignore the signal
	if (!action->isChecked()) return;
	Model *m;
	Atom::DrawStyle ds = Atom::StickStyle;
	if (action == ui.actionStyleStick) ds = Atom::StickStyle;
	else if (action == ui.actionStyleTube) ds = Atom::TubeStyle;
	else if (action == ui.actionStyleSphere) ds = Atom::SphereStyle;
	else if (action == ui.actionStyleScaled) ds = Atom::ScaledStyle;
	else if (action == ui.actionStyleIndividual) ds = Atom::IndividualStyle;
	prefs.setRenderStyle(ds);
	// Inform the displayed model
	m = aten.currentModelOrFrame();
// 	m->projectAll();
// 	m->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}

