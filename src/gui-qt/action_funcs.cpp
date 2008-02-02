/*
	*** Qt mainwindow action functions
	*** src/gui-qt/action_funcs.cpp
	Copyright T. Youngs 2007

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

#include "base/master.h"
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"
#include "gui-qt/prefs.h"

/*
// Draw style Actions
*/

void AtenForm::on_StyleToolBar_actionTriggered(QAction *action)
{
	// If the source action is not checked, ignore the signal
	if (!action->isChecked()) return;
	model *m, *trajframe;
	draw_style ds = DS_STICK;
	if (action == ui.actionStyleStick) ds = DS_STICK;
	else if (action == ui.actionStyleTube) ds = DS_TUBE;
	else if (action == ui.actionStyleSphere) ds = DS_SPHERE;
	else if (action == ui.actionStyleScaled) ds = DS_SCALED;
	else if (action == ui.actionStyleIndividual) ds = DS_INDIVIDUAL;
	prefs.set_render_style(ds);
	// Inform the displayed model
	m = master.get_currentmodel();
	m->project_all();
	m->log_change(LOG_VISUAL);
	trajframe = m->get_currentframe();
	if (trajframe != NULL)
	{
		trajframe->project_all();
		trajframe->log_change(LOG_VISUAL);
	}
	gui.mainview.postredisplay();
}

/*
// Settings Actions
*/

void AtenForm::on_actionPreferences_triggered(bool checked)
{
	gui.prefsdialog->show();
}
