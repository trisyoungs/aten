/*
	*** Script Movie Dock Widget
	*** src/gui/scriptmovie_funcs.cpp
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
#include "gui/scriptmovie.h"
//#include "gui/mainwindow.h"
#include "gui/toolbox.h"
#include "gui/gui.h"
//#include "model/model.h"
//#include "parser/commandnode.h"
//#include "base/sysfunc.h"

// Constructor
ScriptMovieWidget::ScriptMovieWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

void ScriptMovieWidget::showWidget()
{
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.ScriptMovieButton->setChecked(TRUE);
}

void ScriptMovieWidget::on_SaveScriptedMovieButton_clicked(bool on)
{
	// First, attempt to generate script from supplied code
	Program script;
	if (!script.generateFromString(qPrintable(ui.ScriptTextEdit->toPlainText()), "ScriptedMovie"))
	{
		QMessageBox::warning(NULL, "Aten", "Couldn't compile script for movie generation.\nCheck message box for errors.", QMessageBox::Ok, QMessageBox::Ok);
		return;
	}
	
}

void ScriptMovieWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.ScriptMovieButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();

	event->accept();
}
