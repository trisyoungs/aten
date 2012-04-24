/*
	*** Script Movie Dock Widget
	*** src/gui/scriptmovie_funcs.cpp
	Copyright T. Youngs 2007-2012

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
#include "gui/mainwindow.h"
#include "gui/toolbox.h"
#include "gui/gui.h"
#include "base/progress.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"
#include "gui/tprocess.uih"

// Constructor
ScriptMovieWidget::ScriptMovieWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	// Add default text to the text edit
	ui.setupUi(this);
	QString text = "for (int n=0; n<100; ++n)\n{\n\trotateview(3.6,0.0);\n\tsavebitmap(\"png\", \"<filename>\");\n}";
	ui.ScriptTextEdit->document()->setPlainText(text);
}

void ScriptMovieWidget::showWidget()
{
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.ScriptMovieButton->setChecked(TRUE);
}

void ScriptMovieWidget::on_LoadScriptButton_clicked(bool on)
{
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Load Script", currentDirectory_.path(), gui.mainWindow()->loadScriptFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);

		ui.ScriptTextEdit->clear();
		QFile scriptFile(filename);
		scriptFile.open(QIODevice::ReadOnly);
		QTextStream in(&scriptFile);
		QString line = in.readAll();
		scriptFile.close();
		ui.ScriptTextEdit->document()->setPlainText(line);
	}
}

void ScriptMovieWidget::on_SaveScriptButton_clicked(bool on)
{
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
	
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainCanvas()->width(), (int) gui.mainCanvas()->height());
	int width, height;
	
	Tree dialog;
	TreeGui &ui = dialog.defaultDialog();
	ui.setProperty(TreeGuiWidgetEvent::TextProperty, "Movie Options");
	ui.addEdit("geometry", "Image Geometry", geometry,1,1);
	ui.addIntegerSpin("maxframes", "Maximum Frames", 1, 1e6, 100, 1000 ,1,2);
	ui.addIntegerSpin("fps", "Movie FPS", 1, 200, 1, 25 ,1,2);

	if (!dialog.defaultDialog().execute()) return;

	// Retrieve widget values
	geometry = ui.asCharacter("geometry");
	width = atoi(beforeChar(geometry,'x'));
	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		Dnchar message(-1, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry.get());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	int maxframes = ui.asInteger("maxframes");
	int fps = ui.asInteger("fps");
	
	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Scripted Movie", currentDirectory.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;
	// Store path for next use
	currentDirectory.setPath(filename);
	
	// Check that defined encoder exe exists
	if (!fileExists(prefs.encoderExe()))
	{
		Dnchar message(-1, "Error: Encoder excutable doesn't appear to exist ('%s').\n", prefs.encoderExe());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	
	// Set offscreen rendering and save some current view preferences
	bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView(), viewglobe = prefs.viewRotationGlobe();
	prefs.setFrameCurrentModel(FALSE);
	prefs.setFrameWholeView(FALSE);
	prefs.setViewRotationGlobe(FALSE);
	
	// Generate unique file basename and initialise image redirection
	int runid;
	Dnchar basename;
	do
	{
		runid = AtenMath::randomimax();
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, 0);
	} while (fileExists(basename));
	basename.sprintf("%s%caten-movie-%i-%i-%%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid);
	aten.initialiseImageRedirect(basename, maxframes);
	
	// Temporarily adjust label size...
	int oldlabelsize = prefs.labelSize();
	int newlabelsize = int (oldlabelsize*( (1.0*height / gui.mainCanvas()->height()) ));
	prefs.setLabelSize(newlabelsize);

	int progid = progress.initialise("Saving scripted movie frames...", -1, FALSE);
	bool canceled = FALSE;
	ReturnValue rv;
	script.execute(rv);

	progress.terminate(progid);
	prefs.setFrameCurrentModel(framemodel);
	prefs.setFrameWholeView(frameview);
	prefs.setViewRotationGlobe(viewglobe);

	// Restore label size
	prefs.setLabelSize(oldlabelsize);

	// Now run external program to create movie
	TProcess encoderProcess;
	// Grab encoder command and replace
	basename.sprintf("%s%caten-movie-%i-%i-*.png", prefs.tempDir(), PATHSEP, gui.pid(), runid);
	QString encoderArgs = prefs.encoderArguments();
	encoderArgs.replace("OUTPUT", qPrintable(filename));
	encoderArgs.replace("FILES", basename.get());
	encoderArgs.replace("FPS", itoa(fps));
	msg.print("Command to run will be '%s %s'\n", prefs.encoderExe(), qPrintable(encoderArgs));
	if (!encoderProcess.execute(prefs.encoderExe(),qPrintable(encoderArgs),NULL))
	{
		msg.print("Error: Failed to run encoder command.\n");
		return;
	}

	// Follow output here...
	while (!encoderProcess.finished())
	{
		// Is output file already present?
		while (encoderProcess.outputAvailable()) encoderProcess.printLineToMessages();
		gui.processMessages();
	}

	// Run secondary, post-process command (if one was given)
	if (prefs.encoderPostExe() != NULL)
	{
// 		printf("Post encoder command given is [%s]\n", prefs.encoderPostExe());
		TProcess postProcess;
		// Grab encoder command and replace
		QString encoderArgs = prefs.encoderPostArguments();
		encoderArgs.replace("OUTPUT", qPrintable(filename));
		encoderArgs.replace("FILES", basename.get());
		encoderArgs.replace("FPS", itoa(fps));
		msg.print("Command to run will be '%s %s'\n", prefs.encoderPostExe(), qPrintable(encoderArgs));
		if (!postProcess.execute(prefs.encoderPostExe(),qPrintable(encoderArgs),NULL))
		{
			msg.print("Error: Failed to run encoder post-processing command.\n");
		}
		else while (!postProcess.finished())
		{
			// Is output file already present?
			while (postProcess.outputAvailable()) postProcess.printLineToMessages();
			gui.processMessages();
		}
	}

	// Cancel image redirection and perform cleanup
	int nframes = aten.cancelImageRedirect();
	bool pid = progress.initialise("Cleaning up...", nframes, FALSE);
	for (int n = 0; n < nframes; ++n)
	{
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
		QFile::remove(basename.get());
		if (!progress.update(pid,n)) break;
	}
	progress.terminate(pid);
}

void ScriptMovieWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.ScriptMovieButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainCanvas()->postRedisplay();

	event->accept();
}
