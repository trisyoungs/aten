/*
	*** Script Movie Dock Widget
	*** src/gui/scriptmovie_funcs.cpp
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
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
#include "gui/tprocess.uih"
#include "main/aten.h"
#include "gui/scriptmovie.h"
#include "gui/mainwindow.h"
#include "base/progress.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Constructor
ScriptMovieWidget::ScriptMovieWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	// Add default text to the text edit
	ui.setupUi(this);
	QString text = "for (int n=0; n<100; ++n)\n{\n\trotateview(3.6,0.0);\n\tsavebitmap(\"png\", \"<filename>\");\n}";
	ui.ScriptTextEdit->document()->setPlainText(text);
}

void ScriptMovieWidget::showWidget()
{
	show();
}

void ScriptMovieWidget::on_LoadScriptButton_clicked(bool on)
{
	static QDir currentDirectory_(parent_.aten().workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Load Script", currentDirectory_.path(), "All files (*)", &selFilter);
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
	// TODO
}

void ScriptMovieWidget::on_SaveScriptedMovieButton_clicked(bool on)
{
	// First, attempt to generate script from supplied code
	Program script;
	if (!script.generateFromString(qPrintable(ui.ScriptTextEdit->toPlainText()), "ScriptedMovie", "Scripted Movie Command"))
	{
		QMessageBox::warning(NULL, "Aten", "Couldn't compile script for movie generation.\nCheck message box for errors.", QMessageBox::Ok, QMessageBox::Ok);
		return;
	}
	
	QString geometry;
	geometry.sprintf("%ix%i", (int) parent_.ui.MainView->width(), (int) parent_.ui.MainView->height());
	int width, height;
	
	Tree dialog;
	TreeGui &ui = dialog.defaultDialog();
	ui.setProperty(TreeGuiWidgetEvent::TextProperty, "Movie Options");
	ui.addEdit("geometry", "Image Geometry", geometry,1,1);
	ui.addIntegerSpin("maxframes", "Maximum Frames", 1, 1e6, 100, 1000 ,1,2);
	ui.addIntegerSpin("fps", "Movie FPS", 1, 200, 1, 25 ,1,2);

	if (!dialog.defaultDialog().execute()) return;

	// Retrieve widget values
	geometry = ui.asString("geometry");
// 	width = atoi(beforeChar(geometry,'x'));	// ATEN2 TODO
// 	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		QMessageBox::warning(this, "Aten", "The geometry '" + geometry + "' is not valid since one (or both) components are less than 1.", QMessageBox::Ok);
		return;
	}
	int maxframes = ui.asInteger("maxframes");
	int fps = ui.asInteger("fps");
	
	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory(parent_.aten().workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Scripted Movie", currentDirectory.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;
	// Store path for next use
	currentDirectory.setPath(filename);
	
	// Check that defined encoder exe exists
	QFileInfo fileInfo(prefs.encoderExe());
	if (!fileInfo.exists())
	{
		QString message = "Error: Encoder executable doesn't appear to exist ('" + prefs.encoderExe() + "').";
		QMessageBox::warning(this, "Aten", message, QMessageBox::Ok);
		return;
	}
	
	// Set offscreen rendering and save some current view preferences
	bool viewglobe = prefs.viewRotationGlobe();
	prefs.setViewRotationGlobe(false);
	
	// Generate unique file basename and initialise image redirection
	int runid;
	QString basename;
	do
	{
		runid = AtenMath::randomimax();
		basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.arc").arg(QApplication::applicationPid(), runid).arg(0, 9, 10, QChar('0'));
		fileInfo.setFile(basename);
	} while (fileInfo.exists());
// 	basename.sprintf("%s%caten-movie-%i-%i-%%09i.png", qPrintable(prefs.tempDir()), PATHSEP, parent_.pid(), runid); ATEN2 TODO
	parent_.aten().initialiseImageRedirect(basename, maxframes);
	
	int progid = progress.initialise("Saving scripted movie frames...", -1);
	bool canceled = false;
	ReturnValue rv;
	script.execute(rv);

	progress.terminate(progid);
	prefs.setViewRotationGlobe(viewglobe);

	// Now run external program to create movie
	TProcess encoderProcess;

	// Grab encoder command and replace
	basename = prefs.tempDir().filePath("aten-movie-%1-%2-*.png").arg(QApplication::applicationPid(), runid);
	QString encoderArgs = prefs.encoderArguments();
	encoderArgs.replace("OUTPUT", qPrintable(filename));
	encoderArgs.replace("FILES", basename);
	encoderArgs.replace("FPS", QString::number(fps));
	Messenger::print("Command to run will be '%s %s'", qPrintable(prefs.encoderExe()), qPrintable(encoderArgs));
	if (!encoderProcess.execute(prefs.encoderExe(), qPrintable(encoderArgs), NULL))
	{
		Messenger::print("Error: Failed to run encoder command.");
		return;
	}

	// Follow output here...
	while (!encoderProcess.finished())
	{
		// Is output file already present?
		while (encoderProcess.outputAvailable()) encoderProcess.printLineToMessages();
		QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
	}

	// Run secondary, post-process command (if one was given)
	if (prefs.encoderPostExe() != NULL)
	{
// 		printf("Post encoder command given is [%s]\n", prefs.encoderPostExe());
		TProcess postProcess;
		// Grab encoder command and replace
		QString encoderArgs = prefs.encoderPostArguments();
		encoderArgs.replace("OUTPUT", qPrintable(filename));
		encoderArgs.replace("FILES", basename);
		encoderArgs.replace("FPS", QString::number(fps));
		Messenger::print("Command to run will be '%s %s'", qPrintable(prefs.encoderPostExe()), qPrintable(encoderArgs));
		if (!postProcess.execute(prefs.encoderPostExe(), encoderArgs, NULL))
		{
			Messenger::print("Error: Failed to run encoder post-processing command.");
		}
		else while (!postProcess.finished())
		{
			// Is output file already present?
			while (postProcess.outputAvailable()) postProcess.printLineToMessages();
			QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
		}
	}

	// Cancel image redirection and perform cleanup
	int nframes = parent_.aten().cancelImageRedirect();
	bool pid = progress.initialise("Cleaning up...", nframes);
	for (int n = 0; n < nframes; ++n)
	{
		basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.arc").arg(QApplication::applicationPid(), runid).arg(n, 9, 10, QChar('0'));
		QFile::remove(basename);
		if (!progress.update(pid,n)) break;
	}
	progress.terminate(pid);
}

void ScriptMovieWidget::closeEvent(QCloseEvent* event)
{
	event->accept();
}
