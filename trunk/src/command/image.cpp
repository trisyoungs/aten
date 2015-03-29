/*
	*** Image Commands
	*** src/command/image.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "gui/mainwindow.h"
#include "gui/tprocess.uih"
#include "model/model.h"
#include "base/prefs.h"
#include "base/sysfunc.h"
#include <QtGui>
#include <QFileInfo>
#include "base/progress.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

// Pre-setup
int movieSetup(bool pre, int height)
{
	static bool viewglobe;
	if (pre)
	{
		// Check that defined encoder exe exists
		QFileInfo fileInfo(prefs.encoderExe());
		if (!fileInfo.exists())
		{
			Messenger::print("Error: Encoder excutable doesn't appear to exist ('%s').", qPrintable(prefs.encoderExe()));
			return FALSE;
		}
		else Messenger::print(Messenger::Verbose, "Found encoder executable ('%s').", qPrintable(prefs.encoderExe()));

		// Save some current view preferences
		viewglobe = prefs.viewRotationGlobe();
		prefs.setViewRotationGlobe(FALSE);
		
		// Generate unique file basename
		int runid;
		QString basename;
		do
		{
			runid = AtenMath::randomimax();
			basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.png").arg(QApplication::applicationPid(), runid).arg(0, 9, 10, QChar('0'));
			fileInfo.setFile(basename);
		} while (fileInfo.exists());
		Messenger::print("First temporary basename for movie images is '%s'.", qPrintable(basename));

		return runid;
	}
	else
	{
		prefs.setViewRotationGlobe(viewglobe);
	}
}

// Perform post-processing on saved movie frames
bool moviePostProcess(QStringList files, int runid, QString movieFilename, int fps)
{
	// Create filelist on disk
	QString framesFile = prefs.tempDir().filePath("aten-movie-%1-%2-files.txt").arg(QApplication::applicationPid(), runid);
	LineParser parser;
	parser.openOutput(framesFile, TRUE);
	if (parser.isFileGoodForWriting())
	{
		foreach (QString str, files) parser.writeLineF("%s", qPrintable(str));
		parser.closeFiles();
	}
	else
	{
		Messenger::print("Error: Couldn't create framelist file '%s'.", qPrintable(framesFile));
		return FALSE;
	}

	// Now run external program to create movie
	TProcess encoderProcess;
	
	// Grab encoder command and replace control strings
	QString encoderArgs = prefs.encoderArguments();
	QString quotedMovieFilename = '"' + movieFilename + '"';
	QString atFramesFile = "@\"" + framesFile + "\"";
	encoderArgs.replace("OUTPUT", quotedMovieFilename);
	encoderArgs.replace("FILES", atFramesFile);
	encoderArgs.replace("FPS", QString::number(fps));
	Messenger::print("Command to run will be '%s %s'", qPrintable(prefs.encoderExe()), qPrintable(encoderArgs));
	if (!encoderProcess.execute(prefs.encoderExe(),qPrintable(encoderArgs),NULL))
	{
		Messenger::print("Error: Failed to run encoder command.");
		return FALSE;
	}

	// Follow output here...
	while (!encoderProcess.finished())
	{
		// Is output file already present?
		while (encoderProcess.outputAvailable()) encoderProcess.printLineToMessages();
		QApplication::processEvents();
	}

	// Run secondary, post-process command (if one was given)
	if (prefs.encoderPostExe() != NULL)
	{
		TProcess postProcess;
		// Grab encoder command and replace
		encoderArgs = prefs.encoderPostArguments();
		encoderArgs.replace("OUTPUT", quotedMovieFilename);
		encoderArgs.replace("FILES", atFramesFile);
		encoderArgs.replace("FPS", QString::number(fps));

		Messenger::print("Post-process command to run will be '%s %s'", qPrintable(prefs.encoderPostExe()), qPrintable(encoderArgs));
		if (!postProcess.execute(prefs.encoderPostExe(),qPrintable(encoderArgs),NULL))
		{
			Messenger::print("Error: Failed to run encoder post-processing command.");
			return FALSE;
		}
		
		// Follow output here...
		while (!postProcess.finished())
		{
			// Is output file already present?
			while (postProcess.outputAvailable()) postProcess.printLineToMessages();
			QApplication::processEvents();
		}
	}

	// Cleanup
	QFile::remove(framesFile);
	bool pid = progress.initialise("Cleaning up...", files.size());
	int n = 0;
	foreach (QString str, files)
	{
		QFile::remove(str);
		if (!progress.update(pid,++n)) break;
	}
	progress.terminate(pid);	

	return TRUE;
}

// Save current view as bitmap image
bool Commands::function_SaveBitmap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Convert format to bitmap_format
	Aten::BitmapFormat bf = Aten::bitmapFormat(c->argc(0));
	if (bf == Aten::nBitmapFormats)
	{
		Messenger::print("Unrecognised bitmap format.");
		return FALSE;
	}

	int width = 0, height = 0, quality = -1;
	if (c->hasArg(3))
	{
		width = c->argi(2);
		height = c->argi(3);
	}
	if (c->hasArg(4)) quality = c->argi(4);

	// Set some options, remembering current values
	bool viewGlobe = prefs.viewRotationGlobe();
	prefs.setViewRotationGlobe(FALSE);

	rv.reset();
	bool result;

	// Has image saving been redirected? If so, use filename provided by Aten
 	if (aten_.redirectedImagesActive())
	{
		QString fileName = aten_.nextRedirectedFilename();
		if (fileName.isEmpty())
		{
			Messenger::print("Maximum number of frames for image redirect reached. Raising error...");
			result = FALSE;
		}
		else
		{
			QPixmap pixmap = aten_.currentViewAsPixmap(width, height);
			result = pixmap.save(fileName, Aten::bitmapFormatExtension(bf), quality);
		}
	}
 	else
	{
		QPixmap pixmap = aten_.currentViewAsPixmap(width, height);
		result = pixmap.save(c->argc(1), Aten::bitmapFormatExtension(bf), quality);
	}

	prefs.setViewRotationGlobe(viewGlobe);
	return result;
}

// Save movie of current trajectory
bool Commands::function_SaveMovie(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Check that a trajectory exists for the current model
	if (!obj.m->hasTrajectory())
	{
		Messenger::print("No trajectory associated to current model.");
		return FALSE;
	}

	// Get arguments...
	int width = c->hasArg(1) ? c->argi(1) : aten_.atenWindow()->ui.MainView->width();
	if (width == -1) width = aten_.atenWindow()->ui.MainView->width();
	int height = c->hasArg(2) ? c->argi(2) : aten_.atenWindow()->ui.MainView->height();
	if (height == -1) height = aten_.atenWindow()->ui.MainView->height();
	int quality = c->hasArg(3) ? c->argi(3) : -1;
	int firstFrame = c->hasArg(4) ? c->argi(4)-1 : 1;
	int lastFrame = c->hasArg(5) ? c->argi(5)-1 : obj.m->nTrajectoryFrames()-1;
	int frameSkip = c->hasArg(6) ? c->argi(6) : 0;
	int fps = c->hasArg(7) ? c->argi(7) : 25;

	// Check initial movie 'setup'
	int runid = movieSetup(TRUE, height);
	if (runid == -1) return FALSE;

	// Save all trajectory frame images
	QPixmap pixmap;
	QImage image;
	obj.m->setRenderSource(Model::TrajectorySource);
	int progid = progress.initialise("Saving trajectory movie frames...", lastFrame-firstFrame);
	bool canceled = FALSE;
	QString basename;
	QStringList files;
	for (int n = firstFrame; n <= lastFrame; n += (1+frameSkip))
	{
		obj.m->seekTrajectoryFrame(n, TRUE);
		basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.png").arg(QApplication::applicationPid(), runid).arg(n, 9, 10, QChar('0'));

		pixmap = aten_.currentViewAsPixmap(width, height);
		pixmap.save(basename, "png", -1);
		files << basename;

		if (!progress.update(progid,n))
		{
			canceled = TRUE;
			Messenger::print("Canceled.");
			break;
		}
	}
	progress.terminate(progid);

	// Reset after movie frame creation
	movieSetup(FALSE, -1);

	// Perform post-processing of movie frames
	if (!moviePostProcess(files, runid, c->argc(0), fps)) return FALSE;

	if (canceled) return FALSE;
	
	return TRUE;
}

// Save movie of specified vibration frame
bool Commands::function_SaveVibrationMovie(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// CommandNode::run(Commands::SaveVibrationMovie, "ciiiiiii", qPrintable(filename), width, height, -1, ui.VibrationsList->currentRow(), nsteps, ncycles, fps);
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Get arguments...
	int width = c->hasArg(1) ? c->argi(1) : aten_.atenWindow()->ui.MainView->width();
	if (width == -1) width = aten_.atenWindow()->ui.MainView->width();
	int height = c->hasArg(2) ? c->argi(2) : aten_.atenWindow()->ui.MainView->height();
	if (height == -1) height = aten_.atenWindow()->ui.MainView->height();
	int quality = c->hasArg(3) ? c->argi(3) : -1;
	int vibrationId = c->argi(4) - 1;
	int framesPerVibration = c->argi(5);
	int nCycles = c->argi(6);
	int fps = c->hasArg(7) ? c->argi(7) : 25;
	bool fromVib = obj.rs()->renderFromVibration();

	// Check that the specified vibration exists
	Vibration* vib = obj.rs()->vibration(vibrationId);
	if (!vib)
	{
		Messenger::print("Specified vibration (id %i) does not exist in current model.", vibrationId);
		return FALSE;
	}
	
	// Check initial movie 'setup'
	int runid = movieSetup(TRUE, height);
	if (runid == -1) return FALSE;

	// Generate vibration frams info
	obj.rs()->generateVibration(vibrationId, framesPerVibration);
	obj.rs()->setVibrationFrameIndex(0);
	obj.rs()->setRenderFromVibration(TRUE);

	QPixmap pixmap;
	QImage image;

	int progid = progress.initialise("Saving vibration movie frames...", framesPerVibration);
	bool canceled = FALSE;
	QString basename;
	for (int n = 0; n < framesPerVibration; ++n)
	{
		obj.rs()->setVibrationFrameIndex(n);
		
		basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.png").arg(QApplication::applicationPid(), runid).arg(n, 9, 10, QChar('0'));
// 		parent_.postRedisplay();

		pixmap = aten_.currentViewAsPixmap(width, height);
		pixmap.save(basename, "png", -1);
		
		if (!progress.update(progid,n))
		{
			canceled = TRUE;
			Messenger::print("Canceled.");
			break;
		}
	}
	progress.terminate(progid);
	obj.rs()->setRenderFromVibration(fromVib);

	// Construct list of ping-pong'ed frame images
	QStringList files;
	int n;
	for (int cycle = 0; cycle < nCycles; ++cycle)
	{
		// First half...
		for (n=0; n<framesPerVibration; ++n)
		{
			basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.png").arg(QApplication::applicationPid(), runid).arg(n, 9, 10, QChar('0'));
			files << basename;
		}
		// Second half...
		for (n=framesPerVibration-1; n>0; --n)
		{
			basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.png").arg(QApplication::applicationPid(), runid).arg(n, 9, 10, QChar('0'));
			files << basename;
		}
	}

	// Reset after movie frame creation
	movieSetup(FALSE, -1);

	// Perform post-processing of movie frames
	if (!moviePostProcess(files, runid, c->argc(0), fps)) return FALSE;

	if (canceled) return FALSE;
	
	return TRUE;
}
