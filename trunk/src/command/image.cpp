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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/tcanvas.uih"
#include "gui/tprocess.uih"
#include "model/model.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"
#include "base/progress.h"
#include "main/aten.h"


// Pre-setup
int movieSetup(bool pre, int height)
{
	static bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView(), viewglobe = prefs.viewRotationGlobe();
	if (pre)
	{
		// Check that defined encoder exe exists
		if (!fileExists(prefs.encoderExe()))
		{
			msg.print("Error: Encoder excutable doesn't appear to exist ('%s').\n", prefs.encoderExe());
			return FALSE;
		}
		else msg.print(Messenger::Verbose, "Found encoder executable ('%s').\n", prefs.encoderExe());

		// Save some current view preferences
		framemodel = prefs.frameCurrentModel();
		frameview = prefs.frameWholeView();
		viewglobe = prefs.viewRotationGlobe();
		prefs.setFrameCurrentModel(FALSE);
		prefs.setFrameWholeView(FALSE);
		prefs.setViewRotationGlobe(FALSE);
		
		// Generate unique file basename
		int runid;
		Dnchar basename;
		do
		{
			runid = AtenMath::randomimax();
			basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, 0);
		} while (fileExists(basename));
		msg.print("First temporary basename for movie images is '%s'.\n", basename.get());

		return runid;
	}
	else
	{
		prefs.setFrameCurrentModel(framemodel);
		prefs.setFrameWholeView(frameview);
		prefs.setViewRotationGlobe(viewglobe);
	}
}

// Perform post-processing on saved movie frames
bool moviePostProcess(QStringList files, int runid, const char *movieFilename, int fps)
{
	// Create filelist on disk
	Dnchar framesFile(-1, "%s%caten-movie-%i-%i-files.txt", prefs.tempDir(), PATHSEP, gui.pid(), runid);
	LineParser parser;
	parser.openOutput(framesFile, TRUE);
	if (parser.isFileGoodForWriting())
	{
		foreach (QString str, files) parser.writeLineF("%s\n", qPrintable(str));
		parser.closeFiles();
	}
	else
	{
		msg.print("Error: Couldn't create framelist file '%s'.\n", framesFile.get());
		return FALSE;
	}

	// Now run external program to create movie
	TProcess encoderProcess;
	
	// Grab encoder command and replace control strings
	QString encoderArgs = prefs.encoderArguments();
	Dnchar quotedMovieFilename(-1, "\"%s\"", movieFilename);
	Dnchar atFramesFile(-1, "@\"%s\"", framesFile.get());
	encoderArgs.replace("OUTPUT", quotedMovieFilename.get());
	encoderArgs.replace("FILES", atFramesFile.get());
	encoderArgs.replace("FPS", itoa(fps));
	msg.print("Command to run will be '%s %s'\n", prefs.encoderExe(), qPrintable(encoderArgs));
	if (!encoderProcess.execute(prefs.encoderExe(),qPrintable(encoderArgs),NULL))
	{
		msg.print("Error: Failed to run encoder command.\n");
		return FALSE;
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
		TProcess postProcess;
		// Grab encoder command and replace
		encoderArgs = prefs.encoderPostArguments();
		encoderArgs.replace("OUTPUT", quotedMovieFilename.get());
		encoderArgs.replace("FILES", atFramesFile.get());
		encoderArgs.replace("FPS", itoa(fps));

		msg.print("Post-process command to run will be '%s %s'\n", prefs.encoderPostExe(), qPrintable(encoderArgs));
		if (!postProcess.execute(prefs.encoderPostExe(),qPrintable(encoderArgs),NULL))
		{
			msg.print("Error: Failed to run encoder post-processing command.\n");
			return FALSE;
		}
		
		// Follow output here...
		while (!postProcess.finished())
		{
			// Is output file already present?
			while (postProcess.outputAvailable()) postProcess.printLineToMessages();
			gui.processMessages();
		}
	}

	// Cleanup
	QFile::remove(framesFile.get());
	bool pid = progress.initialise("Cleaning up...", files.size(), FALSE);
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
bool Command::function_SaveBitmap(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Convert format to bitmap_format
	RenderEngine::BitmapFormat bf = RenderEngine::bitmapFormat(c->argc(0));
	if (bf == RenderEngine::nBitmapFormats)
	{
		msg.print("Unrecognised bitmap format.\n");
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
	bool viewglobe = prefs.viewRotationGlobe();
	prefs.setViewRotationGlobe(FALSE);

	rv.reset();
	bool result;
	// Has image saving been redirected? If so, use filename provided by Aten
 	if (aten.redirectedImagesActive())
	{
		const char *filename = aten.nextRedirectedFilename();
		if (isEmpty(filename))
		{
			msg.print("Maximum number of frames for image redirect reached. Raising error...\n");
			result = FALSE;
		}
		else result = engine().saveImage(filename, bf, width, height, quality);
	}
 	else result = engine().saveImage(c->argc(1), bf, width, height, quality);

	prefs.setViewRotationGlobe(viewglobe);
	return result;
}

// Save movie of current trajectory
bool Command::function_SaveMovie(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Check that a trajectory exists for the current model
	if (!obj.m->hasTrajectory())
	{
		msg.print("No trajectory associated to current model.\n");
		return FALSE;
	}

	// Get arguments...
	int width = c->hasArg(1) ? c->argi(1) : gui.mainCanvas()->width();
	if (width == -1) width = gui.mainCanvas()->width();
	int height = c->hasArg(2) ? c->argi(2) : gui.mainCanvas()->height();
	if (height == -1) height = gui.mainCanvas()->height();
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
	int progid = progress.initialise("Saving trajectory movie frames...", lastFrame-firstFrame, FALSE);
	bool canceled = FALSE;
	Dnchar basename;
	QStringList files;
	for (int n = firstFrame; n <= lastFrame; n += (1+frameSkip))
	{
		obj.m->seekTrajectoryFrame(n, TRUE);
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
// 		parent_.postRedisplay();

		pixmap = engine().renderSceneImage(RenderEngine::HighQuality, width, height);
		pixmap.save(basename.get(), "png", -1);
		files << basename.get();

		if (!progress.update(progid,n))
		{
			canceled = TRUE;
			msg.print("Canceled.\n");
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
bool Command::function_SaveVibrationMovie(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// CommandNode::run(Command::SaveVibrationMovie, "ciiiiiii", qPrintable(filename), width, height, -1, ui.VibrationsList->currentRow(), nsteps, ncycles, fps);
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Get arguments...
	int width = c->hasArg(1) ? c->argi(1) : gui.mainCanvas()->width();
	if (width == -1) width = gui.mainCanvas()->width();
	int height = c->hasArg(2) ? c->argi(2) : gui.mainCanvas()->height();
	if (height == -1) height = gui.mainCanvas()->height();
	int quality = c->hasArg(3) ? c->argi(3) : -1;
	int vibrationId = c->argi(4) - 1;
	int framesPerVibration = c->argi(5);
	int nCycles = c->argi(6);
	int fps = c->hasArg(7) ? c->argi(7) : 25;
	bool fromVib = obj.rs()->renderFromVibration();

	// Check that the specified vibration exists
	Vibration *vib = obj.rs()->vibration(vibrationId);
	if (!vib)
	{
		msg.print("Specified vibration (id %i) does not exist in current model.\n", vibrationId);
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

	int progid = progress.initialise("Saving vibration movie frames...", framesPerVibration, FALSE);
	bool canceled = FALSE;
	Dnchar basename;
	for (int n = 0; n < framesPerVibration; ++n)
	{
		obj.rs()->setVibrationFrameIndex(n);
		
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
// 		parent_.postRedisplay();

		pixmap = engine().renderSceneImage(RenderEngine::HighQuality, width, height);
		pixmap.save(basename.get(), "png", -1);
		
		if (!progress.update(progid,n))
		{
			canceled = TRUE;
			msg.print("Canceled.\n");
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
			basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
			files << basename.get();
		}
		// First half...
		for (n=framesPerVibration-1; n>0; --n)
		{
			basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
			files << basename.get();
		}
	}

	// Reset after movie frame creation
	movieSetup(FALSE, -1);

	// Perform post-processing of movie frames
	if (!moviePostProcess(files, runid, c->argc(0), fps)) return FALSE;

	if (canceled) return FALSE;
	
	return TRUE;
}
