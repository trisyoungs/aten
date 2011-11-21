/*
	*** Image Commands
	*** src/command/image.cpp
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

// Save current view as bitmap image
bool Command::function_SaveBitmap(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Convert format to bitmap_format
	GuiQt::BitmapFormat bf = GuiQt::bitmapFormat(c->argc(0));
	if (bf == GuiQt::nBitmapFormats)
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
		else result = gui.saveImage(filename, bf, width, height, quality);
	}
 	else result = gui.saveImage(c->argc(1), bf, width, height, quality);

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
	
	// Check that defined encoder exe exists
	if (!fileExists(prefs.encoderExe()))
	{
		msg.print("Error: Encoder excutable doesn't appear to exist ('%s').\n", prefs.encoderExe());
		return FALSE;
	}
	
	// Get arguments...
	int width = c->hasArg(1) ? c->argi(1) : gui.mainCanvas()->width();
	if (width == -1) width = gui.mainCanvas()->width();
	int height = c->hasArg(2) ? c->argi(2) : gui.mainCanvas()->height();
	if (height == -1) height = gui.mainCanvas()->height();
	int quality = c->hasArg(3) ? c->argi(3) : -1;
	int firstframe = c->hasArg(4) ? c->argi(4)-1 : 0;
	int lastframe = c->hasArg(5) ? c->argi(5)-1 : obj.m->nTrajectoryFrames()-1;
	int frameskip = c->hasArg(6) ? c->argi(6) : 1;
	int fps = c->hasArg(7) ? c->argi(7) : 25;

	// Set offscreen rendering and save some current view preferences
	bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView(), viewglobe = prefs.viewRotationGlobe();
	prefs.setFrameCurrentModel(FALSE);
	prefs.setFrameWholeView(FALSE);
	prefs.setViewRotationGlobe(FALSE);
	obj.m->setRenderSource(Model::TrajectorySource);
	
	// Generate unique file basename
	int runid;
	Dnchar basename;
	do
	{
		runid = AtenMath::randomimax();
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, 0);
	} while (fileExists(basename));
	
	// Save all frame images
	QPixmap pixmap;
	QImage image;
	// Temporarily adjust label size...
	int oldlabelsize = prefs.labelSize();
	int newlabelsize = int (oldlabelsize*( (1.0*height / gui.mainCanvas()->height()) ));
	prefs.setLabelSize(newlabelsize);

	int progid = progress.initialise("Saving movie frames...", lastframe-firstframe, FALSE);
	bool canceled = FALSE;
	for (int n = firstframe; n <= lastframe; n += frameskip)
	{
		obj.m->seekTrajectoryFrame(n, TRUE);
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
		gui.mainCanvas()->postRedisplay(TRUE);

		pixmap = gui.mainCanvas()->generateImage(width, height, TRUE);

		pixmap.save(basename.get(), "png", -1);
		if (!progress.update(progid,n))
		{
			canceled = TRUE;
			msg.print("Canceled.\n");
			break;
		}
	}
	progress.terminate(progid);
	prefs.setFrameCurrentModel(framemodel);
	prefs.setFrameWholeView(frameview);
	prefs.setViewRotationGlobe(viewglobe);

	// Restore label size
	prefs.setLabelSize(oldlabelsize);

	if (canceled) return FALSE;
	
	// Now run external program to create movie
	TProcess encoderProcess;
	// Grab encoder command and replace
	basename.sprintf("%s%caten-movie-%i-%i-*.png", prefs.tempDir(), PATHSEP, gui.pid(), runid);
	QString encoderArgs = prefs.encoderArguments();
	Dnchar filename(-1, "\"%s\"", c->argc(0));
	encoderArgs.replace("OUTPUT", filename.get());
	encoderArgs.replace("FILES", basename.get());
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
		QString encoderArgs = prefs.encoderPostArguments();
		Dnchar filename(-1, "\"%s\"", c->argc(0));
		encoderArgs.replace("OUTPUT", filename.get());
		encoderArgs.replace("FILES", basename.get());
		encoderArgs.replace("FPS", itoa(fps));
		msg.print("Command to run will be '%s %s'\n", prefs.encoderPostExe(), qPrintable(encoderArgs));
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
	bool pid = progress.initialise("Cleaning up...", lastframe-firstframe, FALSE);
	for (int n = firstframe; n <= lastframe; n += frameskip)
	{
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
		QFile::remove(basename.get());
		if (!progress.update(pid,n)) break;
	}
	progress.terminate(pid);
	
	return TRUE;
}

// Save movie of specified vibration frame
bool Command::function_SaveVibrationMovie(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// CommandNode::run(Command::SaveVibrationMovie, "ciiiiiii", qPrintable(filename), width, height, -1, ui.VibrationsList->currentRow(), fpv, ncycles, fps);
	return FALSE;
}
