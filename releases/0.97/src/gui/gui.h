/*
	*** Qt user interface
	*** src/gui/gui.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_GUIQT_H
#define ATEN_GUIQT_H

#include "gui/gui.h"
#include "gui/canvas.h"

// Forward declarations
class AtenForm;
class AtenPrefs;
class AtenEdit;
class AtenLoadModel;
class AtenSelectPattern;
class Atom;
class Model;
class Forcefield;
class Grid;
class QApplication;
class QProgressDialog;

// QT4 GUI
class GuiQt
{
	/*
	// Existence of GUI
	*/
	private:
	// Is a GUI available?
	bool isAvailable_;
	// Does the GUI exist (has it been created)
	bool doesExist_;

	/*
	// Basic Window Functions
	*/
	public:
	// Constructor
	GuiQt();
	// Returns if the GUI has been created
	bool exists();
	// Initialise the QApplication (and some other small things)
	void initialise(int, char**);
	// Create the GUI and hand over control to Qt
	void run();

	/*
	// General Window Functions
	*/
	public:
	// Add a message to the main window's message output box
	void printMessage(const char*);
	// Save before close
	bool saveBeforeClose();

	/*
	// Gui Refresh Functions
	*/
	public:
	// Update model tabs and lists
	void updateModelLists();
	// Update forcefield list
	void updateForcefieldList();
	// Update grid list
	void updateGridList();
	// Update trajectory control widgets
	void updateTrajControls();
	// Refreshes all widgets to do with the model
	void modelChanged(bool updateAtoms = TRUE, bool updateCell = TRUE, bool updateForcefield = TRUE);

	/*
	// Model management
	*/
	public:
	// Add model (adds new model to list)
	void addModel(Model*);
	// Remove model from list
	void removeModel(int id);

	/*
	// Files
	*/
	public:
	// Initialise GUI file filters array
	void initFilters();

	/*
	// Misc
	*/
	public:
	// Convert Qt key code value to internal key
	Canvas::KeyCode convertToKeyCode(int);

	/*
	// Windows / Dialogs
	*/
	public:
	// Main Qt widget for the interface
	AtenForm *mainWindow;
	// Main application structure
	QApplication *app;
	// Preferences Dialog
	AtenPrefs *prefsDialog;
	// Forcefield Editor
	AtenEdit *editDialog;
	// Load model dialog
	AtenLoadModel *loadModelDialog;
	// Select pattern dialog
	AtenSelectPattern *selectPatternDialog;
	// Call the atompopup menu
	void callAtomPopup(Atom*, int, int);
	// Main view Widget
	TCanvas *mainWidget;
	// Main view canvas
	Canvas mainView;

	/*
	// Trajectory State
	*/
	private:
	// Whether the trajectory is currently playing
	bool trajectoryPlaying_;
	// ID of rtrajectory timer
	int trajectoryTimerId_;

	public:
	// Return state of trajectory playback
	bool trajectoryPlaying();
	// Set state of trajectory playback
	void setTrajectoryPlaying(bool b);
	// Return trajectory timer id
	int trajectoryTimerId();
	// Set state of trajectory playback
	void setTrajectoryTimerId(int i);
	// Stop trajectory playback
	void stopTrajectoryPlayback();

	/*
	// Progress Dialog
	*/
	private:
	// Indicator that the 'Cancel' button was pressed
	bool progressCanceled_;
	// Variables for the position and maximum of the text progress dialog
	int textProgressStepsToDo_, textProgressPercent_;

	public:
	// Notify that the progress indicator should be canceled
	void notifyProgressCanceled();
	// Instantiate a progress dialog
	void progressCreate(const char *jobtitle, int stepstodo);
	// Update the progress dialog
	bool progressUpdate(int currentstep);
	// Terminate the progress dialog
	void progressTerminate();
	// Instantiate a text-based progress dialog
	void textProgressCreate(const char *jobtitle, int stepstodo);
	// Update the text progress dialog
	void textProgressUpdate(int currentstep);
	// Terminate the progress dialog
	void textProgressTerminate();

	/*
	// Basic Offscreen Canvas
	*/
	public:
	// Offscreen canvas (for use by, e.g., g2ps routines)
	Canvas offscreenCanvas;
};

extern GuiQt gui;

#endif
