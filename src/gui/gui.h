/*
	*** Qt user interface
	*** src/gui/gui.h
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

#ifndef ATEN_GUIQT_H
#define ATEN_GUIQT_H

#include "gui/gui.h"
#include "render/canvas.h"

// Forward declarations
class AtenForm;
class AtenAbout;
class AtenPrefs;
class AtenForcefieldEditor;
class AtenLoadModel;
class AtenSelectFilter;
class AtenSelectPattern;
class AtenSelectElement;
class AtenCommandHelp;
class AtenAtomlist;
class AtenBuild;
class AtenCellDefine;
class AtenCellTransform;
class AtenCommand;
class AtenDisorder;
class AtenForcefields;
class AtenFragment;
class AtenGeometry;
class AtenGlyphs;
class AtenGrids;
class AtenMinimiser;
class AtenPosition;
class AtenTransform;
class Atom;
class Model;
class Forcefield;
class Grid;
class QApplication;
class QProgressDialog;

// QT4 GUI
class GuiQt
{
	public:
	// Constructor / Destructor
	GuiQt();
	~GuiQt();
	// Bitmap Formats
	enum BitmapFormat { BitmapBMP, BitmapPG, BitmapPNG, BitmapPPM, BitmapXBM, BitmapX11, nBitmapFormats };
	static BitmapFormat bitmapFormat(const char *name, bool reporterror = 0);
	static BitmapFormat bitmapFormatFromFilter(const char *s);
	static const char *bitmapFormatFilter(BitmapFormat bf);
	static const char *bitmapFormatExtension(BitmapFormat bf);

	/*
	// Refresh Functions
	*/
	public:
	// Refreshes all widgets to do with the model
	void update(bool updateAtoms = TRUE, bool updateCell = TRUE, bool updateForcefield = TRUE);
	// Update statusbar
	void updateStatusBar(bool clear = FALSE);


	/*
	// General Functions
	*/
	private:
	// Is a GUI available?
	bool isAvailable_;
	// Does the GUI exist (has it been created)
	bool doesExist_;

	public:
	// Returns if the GUI has been created
	bool exists();
	// Initialise the QApplication (and some other small things)
	void initialise(int &argc, char **argv);
	// Create the GUI and hand over control to Qt
	void run();


	/*
	// Methods
	*/
	public:
	// Add model (adds new model to list)
	void addModel(Model*);
	// Convert Qt key code value to internal key
	Canvas::KeyCode convertToKeyCode(int);
	// Initialise GUI file filters array
	void initFilters();
	// Add a message to the main window's message output box
	void printMessage(const char*);
	// Remove model from list
	void removeModel(int id);
	// Save before close
	bool saveBeforeClose();
	// Save image of current view
	bool saveImage(const char *filename, BitmapFormat bf, int width, int height, int quality = 85);
	// Enable / disable GUI (except progress bar group)
	void setWindowsEnabled(bool b);

	/*
	// Windows / Subwindows
	*/
	public:
	// Main application structure
	QApplication *app;
	// Main Window
	AtenForm *mainWindow;
	// Atom list subwindow
	AtenAtomlist *atomlistWindow;
	// Build subwindow
	AtenBuild *buildWindow;
	// Cell definition subwindow
	AtenCellDefine *cellDefineWindow;
	// Cell transform subwindow
	AtenCellTransform *cellTransformWindow;
	// Command subwindow
	AtenCommand *commandWindow;
	// Disordered builder window
	AtenDisorder *disorderWindow;
	// Forcefields window
	AtenForcefields *forcefieldsWindow;
	// Fragment Library window
	AtenFragment *fragmentWindow;
	// Geometry window
	AtenGeometry *geometryWindow;
	// Glyphs window
	AtenGlyphs *glyphsWindow;
	// Grids window
	AtenGrids *gridsWindow;
	// Minimiser window
	AtenMinimiser *minimiserWindow;
	// Atom positioning window
	AtenPosition *positionWindow;
	// Atom transformation window
	AtenTransform *transformWindow;

	/*
	// Dialogs
	*/
	public:
	// About Dialog
	AtenAbout *aboutDialog;
	// Preferences Dialog
	AtenPrefs *prefsDialog;
	// Forcefield Editor
	AtenForcefieldEditor *forcefieldEditorDialog;
	// Load model dialog
	AtenLoadModel *loadModelDialog;
	// Select filter dialog
	AtenSelectFilter *selectFilterDialog;
	// Select pattern dialog
	AtenSelectPattern *selectPatternDialog;
	// Select element dialog
	AtenSelectElement *selectElementDialog;
	// Command help dialog
	AtenCommandHelp *commandHelpDialog;

	/*
	// Canvas
	*/
	public:
	// Main view Widget
	TCanvas *mainWidget;
	// Main view canvas
	Canvas mainView;
	// Update context menu
	void updateContextMenu();
	// Call the atompopup menu
	void callContextMenu(Atom*, int, int);

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
	bool isTrajectoryPlaying();
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
	// QTime object (used to prevent showing of progress indicator if the operation will be quick)
	QTime time_;
	// Indicator that the 'Cancel' button was pressed
	bool progressCanceled_;
	// Variables for the position and maximum of the text progress dialog
	int progressStepsToDo_, progressPercent_, progressCurrentStep_;

	public:
	// Notify that the progress indicator should be canceled
	void notifyProgressCanceled();
	// Instantiate a progress dialog
	void progressCreate(const char *jobtitle, int stepstodo);
	// Update the progress dialog
	bool progressUpdate(int currentstep = -1, Dnchar *shorttext = NULL);
	// Terminate the progress dialog
	void progressTerminate();
};

extern GuiQt gui;

#endif
