/*
	*** Main User Interface
	*** src/gui/gui.h
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

#ifndef ATEN_GUIQT_H
#define ATEN_GUIQT_H

#include "gui/tcanvas.uih"

// Forward Declarations 1 - Main Form and Windows
class AtenForm;
class AtenAbout;
class AtenPrefs;
class AtenForcefieldEditor;
class QApplication;
class QProgressDialog;

// Forward Declarations 2 - Dialogs
class AtenLoadModel;
class AtenSelectFilter;
class AtenSelectPattern;
class AtenSelectElement;
class AtenSelectVariable;
class AtenDisorder;
class AtenViewBasis;
class AtenViewEigenvector;
class AtenZMatrix;

// Forward Declarations 3 - Dock Widgets
class AtomListWidget;
class BuildWidget;
class CellDefinitionWidget;
class CellTransformWidget;
class CommandWidget;
class ForcefieldsWidget;
class FragmentsWidget;
class GeometryWidget;
class GlyphsWidget;
class GridsWidget;
class MDWidget;
class ModelListWidget;
class PositionWidget;
class SelectWidget;
class ToolBoxWidget;
class TransformWidget;
class VibrationsWidget;

// Forward Declarations 4 - Other Classes
class Atom;
class Model;
class Forcefield;
class Grid;

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
	// Update Targets
	enum UpdateTarget { AtomsTarget = 1, CellTarget = 2, ForcefieldsTarget = 4, GlyphsTarget = 8, GridsTarget = 16, AllTarget = 31 };


	/*
	// Refresh Functions
	*/
	public:
	// Refreshes all widgets to do with the model
	void update(int targets = 0);
	// Update statusbar
	void updateStatusBar(bool clear = FALSE);


	/*
	// General Functions
	*/
	private:
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
	// Initialise GUI file filters array
	void initFilters();
	// Add a message to the main window's message output box
	void printMessage(const char*);
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
	// Disordered builder window
	AtenDisorder *disorderWindow;
	// Zmatrix window
	AtenZMatrix *zmatrixWindow;

	
	/*
	// Dock Widgets
	*/
	private:
	// List of dock widgets
	QList<QObject*> dockWidgets_;
	
	public:
	// Atom list dock widget
	AtomListWidget *atomListWidget;
	// Build dock widget
	BuildWidget *buildWidget;
	// Cell definition dock widget
	CellDefinitionWidget *cellDefinitionWidget;
	// Cell transform dock widget
	CellTransformWidget *cellTransformWidget;
	// Command dock widget
	CommandWidget *commandWidget;
	// Forcefields dock widget
	ForcefieldsWidget *forcefieldsWidget;
	// Fragment Library dock widget
	FragmentsWidget *fragmentsWidget;
	// Geometry dock widget
	GeometryWidget *geometryWidget;
	// Glyphs dock widget
	GlyphsWidget *glyphsWidget;
	// Grids dock widget
	GridsWidget *gridsWidget;
	// MD dock widget
	MDWidget *mdWidget;
	// Model List dock widget
	ModelListWidget *modelListWidget;
	// Atom positioning dock widget
	PositionWidget *positionWidget;
	// Atom selection dock widget
	SelectWidget *selectWidget;
	// Toolbox dock widget
	ToolBoxWidget *toolBoxWidget;
	// Atom transformation dock widget
	TransformWidget *transformWidget;
	// Vibrations dock widget
	VibrationsWidget *vibrationsWidget;
	

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
	// Select variable dialog
	AtenSelectVariable *selectVariableDialog;
	// Select element dialog
	AtenSelectElement *selectElementDialog;
	// View basis dialog
	AtenViewBasis *viewBasisDialog;
	// View eigenvector dialog
	AtenViewEigenvector *viewEigenvectorDialog;


	/*
	// Canvas
	*/
	public:
	// Main view Widget
	TCanvas *mainWidget;
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
