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
class AtenProgress;
class QApplication;

// Forward Declarations 2 - Dialogs
class AtenLoadModel;
class AtenSelectFilter;
class AtenSelectPattern;
class AtenSelectElement;
class AtenSelectVariable;
class AtenViewBasis;
class AtenViewEigenvector;
class AtenZMatrix;

// Forward Declarations 3 - Dock Widgets and Wizards
class AtomListWidget;
class BuildWidget;
class CellDefinitionWidget;
class CellTransformWidget;
class CommandWidget;
class DisorderWizard;
class ForcefieldsWidget;
class FragmentsWidget;
class GeometryWidget;
class GlyphsWidget;
class GridsWidget;
class MDWidget;
class MessagesWidget;
class ModelListWidget;
class PositionWidget;
class ScriptMovieWidget;
class SelectWidget;
class ToolBoxWidget;
class TrajectoryWidget;
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
	// Constructor
	GuiQt();
	// Bitmap Formats
	enum BitmapFormat { BitmapBMP, BitmapPG, BitmapPNG, BitmapPPM, BitmapXBM, BitmapX11, nBitmapFormats };
	static BitmapFormat bitmapFormat(const char *name, bool reportError = 0);
	static BitmapFormat bitmapFormatFromFilter(const char *s);
	static const char *bitmapFormatFilter(BitmapFormat bf);
	static const char *bitmapFormatExtension(BitmapFormat bf);
	// Update Targets
	enum UpdateTarget { AtomsTarget = 1, CellTarget = 2, ForcefieldsTarget = 4, GlyphsTarget = 8, GridsTarget = 16, ModelsTarget = 32, CanvasTarget = 64, StatusBarTarget = 128, GeometryTarget = 256, VibrationsTarget = 512, SelectTarget = 1024, TrajectoryTarget = 2048, AllTarget = 4095 };


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
	// Return the PID of Aten
	int pid();
	// Process application messages
	void processMessages();
	// Set interactivity (to full or zero), except for main view camera changes
	void setInteractive(bool interactive);

	/*
	// Refresh Functions
	*/
	public:
	// Refreshes all widgets to do with the model
	void update(int targets = 0);
	// Initialise (but don't show) the progress dialog
	void initialiseProgressDialog();
	// Update progress dialog, showing the window and disabling GUI in the process
	void updateProgressDialog();
	// Send signal to close progress dialog
	void terminateProgressDialog();


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


	/*
	// Windows / Subwindows
	*/
	private:
	// Main application
	QApplication *application_;
	// Main window
	AtenForm *mainWindow_;
	// Main rendering canvas
	TCanvas *mainWidget_;
	// Original QGLContext, created in initialise()
	QGLContext *mainContext_;
	// Type of application initialised
	QApplication::Type applicationType_;

	public:
	// Return main view Widget
	TCanvas *mainWidget();
	// Main application structure
	QApplication *application();
	// Return type of application initialised
	QApplication::Type applicationType();
	// Main Window
	AtenForm *mainWindow();
	// Update context menu
	void updateContextMenu();
	// Call the atompopup menu
	void callContextMenu(Atom*, int, int);

	
	/*
	// Dock Widgets
	*/
	private:
	// List of dock widgets
	QList<QDockWidget*> dockWidgets_;
	
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
	// Disorder wizard
	DisorderWizard *disorderWizard;
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
	// Messages dock widget
	MessagesWidget *messagesWidget;
	// Model List dock widget
	ModelListWidget *modelListWidget;
	// Atom positioning dock widget
	PositionWidget *positionWidget;
	// Scripted movie dock widget
	ScriptMovieWidget *scriptMovieWidget;
	// Atom selection dock widget
	SelectWidget *selectWidget;
	// Toolbox dock widget
	ToolBoxWidget *toolBoxWidget;
	// Trajectory control dock widget
	TrajectoryWidget *trajectoryWidget;
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
	// Progress indicator
	AtenProgress *progressDialog;
	// Zmatrix window
	AtenZMatrix *zmatrixDialog;
};

extern GuiQt gui;

#endif
