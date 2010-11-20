/*
	*** Rendering Canvas
	*** src/render/canvas.h
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

#ifndef ATEN_CANVAS_H
#define ATEN_CANVAS_H

#include "render/engine.h"
#include "templates/vector3.h"
#include "templates/reflist.h"
#include "gui/useractions.h"
#include "classes/prefs.h"
#include "base/log.h"

// Forward declarations
class Atom;
class Bond;
class Model;
class Geometry;
class Subselection;
class Cell;
class TCanvas;

// Text object
class TextObject
{
	public:
	// Constructor
	TextObject(int,int,bool,const char*);

	// Screen coordinates for text
	int x, y;
	// Whether to right-align text at the provided coordinate
	bool rightAlign;
	// Text to render
	Dnchar text;
	// List pointers
	TextObject *prev, *next;
};

/*
// Canvas Master Class
// Provides GL rendering functions for a context
*/
class Canvas
{
	public:
	// Constructor
	Canvas();
	
	// Keyboard Key Codes (translated from GTK/Qt keysyms)
	enum KeyCode { OtherKey, EscapeKey, LeftShiftKey, RightShiftKey, LeftControlKey, RightControlKey, LeftAltKey, RightAltKey, LeftKey, RightKey, UpKey, DownKey, nKeyCodes };

	// GL Objects
	enum GlObject { StickAtomGlob, TubeAtomGlob, SphereAtomGlob, UnitAtomGlob, WireTubeAtomGlob, WireSphereAtomGlob, WireUnitAtomGlob, GlobeGlob, GuideGlob, CircleGlob, CellAxesGlob, SelectedTubeAtomGlob, SelectedSphereAtomGlob, SelectedUnitAtomGlob, WireUnitCubeGlob, UnitCubeGlob, CrossedUnitCubeGlob, ModelGlob, nGlobs };


	/*
	// Base rendering context
	*/
	private:
	// Internal name of the canvas for error reporting
	const char *name_;
	// Width, height, and aspect ratio of the canvas
	GLsizei width_, height_;
	// Point at which the stored atom display list was valid (sum of Change::StructureLog and Change::CoordinateLog points)
	Log renderPoint_;
	// Flag to indicate whether we may draw to the canvas
	bool valid_;
	// Flag indicating if we are currently drawing to this canvas
	bool drawing_;
	// Flag to prevent rendering (used to restrict unnecessary renders before canvas is even visible)
	bool noDraw_;
	// Qt Target widget
	TCanvas *contextWidget_;
	// Flag used by some sub-rendering processes (e.g. surfaces) in order to decide which display list to use
	bool renderOffScreen_;

	public:
	// Set the internal name of the canvas
	void setName(const char *s);
	// Return the current height of the drawing area
	GLsizei height() const;
	// Return the current width of the drawing area
	GLsizei width() const;
	// Return whether the canvas is currently drawing
	bool isDrawing() const;
	// Return if the canvas is valid
	bool isValid() const;
	// Set the validity of the canvas
	void setValid(bool valid);
	// Set up widget for OpenGL drawing
	bool setWidget(TCanvas*);
	// Update Canvas
	void postRedisplay();
	// Called when context has changed size etc.
	void configure(int w, int h);
	// Enable rendering
	void enableDrawing();
	// Disable rendering
	void disableDrawing();
	// Set whether offscreen rendering is being performed
	void setOffScreenRendering(bool b);
	// Return whether offscreen renderinf is being performed
	bool offScreenRendering() const;


	/*
	// General Rendering Calle
	*/
	protected:
	// Last model rendered by canvas (needed for mouse hover etc.)
	Model *displayModel_;
	// Last frame ID rendered by the canvas
	int displayFrameId_;
	// Rendering engine
	RenderEngine engine_;

	public:
	// Configure OpenGL, generating display lists
	void initGl();
	// Set OpenGL options ready for drawing
	void prepGl();
	// Begin construct for any OpenGL commands
	bool beginGl();
	// Finish OpenGL commands
	void endGl();
	// Check for GL error
	void checkGlError();
	// Reset the projection matrix based on the current canvas geometry
	void doProjection();
	// Return the current display model
	Model *displayModel() const;
	// Render a scene based on the specified model
	void renderModel(Model*);
	// Project given model coordinates into world coordinates (and screen coordinates if Vec3 is supplied)
	Vec3<double> &modelToWorld(Vec3<double> pos, Mat4<double> &viewMatrix, Vec4<double> *screenr = NULL, double screenradius = 0.0);
	// Project the specified world coordinates into 2D screen coords
	Vec4<double> &worldToScreen(const Vec3<double>&, Mat4<double> &viewMatrix);


	/*
	// Selection
	*/
	private:
	// Number of atoms to pick in PickAtomsAction
	int nAtomsToPick_;
	// User action before picking mode was entered
	QAction *actionBeforePick_;
	// List of picked atoms
	Reflist<Atom,int> pickedAtoms_;
	// Pointer to callback function when PickAtomsAction exits
	void (*pickAtomsCallback_)(Reflist<Atom,int>*);
	// Atom that was clicked at the start of a mouse press event
	Atom *atomClicked_;
	// Whether we are selecting atoms and placing them in the subsel list	
	bool pickEnabled_;
	// Reflist of selected atoms and their positions so manipulations may be un-done
	Reflist< Atom,Vec3<double> > oldPositions_;

	public:
	// Returns the clicked atom within a mouse click event
	Atom *atomClicked();
	// Clears the subsel of atoms
	void clearPicked();
	// Manually enter picking mode to select N atoms
	void beginManualPick(int natoms, void (*callback)(Reflist<Atom,int>*));
	// End manual picking
	void endManualPick(bool resetaction);


	/*
	// Mouse
	*/
	protected:
	// Canvas coordinates of mouse down / mouse up events
	Vec3<double> rMouseUp_, rMouseDown_;
	// Canvas coordinates of mouse cursor
	Vec3<double> rMouseLast_;


	/*
	// Interaction
	*/
	protected:
	// Active interaction mode of the main canvas
	UserAction::Action activeMode_;
	// Selected interaction mode (from GUI)
	UserAction::Action selectedMode_;
	// Button flags (uses enum 'MouseButton')
	bool mouseButton_[Prefs::nMouseButtons];
	// Key flags (set by Gui::informMouseDown and used by TCanvas::beginMode)
	bool keyModifier_[Prefs::nModifierKeys];
	// Begin an action on the model (called from MouseButtondown)
	void beginMode(Prefs::MouseButton);
	// Handle mouse motion while performing actions
	void modeMotion(double, double);
	// Handle mousewheel scroll events
	void modeScroll(bool);
	// End an action on the model (called from MouseButtonup)
	void endMode(Prefs::MouseButton);
	// Whether the mouse has moved between begin_mode() and end_mode() calls
	bool hasMoved_;
	// Current drawing depth for certain tools
	double currentDrawDepth_;
	// Whether to accept editing actions (i.e. anything other than view manipulation)
	bool editable_;

	public:
	// Set the active mode to the current user mode
	void useSelectedMode();
	// Sets the currently selected interact mode
	void setSelectedMode(UserAction::Action ua);
	// Return the currently selected mode
	UserAction::Action selectedMode() const;
	// Inform the canvas of a mouse down event
	void informMouseDown(Prefs::MouseButton, double x, double y, bool shiftkey, bool ctrlkey, bool altkey);
	// Inform the canvas of a mouse up event
	void informMouseUp(Prefs::MouseButton, double, double);
	// Inform the canvas of a mouse move event
	void informMouseMove(double, double);
	// Inform the canvas of a mouse wheel scroll event
	void informScroll(bool);
	// Inform the canvas of a keydown event
	void informKeyDown(KeyCode kc, bool shiftkey, bool ctrlkey, bool altkey);
	// Inform the canvas of a keydown event
	void informKeyUp(KeyCode kc, bool shiftkey, bool ctrlkey, bool altkey);
	// Return modifier status
	bool modifierOn(Prefs::ModifierKey) const;
	// Set whether to accept editing actions (i.e. anything other than view manipulation)
	void setEditable(bool b);
	// Return whether to accept editing actions (i.e. anything other than view manipulation)
	bool editable();


	/*
	// New Rendering
	*/
	private:
	
};

#endif
