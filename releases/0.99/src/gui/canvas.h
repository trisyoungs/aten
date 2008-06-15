/*
	*** Qt canvas
	*** src/gui/canvas.h
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

#ifndef ATEN_CANVASQT_H
#define ATEN_CANVASQT_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "base/prefs.h"
#include "render/gl2ps_extra.h"

// GL Objects
enum GlObject { GLOB_STICKATOM, GLOB_TUBEATOM, GLOB_SPHEREATOM, GLOB_UNITATOM, GLOB_WIRETUBEATOM, GLOB_WIRESPHEREATOM, GLOB_WIREUNITATOM, GLOB_CYLINDER, GLOB_SELCYLINDER, GLOB_WIRECYLINDER, GLOB_SELWIRECYLINDER, GLOB_GLOBE, GLOB_GUIDE, GLOB_CIRCLE, GLOB_CELLAXES, GLOB_SELTUBEATOM, GLOB_SELSPHEREATOM, GLOB_SELUNITATOM, GLOB_WIREUNITCUBE, GLOB_UNITCUBE, GLOB_TUBEARROW, GLOB_MODEL, GLOB_NITEMS };

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
	char text[128];
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

	// Actions
	enum UserAction { NoAction, SelectAction, SelectMoleculeAction, SelectElementAction, SelectRadialAction, MeasureDistanceAction, MeasureAngleAction, MeasureTorsionAction, EditDrawAction, EditChainAction, EditTransmuteAction, EditDeleteAction, EditProbeAction, EditBondSingleAction, EditBondDoubleAction, EditBondTripleAction, EditDeleteBondAction, EditAddHydrogenAction, RotateXYAction, RotateZAction, TranslateAction, ZoomAction, TransformRotateXYAction, TransformRotateZAction, TransformTranslateAction, nUserActions };
	// Keyboard Key Codes (translated from GTK/Qt keysyms)
	enum KeyCode { OtherKey, EscapeKey, LeftShiftKey, RightShiftKey, LeftControlKey, RightControlKey, LeftAltKey, RightAltKey, LeftKey, RightKey, UpKey, DownKey, nKeyCodes };

	/*
	// Base rendering context
	*/
	private:
	// Internal name of the canvas for error reporting
	const char *name_;
	// Width, height, and aspect ratio of the canvas
	double width_, height_, aspect_;
	// Point at which the stored atom display list was valid (sum of Change::StructureLog and Change::CoordinateLog points)
	int renderPoint_;
	// Flag to indicate whether we may draw to the canvas
	bool valid_;
	// Flag indicating if we are currently drawing to this canvas
	bool drawing_;
	// Flag to prevent rendering (used to restrict unnecessary renders before canvas is even visible)
	bool noDraw_;
	// Qt Target widget
	TCanvas *contextWidget_;

	public:
	// Set the internal name of the canvas
	void setName(const char *s);
	// Return the current height of the drawing area
	double height();
	// Return the current width of the drawing area
	double width();
	// Return whether the canvas is currently drawing
	bool isDrawing();
	// Return if the canvas is valid
	bool isValid();
	// Set the validity of the canvas
	void setValid(bool);
	// Set up widget for OpenGL drawing
	bool setWidget(TCanvas*);
	// Update Canvas
	void postRedisplay();
	// Called when context is initialised and ready
	void realize();
	// Called when context is resized
	void configure();
	// Called when context needs to be redrawn
	void expose();
	// Enable rendering
	void enableDrawing();
	// Disable rendering
	void disableDrawing();

	/*
	// Rendering display lists
	*/
	private:
	// Display list ID's
	GLuint list_[GLOB_NITEMS];

	public:
	// Create globs for rendering
	void createLists();

	/*
	// Rendering Primitives
	*/
	private:
	// Draw a diamond
	void diamondPrimitive(double xcenter, double ycentre, double size);
	// Draw a square
	void squarePrimitive(double xcentre, double ycentre, double size);
	// Draw a rectangle
	void rectanglePrimitive(double l, double t, double r, double b);
	// Draw a circle
	void circlePrimitive(double xcentre, double ycenter, double radius);
	// Manually draw a unit sphere
	void spherePrimitive(double radius, bool filled);
	// Manually draw unit cylinder
	void cylinderPrimitive(double startradius, double endradius, bool filled);

	/*
	// General Rendering Objects / Calls
	*/
	private:
	// Render text string at specific coordinates
	void glText(double, double, const char*);
	// Render text string at atom's screen coordinates
	void glText(const Vec3<double>, const char*);
	// Draw 3d marks for the atoms in the subselection
	void glSubsel3d();
	// Draw a cylinder along vector supplied
	void glCylinder(const Vec3<double> &vec, double length, int style);
	// Draw ellipsoid
	void glEllipsoid(const Vec3<double>&, const Vec3<double>&, const Vec3<double>&);
	// Draw the unit cell of the model
	void glCell(Cell*);
	// Draw a line arrow
	void glArrow(const Vec3<double> &origin, const Vec3<double> &vector, bool swaphead = FALSE);
	// Draw a cylinder arrow
	void glCylinderArrow(const Vec3<double> &origin, const Vec3<double> &vector, bool swaphead = FALSE);


	protected:
	// Last model rendered by canvas (needed for mouse hover etc.)
	Model *displayModel_;

	public:
	// Configure OpenGL
	void initGl();
	// Begin construct for any OpenGL commands
	virtual bool beginGl();
	// Finish OpenGL commands
	virtual void endGl();
	// Reset the projection matrix based on the current canvas geometry
	void doProjection();
	// Projection matrices for scene and rotation globe
	Mat4<double> PMAT, GlobePMAT;
	// Viewport matrix for canvas
	GLint VMAT[4];
	// Return the current display model
	Model *displayModel();

	/*
	// Scene Rendering
	*/
	private:
	// List of text nuggets to render
	List<TextObject> textObjects_;
	// Render colourscales
	void renderColourscales();
	// Add extra 2D objects
	void renderExtra2d();
	// Add extra 3D objects
	void renderExtra3d();
	// Render the model specified
	void renderModelAtoms();
	// Render model cell
	void renderModelCell();
	// Draw model force arrows		// TODO Defunct now glyphs are available?
	void renderModelForceArrows();
	// Render glyphs in the current model
	void renderModelGlyphs();
	// Add labels to the model
	void renderModelLabels();
	// Add geometry measurements to the model
	void renderModelMeasurements();
	// Render text glyphs in the current model
	void renderModelTextGlyphs();
	// Draw regions specified for MC insertion
	void renderRegions();
	// Render the rotation globe
	void renderRotationGlobe(double *rotmat, double camrot);
	// Render surfaces
	void renderSurfaces();

	public:
	// Render a scene based on the specified model
	void renderScene(Model*);
	// Render text for the current scene
	void renderText(QPainter&);
	// Save scene as vector image
	void saveVector(Model *source, vector_format vf, const char *filename);

	/*
	// Selection
	*/
	protected:
	// Atom that the mouse pointer is currently hovering over
	Atom *atomHover_;
	// Subselection (list of clicked atoms for interactive tools)
	Reflist<Atom,int> subselection_;
	// Whether we are selecting atoms and placing them in the subsel list	
	bool subselectEnabled_;
	// Reflist of atoms selected, filled in some interaction modes
	Reflist< Atom,Vec3<double> > rSelection_;

	public:
	// Returns the atom currently under the mouse
	Atom *atomHover();
	// Clears the subsel of atoms
	void clearSubselection();

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
	UserAction activeMode_;
	// Selected interaction mode (from GUI)
	UserAction selectedMode_;
	// Button flags (uses enum 'MouseButton')
	bool mouseButton_[Prefs::nMouseButtons];
	// Key flags (uses enum 'ModifierKey')
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

	public:
	// Set the active mode to the current user mode
	void useSelectedMode();
	// Sets the currently selected interact mode
	void setSelectedMode(UserAction);
	// Return the currently selected mode
	UserAction selectedMode();
	// Inform the canvas of a mouse down event
	void informMouseDown(Prefs::MouseButton, double, double);
	// Inform the canvas of a mouse up event
	void informMouseUp(Prefs::MouseButton, double, double);
	// Inform the canvas of a mouse move event
	void informMouseMove(double, double);
	// Inform the canvas of a mouse wheel scroll event
	void informScroll(bool);
	// Inform the canvas of a keydown event
	void informKeyDown(KeyCode);
	// Inform the canvas of a keydown event
	void informKeyUp(KeyCode);
};

#endif
