/*
	*** Preferences storage
	*** src/base/prefs.h
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

#ifndef ATEN_PREFS_H
#define ATEN_PREFS_H

#include "base/forms.h"
#include "base/atom.h"
#include "base/elements.h"
#include "classes/colourscale.h"
#include "base/dnchar.h"

// Forward declarations
class Cell;

// Prefs
class Prefs
{
	/*
	// Enumerations
	*/
	public:
	// Mouse buttons
	enum MouseButton { LeftButton, MiddleButton, RightButton, WheelButton, nMouseButtons };
	static MouseButton mouseButton(const char*);
	static const char *mouseButton(MouseButton);
	// Mouse Actions
	enum MouseAction { NoAction, RotateAction, TranslateAction, InteractAction, ZoomAction, ZrotateAction, nMouseActions };
	static MouseAction mouseAction(const char*);
	static const char *mouseAction(MouseAction);
	// Modifier keys
	enum ModifierKey { ShiftKey, CtrlKey, AltKey, nModifierKeys };
	static ModifierKey modifierKey(const char*);
	// Modifier actions
	enum KeyAction { NoKeyAction, ManipulateKeyAction, ZrotateKeyAction, nKeyActions };
	static KeyAction keyAction(const char*);
	// Standard 'Pen' Colours
	enum PenColour { ForegroundColour, BackgroundColour, SpecularColour, GlyphColour, nPenColours };
	static const char *penColour(PenColour);
	static PenColour penColour(const char*);
	// Energy Units
	enum EnergyUnit { Joules, KiloJoules, Calories, KiloCalories, ElectronVolts, Hartree, nEnergyUnits };
	static const char *energyUnit(EnergyUnit);
	static EnergyUnit energyUnit(const char*);
	// Density calculation units
	enum DensityUnit { GramsPerCm, AtomsPerAngstrom, nDensityUnits };
	static const char *densityUnit(DensityUnit);
	static DensityUnit densityUnit(const char*);
	// View Objects
	enum ViewObject { ViewAtoms=1, ViewCell=2, ViewCellAxes=4, ViewCellRepeat=8, ViewForceArrows=16, ViewGlobe=32, ViewLabels=64, ViewMeasurements=128, ViewRegions=256, ViewSurfaces=512, nViewObjects=11 };
	static ViewObject viewObject(const char*);
	static const char *viewObject(ViewObject);
	// GL Options
	enum GlOption { FogOption=1, LineAliasOption=2, PolyAliasOption=4, BackCullOption=8, DummyOption=16, nGlOptions=5 };
	static GlOption glOption(const char*);
	// Atom colouring scheme
	enum ColouringScheme { ElementScheme, ChargeScheme, VelocityScheme, ForceScheme, nColouringSchemes };
	static ColouringScheme colouringScheme(const char*);
	static const char *colouringScheme(ColouringScheme cs);
	// Filter override switches
	enum FilterSwitch { SwitchAsFilter, SwitchOff, SwitchOn };
	// Drawing guide geometry
	enum GuideGeometry { SquareGuide, HexagonalGuide, nGuideGeometries };
	// Spotlight Components
	enum ColourComponent { AmbientComponent, DiffuseComponent, SpecularComponent, nColourComponents };

	public:
	// Constructor
	Prefs();
	// Load preferences from file
	void load(const char*);

	/*
	// Rendering - View Objects
	*/
	private:
	// List of visibilities of renderable objects on screen and on image
	int screenObjects_, imageObjects_;
	// Repeat units in positive xyz directions
	Vec3<int> repeatCellsPos_;
	// Repeat units in negative xyz directions
	Vec3<int> repeatCellsNeg_;
	// Size in pixels of the viewport to draw the rotation globe in.
	int globeSize_;
	// Rendering style of models
	Atom::DrawStyle renderStyle_;

	public:
	// Set the visibility of an object on-screen
	void setVisibleOnScreen(ViewObject vo, bool b);
	// Set the visibility of an object in saved images
	void setVisibleOnImage(ViewObject vo, bool b);
	// Return whether the specified object is visible (i.e. should be rendered)
	bool isVisibleOnScreen(ViewObject vo);
	// Return whether the specified object is visible (i.e. should be rendered) in saved images
	bool isVisibleOnImage(ViewObject vo);
	// Return screenobjects bitvector
	int screenObjects();
	// Set screenobjects bitvector
	void setScreenObjects(int i);
	// Return screenobjects bitvector
	int imageObjects();
	// Set imageobjects bitvector
	void setImageObjects(int i);
	// Return the radius of an atom calculated from the element and draw style
	double screenRadius(Atom*);
	// Set the drawing style of models
	void setRenderStyle(Atom::DrawStyle ds);
	// Return the current drawing style of models
	Atom::DrawStyle renderStyle();
	// Return the current rotation globe size in pixels
	int globeSize();
	// Set positive repeat cell value
	void setRepeatCellsPos(int i, int r);
	// Get positive repeat cell value
	int repeatCellsPos(int i);
	// Set negative repeat cell value
	void setRepeatCellsNeg(int i, int r);
	// Get negative repeat cell value
	int repeatCellsNeg(int i);

	/*
	// Rendering - Style
	*/
	private:
	// Atom colouring style
	Prefs::ColouringScheme colourScheme_;
	// Atom sizes / radii
	GLdouble atomStyleRadius_[Atom::nDrawStyles];
	// Bond radius for Scaled and Sphere drawing styles
	GLdouble bondRadius_;
	// Size scaling for atom selection transparency
	GLdouble selectionScale_;
	// Detail of atom quadric (slices/stacks)
	int atomDetail_;
	// Detail of bond quadric (slices/stacks)
	int bondDetail_;
	// Whether to use a perspective (TRUE) or orthographic (FALSE) projection
	bool perspective_;
	// Viewing angle for perspective projection
	GLdouble perspectiveFov_;
	// Whether the spotlight is on
	bool spotlightActive_;
	// Spotlight components
	GLfloat spotlightColour_[Prefs::nColourComponents][4];
	// Spotlight position
	GLfloat spotlightPosition_[4];

	public:
	// Sets the specified atom size to the given value
	void setAtomStyleRadius(Atom::DrawStyle ds, double f);
	// Return the specified atom radius
	GLdouble atomStyleRadius(Atom::DrawStyle ds);
	// Sets the bond radius used in Scaled and Sphere styles
	void setBondRadius(double f);
	// Return the bond radius used in Scaled and Sphere styles
	GLdouble bondRadius();
	// Sets the detail for atom quadrics
	void setAtomDetail(int n);
	// Return the current detail of atom quadrics
	int atomDetail();
	// Sets the detail for bond quadrics
	void setBondDetail(int n);
	// Return the current detail of bond quadrics
	int bondDetail();
	// Sets the scale of selected atoms
	void setSelectionScale(double f);
	// Return the scale of selected atoms
	GLdouble selectionScale();
	// Return whether perspective viewing is enabled
	bool hasPerspective();
	// Sets perspective viewing on/off
	void setPerspective(bool b);
	// Set the perspective field of view angle
	void setPerspectiveFov(double fov);
	// Return the perspective field of view angle
	double perspectiveFov();
	// Set status of spotlight
	void setSpotlightActive(bool status);
	// Return status of spotlight
	bool spotlightActive();
	// Set spotlight colour component
	void setSpotlightColour(ColourComponent sc, int i, GLfloat value);
	void setSpotlightColour(ColourComponent sc, GLfloat r, GLfloat g, GLfloat b);
	// Return spotlight colour component
	GLfloat *spotlightColour(ColourComponent sc);
	// Set spotlight position
	void setSpotlightPosition(GLfloat r, GLfloat g, GLfloat b);
	void setSpotlightPosition(int component, GLfloat f);
	// Return spotlight position
	GLfloat *spotlightPosition();
	// Set atom colour scheme
	void setColourScheme(Prefs::ColouringScheme sc);
	// Return atom colour scheme
	Prefs::ColouringScheme colourScheme();
	// Set number of segments in colour scale
	void setScaleSegments(int nsegments);
	// Get number of segments in colour scale
	int nScaleSegments();


	/*
	// Rendering - Options
	*/
	private:
	// Postfix (units) label for distances
	Dnchar distanceLabel_;
	// Postfix (units) label for angles
	Dnchar angleLabel_;
	// Pointsize for labels
	int labelSize_;
	// Use QGlWidget::renderText (FALSE) or QPainter::drawText (TRUE) for labels etc.
	bool useNiceText_;

	public:
	// Set the postfix distance label
	void setDistanceLabel(const char *s);
	// Return the postfix distance label
	const char *distanceLabel();
	// Set the postfix angle label
	void setAngleLabel(const char *s);
	// Return the postfix angle label
	const char *angleLabel();
	// Set the pointsize of labels in the model
	void setLabelSize(int size);
	// Return the current label pointsize
	int labelSize();
	// Set whether to use nice text rendering
	void setUseNiceText(bool b);
	// Return whether to use nice text rendering
	bool useNiceText();

	/*
	// GL Options
	*/
	private:
	// Bitvector for GL options
	int glOptions_;
	// Shininess of 3D objects
	GLint shininess_;
	// Fog start and finish depths
	GLint fogNear_, fogFar_;
	// Near and far clipping planes for glPerspective() and glFrustum();
	GLdouble clipNear_, clipFar_;

	public:
	// Set the bit for the specified option (if it is not set already)
	void addGlOption(GlOption go);
	// Unsets the bit for the specified option (if it is not unset already)
	void removeGlOption(GlOption go);
	// Return whether a given option is set
	bool hasGlOption(GlOption go);
	// Sets the start depth of depth cueing
	void setFogNnear(int i);
	// Return depth cue start depth
	GLint fogNear();
	// Sets the end depth of depth cueing
	void setFogFar(int i);
	// Return depth cue end depth
	GLint fogFar();
	// Return the Z depth of the near clipping plane
	GLdouble clipNear();
	// Return the Z depth of the far clipping plane
	GLdouble clipFar();
	// Sets the shininess of GL objects
	void setShininess(int n);
	// Return the current shininess of GL objects
	GLint shininess();


	/*
	// Colours
	*/
	private:
	// RGB colour values
	GLfloat colours_[Prefs::nPenColours][4];

	public:
	// Set the specified colour to the integer RGB values supplied
	void setColour(PenColour c, GLfloat r, GLfloat g, GLfloat b, GLfloat a);
	// Copy the specified colour
	void copyColour(PenColour c, GLfloat*);
	// Return a pointer to the specified colour
	GLfloat *colour(PenColour c);
	// User-definable colour scales
	ColourScale colourScale[10];


	/*
	// File Preferences
	*/
	private:
	// Recalculate bonding when model has loaded
	FilterSwitch bondOnLoad_;
	// Centre non-periodic models on load
	FilterSwitch centreOnLoad_;
	// Fold atomic positions after model load
	FilterSwitch foldOnLoad_;
	// Whether to apply symmetry operators to get crystal packing on load
	FilterSwitch packOnLoad_;
	// Whether to load in all coordinate sets from a file
	bool loadAllCoords_;
	// Convert coordinates from Bohr to Angstrom on load
	bool coordsInBohr_;
	// Size limit (kbytes) for caching trajectory frames
	int cacheLimit_;
	// Type of name->Z mapping to use
	ElementMap::ZmapType zmapType_;
	// Whether to retain file atom type names on load (in a new forcefield)
	bool keepNames_;
	// Whether to retain view when GUI starts (i.e. don't reset it)
	bool keepView_;

	public:
	// Sets whether to calculate bonding on model load
	void setBondOnLoad(FilterSwitch s);
	// Whether bonding should be recalculated on model load
	FilterSwitch bondOnLoad();
	// Sets whether to centre molecule on load
	void setCentreOnLoad(FilterSwitch s);
	// Whether molecule should be centred on model load
	FilterSwitch centreOnLoad();
	// Sets whether to fold atomic positions after model load
	void setFoldOnLoad(FilterSwitch s);
	// Whether atoms should be folded after model load
	FilterSwitch foldOnLoad();
	// Sets whether to apply symmetry operators (pack) on load
	void setPackOnLoad(FilterSwitch s);
	// Whether atoms should be packed (with symmetry operations) after model load
	FilterSwitch packOnLoad();
	// Sets whether to load all coordinate sets on model load
	void setLoadAllCoords(bool b);
	// Whether all geometries in a non-trajectory file should be loaded
	bool loadAllCoords();
	// Set the cache limit (in kb) for trajectory files
	void setCacheLimit(int i);
	// Return the cache limit for trajectory files
	int cacheLimit();
	// Sets the style of element conversion to use
	void setZmapType(ElementMap::ZmapType i);
	// Return the style of element conversion in use
	ElementMap::ZmapType zmapType();
	// Sets whether to convert coords from Bohr to Angstrom on load
	void setCoordsInBohr(bool b);
	// Whether coordinates should be converted from Bohr to Angstrom
	bool coordsInBohr();
	// Set whether to keep file type names on load
	void setKeepNames(bool b);
	// Return whether to keep file type names on load
	bool keepNames();
	// Set whether to keep view on GUI start
	void setKeepView(bool b);
	// Return whether to keep view on GUI start
	bool keepView();


	/*
	// Edit Preferences
	*/
	private:
	// Bonding tolerance for automatic calculation
	double bondTolerance_;
	// Depth for drawing guide
	double drawDepth_;
	// Spacing of grid on drawing guide
	double guideSpacing_;
	// Extent (+- guide_spacing in xy plane) of drawing guide 
	int guideExtent_;
	// Number of ticks between gridpoints of guide
	int guideTicks_;
	// Whether to show the drawing guide
	bool showGuide_;
	// Geometry of the grid in the drawing guide
	Prefs::GuideGeometry guideShape_;
	// User-definable mouse button actions
	MouseAction mouseAction_[Prefs::nMouseButtons];
	// User-definable key modifier actions
	KeyAction keyAction_[Prefs::nModifierKeys];

	public:
	// Sets the bonding tolerance
	void setBondTolerance(double v);
	// Return the bonding tolerance for automatic calculation
	double bondTolerance();
	// Sets the position of the drawing guide
	void setDrawDepth(double v);
	// Return the current position of the drawing guide
	double drawDepth();
	// Spacing of grid on drawing guide
	void setGuideSpacing(double spacing);
	// Spacing of grid on drawing guide
	double guideSpacing();
	// Extent (+- guide_spacing in xy plane) of drawing guide 
	void setGuideExtent(int extent);
	// Return extent (+- guide_spacing in xy plane) of drawing guide 
	int guideExtent();
	// Number of ticks between gridpoints of guide
	void setGuideTicks(int nticks);
	// Number of ticks between gridpoints of guide
	int guideTicks();
	// Sets the visibility of the drawing guide
	void setGuideVisible(bool b);
	// Return whether the draw guide is visible
	bool isGuideVisible();
	// Sets the shape of the drawing guide
	void setGuideShape(Prefs::GuideGeometry g);
	// Return guide shape
	Prefs::GuideGeometry guideShape();
	// Sets the action for the specified mouse button
	void setMouseAction(MouseButton mb, MouseAction ma);
	// Return the action associated with the specified mouse button
	MouseAction mouseAction(MouseButton mb);
	// Sets the modifier key for the specified action
	void setKeyAction(ModifierKey mk, KeyAction ka);
	// Return the action associated with the specified keymod button
	KeyAction keyAction(ModifierKey mk);


	/*
	// Method Preferences
	*/
	private:
	// Main modelview update and energy output frequencies
	int modelUpdate_, energyUpdate_;
	// Maximum ring size in ring search algorithm
	int maxRingSize_;
	// Whether to fold atoms before replication
	bool replicateFold_;
	// Whether to trim atoms after replication
	bool replicateTrim_;

	public:
	// Set the model update frequency
	void setModelUpdate(int n);
	// Return the model update frequency
	int modelUpdate();
	// Set the energy update frequency
	void setEnergyUpdate(int n);
	// Return the energy update frequency
	int energyUpdate();
	// Return whether to update the energy, given the cycle number
	bool shouldUpdateEnergy(int n);
	// Return the maximum ring size allowed
	int maxRingSize();
	// Set whether to fold atoms before replication
	void setReplicateFold(bool b);
	// Return whether to fold atoms before replication
	bool replicateFold();
	// Set whether to trim atoms after replication
	void setReplicateTrim(bool b);
	// Return whether to trim atoms after replication
	bool replicateTrim();


	/*
	// Units and Conversion
	*/
	private:
	DensityUnit densityUnit_;
	// Internal energy units to use for forcefield storage, energy calculation etc.
	EnergyUnit energyUnit_;
	// Conversion factors for energy units
	double energyConversions_[Prefs::nEnergyUnits];
	// Factor to convert from atomic units to internal units
	double elecConvert_;

	public:
	// Sets the current internal energy unit
	void setEnergyUnit(EnergyUnit eu);
	// Return the working energy units
	EnergyUnit energyUnit();
	// Set the density unit to use
	void setDensityUnits(DensityUnit du);
	// Return the current density units to use
	DensityUnit densityUnit();
	// Convert the units of the given quantity
	double convertEnergy(double energy, EnergyUnit);
	// Return the electrostastic energy conversion factor
	double elecConvert();


	/*
	// Expression (general parameters)
	*/
	private:
	// Method of electrostatic calculation
	Electrostatics::ElecMethod electrostaticsMethod_;
	// Whether to calculate VDW interactions
	bool calculateVdw_;
	// Whether to calculate electrostatic interactions
	bool calculateElec_;
	// Whether to calculate intramolecular interactions
	bool calculateIntra_;
	// Ewald sum extent
	Vec3<int> ewaldKvec_;
	// Ewald sum gaussian width and (for auto option) precision
	double ewaldAlpha_;
	// Ewald sum precision for automatic parameter estimation
	double ewaldPrecision_;
	// Cutoff distances for VDW and electrostatics
	double vdwCutoff_, elecCutoff_;
	// Scale factor for VDW radii (used in disorder build)
	double vdwScale_;
	// Whether the automatic Ewald setup is valid
	bool validEwaldAuto_;

	public:
	// Sets the electrostatic model to use in energy/force calculation
	void setElectrostaticsMethod(Electrostatics::ElecMethod em);
	// Return the type of electrostatic treatment to use
	Electrostatics::ElecMethod electrostaticsMethod();
	// Sets whether to calculate intramolecular interactions
	void setCalculateIntra(bool b);
	// Return whether to calculate intramolocular interactions
	bool calculateIntra();
	// Sets whether to calculate VDW interactions
	void setCalculateVdw(bool b);
	// Return whether to calculate VDW interactions
	bool calculateVdw();
	// Sets whether to calculate electrostatic interactions
	void setCalculateElec(bool b);
	// Return whether to calculate electrostatic interactions
	bool calculateElec();
	// Sets the Ewald k-vector extents
	void setEwaldKvec(int a, int b, int c);
	void setEwaldKvec(Vec3<int> v);
	// Return the Ewald k-vector extents
	Vec3<int> ewaldKvec();
	// Sets the Ewald precision
	void setEwaldPrecision(double d);
	// Return the Ewald precision
	double ewaldPrecision();
	// Set the Gaussian width to use in the Ewald sum
	void setEwaldAlpha(double d);
	// Return the Ewald alpha value
	double ewaldAlpha();
	// Set the short-range and electrostatic cutoffs
	void setCutoffs(double vcut, double ecut);
	// Estimate Ewald sum parameters for the current unit cell
	void estimateEwaldParameters(Cell*);
	// Return the validity of automatic Ewald params (invalidated on cell change)
	bool hasValidEwaldAuto();
	// Flag the Ewald auto params as invalid
	void invalidateEwaldAuto();
	// Sets the VDW cutoff radius to use
	void setVdwCutoff(double d);
	// Return the VDW cutoff radius
	double vdwCutoff();
	// Sets the electrostatic cutoff radius to use
	void setElecCutoff(double d);
	// Return the electrostatic cutoff radius
	double elecCutoff();
	// Sets the vdw radius scaling factor
	void setVdwScale(double d);
	// Return the VDW radius scaling factor
	double vdwScale();


	/*
	// Undo levels
	*/
	private:
	// Maximum number of undo levels (-1 for unlimited)
	int maxUndoLevels_;

	public:
	// Set the maximum number of undo levels allowed
	void setMaxUndoLevels(int n);
	// Return the maximum number of undo levels allowed
	int maxUndoLevels();


	/*
	// GUI
	*/
	private:
	// List of common drawing elements to put in SelectElement dialog
	Dnchar commonElements_;

	public:
	// Set list of common elements in SelectElement dialog
	void setCommonElements(const char *s);
	// Return list of common elements to use in SelectElement dialog
	const char *commonElements();
};

extern Prefs prefs;

#endif
