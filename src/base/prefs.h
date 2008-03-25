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

#include "energy/forms.h"
#include "templates/vector3.h"
#include "classes/atom.h"
#include <QtOpenGL/QtOpenGL>

// Atom colouring scheme
enum AtomColours { AC_ELEMENT, AC_CHARGE, AC_VELOCITY, AC_FORCE, AC_NITEMS };

// Preferences switches
enum PrefSwitch { PS_ASFILTER=-1, PS_NO, PS_YES };

// View Objects
enum ViewObject { VO_ATOMS, VO_CELL, VO_CELLAXES, VO_CELLREPEAT, VO_FORCEARROWS, VO_GLOBE, VO_LABELS, VO_MEASUREMENTS, VO_REGIONS, VO_SURFACES, VO_NITEMS };
ViewObject VO_from_text(const char*);

// GL Options
enum GlOption { GO_FOG=1, GO_LINEALIASING=2, GO_POLYALIASING=4, GO_BACKCULLING=8, GO_DUMMY=16, GO_NITEMS=5 };
GlOption GO_from_text(const char*);

// Mouse buttons
enum MouseButton { MB_LEFT, MB_MIDDLE, MB_RIGHT, MB_WHEEL, MB_NITEMS };
MouseButton MB_from_text(const char*);
const char *text_from_MB(MouseButton);

// Mouse Actions
enum MouseAction { MA_NONE, MA_VIEWROTATE, MA_VIEWTRANSLATE, MA_INTERACT, MA_VIEWZOOM, MA_VIEWZROTATE, MA_NITEMS };
MouseAction MA_from_text(const char*);
const char *text_from_MA(MouseAction);
const char **get_MA_strings();

// Modifier keys
enum ModifierKey { MK_SHIFT, MK_CTRL, MK_ALT, MK_NITEMS };
const char **get_MK_strings();
ModifierKey MK_from_text(const char*);

// Modifier actions
enum KeyAction { KA_NONE, KA_MANIPULATE, KA_ZROTATE, KA_NITEMS };
const char **get_KA_strings();
KeyAction KA_from_text(const char*);

// Colours
enum Colour { COL_PEN, COL_BG, COL_SCHEMELO, COL_SCHEMEMID, COL_SCHEMEHI, COL_SPECREFLECT, COL_NITEMS };
const char *text_from_COL(Colour);
Colour COL_from_text(const char*);

// Density calculation units
enum DensityUnit { DU_GPERCM, DU_ATOMSPERANG, DU_NITEMS };
const char *text_from_DU(DensityUnit);
DensityUnit DU_from_text(const char*);

// Drawing guide geometry
enum GuideGeometry { GG_SQUARE, GG_HEXAGONAL, GG_NITEMS };
const char **get_GG_strings();

// Energy Units
enum EnergyUnit { EU_J, EU_KJ, EU_CAL, EU_KCAL, EU_EV, EU_HARTREE, EU_NITEMS };
const char *text_from_EU(EnergyUnit);
EnergyUnit EU_from_text(const char*);

// Name->Z mapping methods
enum ZmapType { ZM_ALPHA, ZM_FIRSTALPHA, ZM_NAME, ZM_NUMERIC, ZM_FORCEFIELD, ZM_AUTO, ZM_NITEMS };
ZmapType ZM_from_text(const char*);
const char **get_ZM_keywords();

// Charge source
enum ChargeSource { QS_MODEL, QS_FF, QS_GASTEIGER, QS_QEQ, QS_NITEMS };

// Spotlight Components
enum SpotlightComponent { SL_AMBIENT, SL_DIFFUSE, SL_SPECULAR, SL_NITEMS };

// Forward declarations
class Cell;

// Prefs
class PrefsData
{
	public:
	// Constructor / Destructor
	PrefsData();
	~PrefsData();
	// Load prefs from file
	void load(const char*);
	// Set GUI controls to reflect prefs choices
	void setControls();

	/*
	// Rendering - View Objects
	*/
	private:
	// List of visibilities of renderable objects
	bool renderObjects_[VO_NITEMS];
	// Repeat units in positive xyz directions
	Vec3<int> repeatCellsPos_;
	// Repeat units in negative xyz directions
	Vec3<int> repeatCellsNeg_;
	// Scaling factor for 3D labels
	double labelScale_;
	// Size in pixels of the viewport to draw the rotation globe in.
	int globeSize_;
	// Rendering style of models
	Atom::DrawStyle renderStyle_;

	public:
	// Set the visibility of an object
	void setVisible(ViewObject vo, bool b);
	// Return whether the specified object is visible (i.e. should be rendered)
	bool shouldRender(ViewObject vo);
	// Return the radius of an atom calculated from the element and draw style
	double screenRadius(Atom*);
	// Set the drawing style of models
	void setRenderStyle(Atom::DrawStyle ds);
	// Return the current drawing style of models
	Atom::DrawStyle renderStyle();
	// Set the scale of labels in the model
	void setLabelScale(double v);
	// Return the current label scale
	double labelScale();
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
	// Atom sizes / radii
	GLdouble atomSize_[Atom::nDrawStyles];
	// Tube size for DS_SPHERE / DS_TUBE / DS_SCALED
	GLdouble tubeSize_;
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
	GLfloat spotlightColour_[SL_NITEMS][4];
	// Spotlight position
	GLfloat spotlightPosition_[3];
	// Atom colouring style
	AtomColours colourScheme_;
	// Number of segments between lo/hi and mid colours in colour scale
	int nScaleSegments_;
	// Graduated colour scale colours
	GLfloat **scaleColours_;

	public:
	// Sets the specified atom size to the given value
	void setAtomSize(Atom::DrawStyle ds, double f);
	// Return the specified atom size
	GLdouble atomSize(Atom::DrawStyle ds);
	// Sets the tube size in DS_TUBE
	void setTubeSize(double f);
	// Return the tube size used in DS_TUBE
	GLdouble tubeSize();
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
	void setSpotlightColour(SpotlightComponent sc, int i, GLfloat value);
	void setSpotlightColour(SpotlightComponent sc, GLfloat r, GLfloat g, GLfloat b);
	// Return spotlight colour component
	GLfloat *spotlightColour(SpotlightComponent sc);
	// Set spotlight position
	void setSpotlightPosition(GLfloat r, GLfloat g, GLfloat b);
	void setSpotlightPosition(int component, GLfloat f);
	// Return spotlight position
	GLfloat *spotlightPosition();
	// Set atom colour scheme
	void setColourScheme(AtomColours ac);
	// Return atom colour scheme
	AtomColours colourScheme();
	// Set number of segments in colour scale
	void setScaleSegments(int nsegments);
	// Get number of segments in colour scale
	int nScaleSegments();
	// Set colour scale colours
	void setScaleColours();
	// Copy colour scale segment into supplied array
	void copyScaleColour(int n, GLfloat *v);

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
	// Rendering - Colours
	*/
	private:
	// RGB colour values
	GLfloat colours_[COL_NITEMS][4];
	// Numerical low limit corresponding to COL_ACSCHEMELO
	double colourSchemeLo_[AC_NITEMS];
	// Numerical high limit corresponding to COL_ACSCHEMELO
	double colourSchemeHi_[AC_NITEMS];

	public:
	// Set the specified colour to the integer RGB values supplied
	void setColour(Colour c, GLfloat r, GLfloat g, GLfloat b, GLfloat a);
	// Return the specified colour
	GLfloat *colour(Colour c);
	// Return the low limit for the scheme specified
	double colourSchemeLo(int i);
	// Sets the low limit for the scheme specified
	void setColourSchemeLo(int i, double d);
	// Return the high limit for the scheme specified
	double colourSchemeHi(int i);
	// Sets the high limit for the scheme specified
	void setColourSchemeHi(int i, double d);

	/*
	// File Preferences
	*/
	private:
	// Recalculate bonding when model has loaded
	PrefSwitch bondOnLoad_;
	// Centre non-periodic models on load
	PrefSwitch centreOnLoad_;
	// Fold atomic positions after model load
	PrefSwitch foldOnLoad_;
	// Whether to apply symmetry operators to get crystal packing on load
	PrefSwitch packOnLoad_;
	// Whether to load in all coordinate sets from a file
	bool loadAllCoords_;
	// Convert coordinates from Bohr to Angstrom on load
	bool coordsInBohr_;
	// Size limit (kbytes) for caching trajectory frames
	int cacheLimit_;
	// Type of name->Z mapping to use
	ZmapType zmapType_;

	public:
	// Sets whether to calculate bonding on model load
	void setBondOnLoad(PrefSwitch s);
	// Whether bonding should be recalculated on model load
	PrefSwitch bondOnLoad();
	// Sets whether to centre molecule on load
	void setCentreOnLoad(PrefSwitch s);
	// Whether molecule should be centred on model load
	PrefSwitch centreOnLoad();
	// Sets whether to fold atomic positions after model load
	void setFoldOnLoad(PrefSwitch s);
	// Whether atoms should be folded after model load
	PrefSwitch foldOnLoad();
	// Sets whether to apply symmetry operators (pack) on load
	void setPackOnLoad(PrefSwitch s);
	// Whether atoms should be packed (with symmetry operations) after model load
	PrefSwitch packOnLoad();
	// Sets whether to load all coordinate sets on model load
	void setLoadAllCoords(bool b);
	// Whether all geometries in a non-trajectory file should be loaded
	bool loadAllCoords();
	// Set the cache limit (in kb) for trajectory files
	void setCacheLimit(int i);
	// Return the cache limit for trajectory files
	int cacheLimit();
	// Sets the style of element conversion to use
	void setZmapType(ZmapType i);
	// Return the style of element conversion in use
	ZmapType zmapType();
	// Sets whether to convert coords from Bohr to Angstrom on load
	void setCoordsInBohr(bool b);
	// Whether coordinates should be converted from Bohr to Angstrom
	bool coordsInBohr();

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
	GuideGeometry guideShape_;
	// User-definable mouse button actions
	MouseAction mouseAction_[MB_NITEMS];
	// User-definable key modifier actions
	KeyAction keyAction_[MK_NITEMS];

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
	void setGuideShape(GuideGeometry g);
	// Return guide shape
	GuideGeometry guideShape();
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

	/*
	// Units and Conversion
	*/
	private:
	DensityUnit densityUnit_;
	// Internal energy units to use for forcefield storage, energy calculation etc.
	EnergyUnit energyUnit_;
	// Conversion factors for energy units
	double energyConversions_[EU_NITEMS];
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
	ElecMethod electrostaticsMethod_;
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
	// Where to get charges from for the model
	ChargeSource chargeSource_;
	// Whether the automatic Ewald setup is valid
	bool validEwaldAuto_;

	public:
	// Sets the electrostatic model to use in energy/force calculation
	void setElectrostaticsMethod(ElecMethod em);
	// Return the type of electrostatic treatment to use
	ElecMethod electrostaticsMethod();
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
	// Set the charge source for the model
	void setChargeSource(ChargeSource cs);
	// Get the charge source for the model
	ChargeSource chargeSource();

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
};

extern PrefsData prefs;

#endif
