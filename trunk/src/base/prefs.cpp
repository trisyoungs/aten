/*
	*** Preferences storage
	*** src/base/prefs.cpp
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

#include "classes/forcefield.h"
#include "parse/parser.h"
#include "base/master.h"
#include "base/elements.h"
#include "base/prefs.h"
#include "base/sysfunc.h"

Prefs prefs;

// GL Options
const char *GlOptionKeywords[Prefs::nGlOptions] = { "fog", "linealias", "polyalias", "backcull", "__DUMMY__" };
Prefs::GlOption Prefs::glOption(const char *s)
{
	return (Prefs::GlOption) power(2,enumSearch("GL option",Prefs::nGlOptions,GlOptionKeywords,s));
}

// Colour Schemes
const char *ColouringSchemeKeywords[Prefs::nColouringSchemes] = { "element", "charge", "velocity", "force" };
Prefs::ColouringScheme Prefs::colouringScheme(const char *s)
{
	return (Prefs::ColouringScheme) enumSearch("colour scheme",Prefs::nColouringSchemes,ColouringSchemeKeywords,s);
}

// Mouse buttons
const char *MouseButtonKeywords[Prefs::nMouseButtons] = { "left", "middle", "right", "wheel" };
Prefs::MouseButton Prefs::mouseButton(const char *s)
{
	return (Prefs::MouseButton) enumSearch("mouse button", Prefs::nMouseButtons, MouseButtonKeywords, s);
}
const char *Prefs::mouseButton(Prefs::MouseButton i)
{
	return MouseButtonKeywords[i];
}

// Mouse actions
const char *MouseActionKeywords[Prefs::nMouseActions] = { "None", "Rotate", "Translate", "Interact", "Zoom", "Z-Rotate" };
Prefs::MouseAction Prefs::mouseAction(const char *s)
{
	return (Prefs::MouseAction) enumSearch("mouse action", Prefs::nMouseActions,  MouseActionKeywords, s);
}
const char *Prefs::mouseAction(Prefs::MouseAction i)
{
	return MouseActionKeywords[i];
}

// Key modifiers
const char *ModifierKeyKeywords[Prefs::nModifierKeys] = { "Shift", "Ctrl", "Alt" };
Prefs::ModifierKey Prefs::modifierKey(const char *s)
{
	return (Prefs::ModifierKey) enumSearch("modifier key", Prefs::nModifierKeys, ModifierKeyKeywords, s);
}

// Key actions
const char *KeyActionKeywords[Prefs::nKeyActions] = { "None", "Transform", "ZRotate" };
Prefs::KeyAction Prefs::keyAction(const char *s)
{
	return (Prefs::KeyAction) enumSearch("key action", Prefs::nKeyActions, KeyActionKeywords, s);
}

// Colours
const char *ColourKeywords[Prefs::nColours] = { "fg", "bg", "specular" };
Prefs::Colour Prefs::colour(const char *s)
{
	return (Prefs::Colour) enumSearch("colour", Prefs::nColours, ColourKeywords, s);
}

// Density calculation units
const char *DensityUnitKeywords[Prefs::nDensityUnits] = { "gpercm", "atomsperang" };
const char *Prefs::densityUnit(Prefs::DensityUnit i)
{
	return DensityUnitKeywords[i];
}
Prefs::DensityUnit Prefs::densityUnit(const char *s)
{
	return (Prefs::DensityUnit) enumSearch("density unit", Prefs::nDensityUnits, DensityUnitKeywords, s);
}

// Energy Units
const char *EnergyUnitFormatted[Prefs::nEnergyUnits] = { "J/mol", "kJ/mol", "cal/mol", "kcal/mol", "eV/mol", "Ha/mol" };
const char *EnergyUnitKeywords[Prefs::nEnergyUnits] = { "j", "kj", "cal", "kcal", "ev", "ha" };
const char *Prefs::energyUnit(Prefs::EnergyUnit i)
{
	return EnergyUnitKeywords[i];
}
Prefs::EnergyUnit Prefs::energyUnit(const char *s)
{
	return (Prefs::EnergyUnit) enumSearch("energy unit", Prefs::nEnergyUnits, EnergyUnitKeywords, s);
}

// ZMapping types
const char *ZM_keywords[Prefs::nZmapTypes] = { "alpha", "firstalpha", "name", "numeric", "ff", "auto" };
Prefs::ZmapType Prefs::zmapType(const char *s)
	{ return (Prefs::ZmapType) enumSearch("element mapping style",Prefs::nZmapTypes,ZM_keywords,s); }

// View Objects
const char *ViewObjectKeywords[Prefs::nViewObjects] = { "atoms", "cell", "cellaxes", "cellrepeat", "forcearrows", "globe", "labels", "measurements", "regions" };
Prefs::ViewObject Prefs::viewObject(const char *s)
{
	return (Prefs::ViewObject) enumSearch("view object", Prefs::nViewObjects, ViewObjectKeywords, s);
}

// Guide Geometries
const char *GG_strings[Prefs::nGuideGeometries] = { "Square", "Hexagonal" };

// Constructor
Prefs::Prefs()
{
	// Rendering - Style
	colourScheme_ = Prefs::ElementScheme;
	atomSize_[Atom::StickStyle] = 0.1;      // Only used as a selection radius
	atomSize_[Atom::TubeStyle] = 0.095;
	atomSize_[Atom::SphereStyle] = 0.35;
	atomSize_[Atom::ScaledStyle] = 1.0;     // Used as a general scaling factor for all atoms
	tubeSize_ = 0.1;
	selectionScale_ = 1.5;
	globeSize_ = 75;
	atomDetail_ = 10;
	bondDetail_ = 6;
	perspective_ = TRUE;
	perspectiveFov_ = 20.0;
	spotlightActive_ = TRUE;
	spotlightColour_[Prefs::AmbientComponent][0] = 0.0f;
	spotlightColour_[Prefs::AmbientComponent][1] = 0.0f;
	spotlightColour_[Prefs::AmbientComponent][2] = 0.0f;
	spotlightColour_[Prefs::AmbientComponent][3] = 1.0f;
	spotlightColour_[Prefs::DiffuseComponent][0] = 0.8f;
	spotlightColour_[Prefs::DiffuseComponent][1] = 0.8f;
	spotlightColour_[Prefs::DiffuseComponent][2] = 0.8f;
	spotlightColour_[Prefs::DiffuseComponent][3] = 1.0f;
	spotlightColour_[Prefs::SpecularComponent][0] = 0.7f;
	spotlightColour_[Prefs::SpecularComponent][1] = 0.7f;
	spotlightColour_[Prefs::SpecularComponent][2] = 0.7f;
	spotlightColour_[Prefs::SpecularComponent][3] = 1.0f;
	spotlightPosition_[0] = 1.0f;
	spotlightPosition_[1] = 1.0f;
	spotlightPosition_[2] = 1.0f;

	// GL Options
	glOptions_ = 0;
	shininess_ = 10;
	clipNear_ = 0.5;
	clipFar_ = 2000.0;
	fogNear_ = 1;
	fogFar_ = 200;

	// Rendering - Objects
	labelScale_ = 4.0;
	renderObjects_[Prefs::ViewAtoms] = TRUE;//TGAY
	renderObjects_[Prefs::ViewLabels] = TRUE;
	renderObjects_[Prefs::ViewMeasurements] = TRUE;
	renderObjects_[Prefs::ViewGlobe] = TRUE;
	renderObjects_[Prefs::ViewCell] = TRUE;
	renderObjects_[Prefs::ViewCellAxes] = TRUE;
	renderObjects_[Prefs::ViewCellRepeat] = FALSE;
	renderObjects_[Prefs::ViewRegions] = TRUE;
	renderObjects_[Prefs::ViewForceArrows] = FALSE;
	renderObjects_[Prefs::ViewSurfaces] = TRUE;
	renderStyle_ = Atom::StickStyle;

	// Build
	showGuide_ = FALSE;
	bondTolerance_ = 1.1;
	drawDepth_ = 10.0;
	guideSpacing_ = 1.0;
	guideTicks_ = 5;
	guideExtent_ = 10;
	guideShape_ = Prefs::SquareGuide;
	
	// Input
	mouseAction_[Prefs::LeftButton] = Prefs::InteractAction;
	mouseAction_[Prefs::MiddleButton] = Prefs::TranslateAction;
	mouseAction_[Prefs::RightButton] = Prefs::RotateAction;
	mouseAction_[Prefs::WheelButton] = Prefs::ZoomAction;
	keyAction_[Prefs::ShiftKey] = Prefs::ZrotateKeyAction;
	keyAction_[Prefs::CtrlKey] = Prefs::ManipulateKeyAction;
	keyAction_[Prefs::AltKey] = Prefs::NoKeyAction;

	// Colours
	setPenColour(Prefs::SpecularColour, 0.9f, 0.9f, 0.9f, 1.0f);
	setPenColour(Prefs::ForegroundColour, 0.0f, 0.0f, 0.0f, 1.0f);
	setPenColour(Prefs::BackgroundColour, 1.0f, 1.0f, 1.0f, 1.0f);
	// Colour scale for atom charge colouring (id 0)
	colourScale[0].setRange(-1.0,1.0);
	colourScale[0].setType(ColourScale::ThreePoint);
	colourScale[0].setColour(ColourScale::MinColour, 1.0f, 0.0f, 0.0f);
	colourScale[0].setColour(ColourScale::MidColour, 1.0f, 1.0f, 1.0f);
	colourScale[0].setColour(ColourScale::MaxColour, 0.0f, 0.0f, 1.0f);
	colourScale[1].setRange(-100.0,100.0);
	colourScale[1].setType(ColourScale::ThreePoint);
	colourScale[1].setColour(ColourScale::MinColour, 1.0f, 0.0f, 0.0f);
	colourScale[1].setColour(ColourScale::MidColour, 1.0f, 1.0f, 1.0f);
	colourScale[1].setColour(ColourScale::MaxColour, 1.0f, 0.0f, 0.0f);
	colourScale[2].setRange(-1000.0,1000.0);
	colourScale[2].setType(ColourScale::ThreePoint);
	colourScale[2].setColour(ColourScale::MinColour, 1.0f, 0.0f, 0.0f);
	colourScale[2].setColour(ColourScale::MidColour, 1.0f, 1.0f, 1.0f);
	colourScale[2].setColour(ColourScale::MaxColour, 1.0f, 0.0f, 0.0f);

	// Methods
	modelUpdate_ = 5;
	energyUpdate_ = 1;
	maxRingSize_ = 6;
	replicateFold_ = TRUE;
	replicateTrim_ = TRUE;

	// File
	bondOnLoad_ = Prefs::SwitchAsFilter;
	foldOnLoad_ = Prefs::SwitchAsFilter;
	centreOnLoad_ = Prefs::SwitchAsFilter;
	packOnLoad_ = Prefs::SwitchAsFilter;
	loadAllCoords_ = TRUE;
	cacheLimit_ = 1024;
	zmapType_ = Prefs::AutoZmap;
	coordsInBohr_ = FALSE;
	keepNames_ = FALSE;

	// Energy unit conversion factors to J
	energyConversions_[Prefs::Joules] = 1.0;
	energyConversions_[Prefs::KiloJoules] = 1000.0;
	energyConversions_[Prefs::Calories] = 4.184;
	energyConversions_[Prefs::KiloCalories] = 4184.0;
	energyConversions_[Prefs::ElectronVolts] = 96485.14925;
	energyConversions_[Prefs::Hartree] = 2625494.616;
	if (this == &prefs) setEnergyUnit(Prefs::KiloJoules);
	densityUnit_ = Prefs::GramsPerCm;

	// Energy
	electrostaticsMethod_ = Forms::EwaldAutoElec;
	calculateIntra_ = TRUE;
	calculateVdw_ = TRUE;
	calculateElec_ = FALSE;
	ewaldKvec_.zero();
	ewaldAlpha_ = 0.5;
	ewaldPrecision_ = 5.0E-6;
	vdwCutoff_ = 10.0;
	elecCutoff_ = 10.0;
	vdwScale_ = 1.0;
	validEwaldAuto_ = FALSE;

	// Undo levels
	maxUndoLevels_ = -1;
}

// Load user preferences file
void Prefs::load(const char *filename)
{
	dbgBegin(Debug::Calls,"Prefs::load");
	int success;
	// Open the file
	ifstream prefsfile(filename,ios::in);
	if (!prefsfile.good())
	{
		printf("Couldn't open preferences file in '%s'\n",filename);
		prefsfile.close();
		dbgEnd(Debug::Calls,"Prefs::load");
		return;
	}
	// Create script structure and initialise
	CommandList prefcmds;
	prefcmds.clear();
	while (!prefsfile.eof())
	{
		success = parser.getArgsDelim(&prefsfile,Parser::UseQuotes+Parser::SkipBlanks);
		if (success == 1)
		{
			msg(Debug::None,"prefs::load - Error reading file.\n");
			break;
		}
		else if (success == -1) break;
		// Add script command
		if (!prefcmds.cacheCommand()) break;
	}
	// Check the flowstack - it should contain just the BC_ROOTNODE branch
	if (prefcmds.nBranches() != 1)
	{
		printf("%i unterminated blocks in prefs file.\n",prefcmds.nBranches());
		dbgEnd(Debug::Calls,"prefs::load");
		return;
	}
	prefcmds.execute();
	dbgEnd(Debug::Calls,"prefs::load");
}

/*
// Rendering - View Objects
*/

// Set the visibility of an object
void Prefs::setVisible(ViewObject vo, bool b)
{
	renderObjects_[vo] = b;
}

// Return whether the specified object is visible (i.e. should be rendered)
bool Prefs::shouldRender(ViewObject vo)
{
	return renderObjects_[vo];
}

// Set the drawing style of models
void Prefs::setRenderStyle(Atom::DrawStyle ds)
{
	renderStyle_ = ds;
}

// Return the current drawing style of models
Atom::DrawStyle Prefs::renderStyle()
{
	return renderStyle_;
}

// Set the scale of labels in the model
void Prefs::setLabelScale(double v)
{
	labelScale_ = v;
}

// Return the current label scale
double Prefs::labelScale()
{
	return labelScale_;
}

// Return the current rotation globe size in pixels
int Prefs::globeSize()
{
	return globeSize_;
}

// Set positive repeat cell value
void Prefs::setRepeatCellsPos(int i, int r)
{
	repeatCellsPos_.set(i,r);
}

// Get positive repeat cell value
int Prefs::repeatCellsPos(int i)
{
	return repeatCellsPos_.get(i);
}

// Set negative repeat cell value
void Prefs::setRepeatCellsNeg(int i, int r)
{
	repeatCellsNeg_.set(i,r);
}

// Get negative repeat cell value
int Prefs::repeatCellsNeg(int i)
{
	return repeatCellsNeg_.get(i);
}

double Prefs::screenRadius(Atom *i)
{
	// Simple routine that returns the screen 'radius' of the supplied atom, which depends on its drawing style
	Atom::DrawStyle dstyle;
	renderStyle_ == Atom::IndividualStyle ? dstyle = i->style() : dstyle = renderStyle_;
	return (dstyle == Atom::ScaledStyle) ? (elements.atomicRadius(i) * atomSize_[Atom::ScaledStyle]) : atomSize_[dstyle];
}

/*
// Rendering - Style
*/

// Sets the specified atom size to the given value
void Prefs::setAtomSize(Atom::DrawStyle ds, double f)
{
	atomSize_[(int)ds] = f;
}

// Return the specified atom size
GLdouble Prefs::atomSize(Atom::DrawStyle ds)
{
	return atomSize_[(int)ds];
}

// Sets the tube size in DS_TUBE
void Prefs::setTubeSize(double f)
{
	tubeSize_ = f;
}

// Return the tube size used in DS_TUBE
GLdouble Prefs::tubeSize()
{
	return tubeSize_;
}

// Sets the detail for atom quadrics
void Prefs::setAtomDetail(int n)
{
	atomDetail_ = n;
}

// Return the current detail of atom quadrics
int Prefs::atomDetail()
{
	return atomDetail_;
}

// Sets the detail for bond quadrics
void Prefs::setBondDetail(int n)
{
	bondDetail_ = n;
}

// Return the current detail of bond quadrics
int Prefs::bondDetail()
{
	return bondDetail_;
}

// Sets the scale of selected atoms
void Prefs::setSelectionScale(double f)
{
	selectionScale_ = f;
}

// Return the scale of selected atoms
GLdouble Prefs::selectionScale()
{
	return selectionScale_;
}

// Return whether perspective viewing is enabled
bool Prefs::hasPerspective()
{
	return perspective_;
}

// Sets perspective viewing on/off
void Prefs::setPerspective(bool b)
{
	perspective_ = b;
}

// Set the perspective field of view angle
void Prefs::setPerspectiveFov(double fov)
{
	perspectiveFov_ = fov;
}

// Return the perspective field of view angle
double Prefs::perspectiveFov()
{
	return perspectiveFov_;
}

// Set status of spotlight
void Prefs::setSpotlightActive(bool status)
{
	spotlightActive_ = status;
}

// Return status of spotlight
bool Prefs::spotlightActive()
{
	return spotlightActive_;
}

// Set element of spotlight colour component
void Prefs::setSpotlightColour(Prefs::ColourComponent sc, int i, GLfloat value)
{
	spotlightColour_[sc][i] = value;
}

// Set spotlight colour component
void Prefs::setSpotlightColour(Prefs::ColourComponent sc, GLfloat r, GLfloat g, GLfloat b)
{
	spotlightColour_[sc][0] = r;
	spotlightColour_[sc][1] = g;
	spotlightColour_[sc][2] = b;
}

// Return spotlight colour component
GLfloat *Prefs::spotlightColour(Prefs::ColourComponent sc)
{
	return spotlightColour_[sc];
}

// Set spotlight position
void Prefs::setSpotlightPosition(GLfloat x, GLfloat y, GLfloat z)
{
	spotlightPosition_[0] = x;
	spotlightPosition_[1] = y;
	spotlightPosition_[2] = z;
}

// Set individual element of spotlight position
void Prefs::setSpotlightPosition(int component, GLfloat f)
{
	spotlightPosition_[component] = f;
}

// Return spotlight position
GLfloat *Prefs::spotlightPosition()
{
	return spotlightPosition_;
}

// Set atom colour scheme
void Prefs::setColourScheme(Prefs::ColouringScheme cs)
{
	colourScheme_ = cs;
}

// Return atom colour scheme
Prefs::ColouringScheme Prefs::colourScheme()
{
	return colourScheme_;
}

/*
// GL Options
*/

// Set the bit for the specified option (if it is not set already)
void Prefs::addGlOption(GlOption go)
{
	if (!(glOptions_&go)) glOptions_ += go;
}

// Unsets the bit for the specified option (if it is not unset already)
void Prefs::removeGlOption(GlOption go)
{
	if (glOptions_&go) glOptions_ -= go;
}

// Return whether a given option is set
bool Prefs::hasGlOption(GlOption go)
{
	return (glOptions_&go ? TRUE : FALSE);
}

// Sets the start depth of depth cueing
void Prefs::setFogNnear(int i)
{
	fogNear_ = i;
}

// Return depth cue start depth
GLint Prefs::fogNear()
{
	return fogNear_;
}

// Sets the end depth of depth cueing
void Prefs::setFogFar(int i)
{
	fogFar_ = i;
}

// Return depth cue end depth
GLint Prefs::fogFar()
{
	return fogFar_;
}

// Return the Z depth of the near clipping plane
GLdouble Prefs::clipNear()
{
	return clipNear_;
}

// Return the Z depth of the far clipping plane
GLdouble Prefs::clipFar()
{
	return clipFar_;
}

// Sets the shininess of GL objects
void Prefs::setShininess(int n)
{
	shininess_ = n;
}

// Return the current shininess of GL objects
GLint Prefs::shininess()
{
	return shininess_;
}

/*
// Colours
*/

// Return the specified colour
GLfloat *Prefs::penColour(Colour c)
{
	return penColours_[c];
}

void Prefs::setPenColour(Colour c, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	penColours_[c][0] = r;
	penColours_[c][1] = g;
	penColours_[c][2] = b;
	penColours_[c][3] = a;
}

/*
// Edit Preferences
*/

// Return the bonding tolerance for automatic calculation
double Prefs::bondTolerance()
{
	return bondTolerance_;
}

// Sets the bonding tolerance
void Prefs::setBondTolerance ( double v )
{
	bondTolerance_ = v;
}

// Sets the position of the drawing guide
void Prefs::setDrawDepth ( double v )
{
	drawDepth_ = v;
}

// Return the current position of the drawing guide
double Prefs::drawDepth()
{
	return drawDepth_;
}

// Set guide spacing
void Prefs::setGuideSpacing(double spacing)
{
	guideSpacing_ = spacing;
}

// Spacing of grid on drawing guide
double Prefs::guideSpacing()
{
	return guideSpacing_;
}

// Extent (+- guide_spacing in xy plane) of drawing guide 
void Prefs::setGuideExtent(int extent)
{
	guideExtent_ = extent;
}

// Extent (+- guide_spacing in xy plane) of drawing guide 
int Prefs::guideExtent()
{
	return guideExtent_;
}

// Number of ticks between gridpoints of guide
void Prefs::setGuideTicks(int nticks)
{
	guideTicks_ = nticks;
}

// Number of ticks between gridpoints of guide
int Prefs::guideTicks()
{
	return guideTicks_;
}

// Sets the visibility of the drawing guide
void Prefs::setGuideVisible ( bool b )
{
	showGuide_ = b;
}

//Return whether the draw guide is visible
bool Prefs::isGuideVisible()
{
	return showGuide_;
}

// Sets the shape of the drawing guide
void Prefs::setGuideShape(Prefs::GuideGeometry g)
{
	guideShape_ = g;
}

// Sets the action for the specified mouse button
void Prefs::setMouseAction(Prefs::MouseButton mb, Prefs::MouseAction ma)
{
	mouseAction_[mb] = ma;
}

// Return the action associated with the specified mouse button
Prefs::MouseAction Prefs::mouseAction(Prefs::MouseButton mb)
{
	return mouseAction_[mb];
}

// Sets the modifier key for the specified action
void Prefs::setKeyAction(Prefs::ModifierKey mk, Prefs::KeyAction ka)
{
	keyAction_[mk] = ka;
}

// Return the action associated with the specified keymod button
Prefs::KeyAction Prefs::keyAction(Prefs::ModifierKey mk)
{
	return keyAction_[mk];
}

/*
// File Preferences
*/

// Sets whether to calculate bonding on model load
void Prefs::setBondOnLoad(FilterSwitch s)
{
	bondOnLoad_ = s;
}

// Whether bonding should be recalculated on model load
Prefs::FilterSwitch Prefs::bondOnLoad()
{
	return bondOnLoad_;
}

// Sets whether to centre molecule on load
void Prefs::setCentreOnLoad(Prefs::FilterSwitch s)
{
	centreOnLoad_ = s;
}

// Whether molecule should be centred on model load
Prefs::FilterSwitch Prefs::centreOnLoad()
{
	return centreOnLoad_;
}

// Sets whether to fold atomic positions after model load
void Prefs::setFoldOnLoad(Prefs::FilterSwitch s)
{
	foldOnLoad_ = s;
}

// Whether atoms should be folded after model load
Prefs::FilterSwitch Prefs::foldOnLoad()
{
	return foldOnLoad_;
}

// Sets whether to apply symmetry operators (pack) on load
void Prefs::setPackOnLoad(Prefs::FilterSwitch s)
{
	packOnLoad_ = s;
}

// Whether atoms should be packed (with symmetry operations) after model load
Prefs::FilterSwitch Prefs::packOnLoad()
{
	return packOnLoad_;
}

// Sets whether to load all coordinate sets on model load
void Prefs::setLoadAllCoords(bool b)
{
	loadAllCoords_ = b;
}

// Whether all geometries in a non-trajectory file should be loaded
bool Prefs::loadAllCoords()
{
	return loadAllCoords_;
}

// Set the cache limit (in kb) for trajectory files
void Prefs::setCacheLimit(int i)
{
	cacheLimit_ = i;
}

// Return the cache limit for trajectory files
int Prefs::cacheLimit()
{
	return cacheLimit_;
}

// Sets the style of element conversion to use
void Prefs::setZmapType(Prefs::ZmapType i)
{
	zmapType_ = i;
}

// Return the style of element conversion in use
Prefs::ZmapType Prefs::zmapType()
{
	return zmapType_;
}

// Sets whether to convert coords from Bohr to Angstrom on load
void Prefs::setCoordsInBohr(bool b)
{
	coordsInBohr_ = b;
}

// Whether coordinates should be converted from Bohr to Angstrom
bool Prefs::coordsInBohr()
{
	return coordsInBohr_;
}

// Set whether to keep file type names on load
void Prefs::setKeepNames(bool b)
{
	keepNames_ = b;
}

// Return whether to keep file type names on load
bool Prefs::keepNames()
{
	return keepNames_;
}

/*
// Energy Units
*/

// Return the working energy units
Prefs::EnergyUnit Prefs::energyUnit()
{
	return energyUnit_;
}

// Set the density unit to use
void Prefs::setDensityUnits(Prefs::DensityUnit du)
{
	densityUnit_ = du;
}

// Return the current density units to use
Prefs::DensityUnit Prefs::densityUnit()
{
	return densityUnit_;
}

// Return the electrostastic energy conversion factor
double Prefs::elecConvert()
{
	return elecConvert_;
}

// Set the internal energy units to use
void Prefs::setEnergyUnit(EnergyUnit eu)
{
	// Reconvert any forcefields already loaded so that they are in the new energy units
	EnergyUnit euold = energyUnit_;
	energyUnit_ = eu;
	for (Forcefield *ff = master.forcefields(); ff != NULL; ff = ff->next) ff->convertParameters(euold);
	// Calculate Electrostatic conversion factor
	// COULCONVERT is stored in J/mol. Use this to calculate new elec_convert
	elecConvert_ = COULCONVERT / energyConversions_[energyUnit_];
}

// Convert energy from specified unit to current internal unit
double Prefs::convertEnergy(double energy, EnergyUnit from)
{
	static double result;
	// Convert supplied value to units of J/mol
	result = energy * energyConversions_[from];
	// Then, convert to internal units
	result /= energyConversions_[energyUnit_];
	return result;
}

/*
// Method Preferences
*/

// Set the model update frequency
void Prefs::setModelUpdate(int n)
{
	modelUpdate_ = n;
}

// Return the model update frequency
int Prefs::modelUpdate()
{
	return modelUpdate_;
}

// Set the energy update frequency
void Prefs::setEnergyUpdate(int n)
{
	energyUpdate_ = n;
}

// Return the energy update frequency
int Prefs::energyUpdate()
{
	return energyUpdate_;
}

// Return whether to update the energy, given the cycle number
bool Prefs::shouldUpdateEnergy(int n)
{
	return (n%energyUpdate_ == 0 ? TRUE : FALSE);
}

// Return the maximum ring size allowed
int Prefs::maxRingSize()
{
	return maxRingSize_;
}

// Set whether to fold atoms before replication
void Prefs::setReplicateFold(bool b)
{
	replicateFold_ = b;
}

// Return whether to fold atoms before replication
bool Prefs::replicateFold()
{
	return replicateFold_;
}

// Set whether to trim atoms after replication
void Prefs::setReplicateTrim(bool b)
{
	replicateTrim_ = b;
}

// Return whether to trim atoms after replication
bool Prefs::replicateTrim()
{
	return replicateTrim_;
}

/*
// Expression (general parameters)
*/

// Sets the electrostatic model to use in energy/force calculation
void Prefs::setElectrostaticsMethod(Forms::ElecMethod em)
{
	electrostaticsMethod_ = em;
}

// Return the type of electrostatic treatment to use
Forms::ElecMethod Prefs::electrostaticsMethod()
{
	return electrostaticsMethod_;
}

// Sets whether to calculate intramolecular interactions
void Prefs::setCalculateIntra(bool b)
{
	calculateIntra_ = b;
}

// Return whether to calculate intramolocular interactions
bool Prefs::calculateIntra()
{
	return calculateIntra_;
}

// Sets whether to calculate VDW interactions
void Prefs::setCalculateVdw(bool b)
{
	calculateVdw_ = b;
}

// Return whether to calculate VDW interactions
bool Prefs::calculateVdw()
{
	return calculateVdw_;
}

// Sets whether to calculate electrostatic interactions
void Prefs::setCalculateElec(bool b)
{
	calculateElec_ = b;
}

// Return whether to calculate electrostatic interactions
bool Prefs::calculateElec()
{
	return calculateElec_;
}

// Sets the Ewald k-vector extents
void Prefs::setEwaldKvec(int a, int b, int c)
{
	ewaldKvec_.set(a,b,c);
}

void Prefs::setEwaldKvec(Vec3<int> v)
{
	ewaldKvec_ = v;
}

// Return the Ewald k-vector extents
Vec3<int> Prefs::ewaldKvec()
{
	return ewaldKvec_;
}

// Sets the Ewald precision
void Prefs::setEwaldPrecision(double d)
{
	ewaldPrecision_ = d;
}

// Return the Ewald precision
double Prefs::ewaldPrecision()
{
	return ewaldPrecision_;
}

// Set the Gaussian width to use in the Ewald sum
void Prefs::setEwaldAlpha(double d)
{
	ewaldAlpha_ = d;
}

// Return the Ewald alpha value
double Prefs::ewaldAlpha()
{
	return ewaldAlpha_;
}

// Flag to indicate validity of automatic Ewald params (invalidated on cell change)
bool Prefs::hasValidEwaldAuto()
{
	return validEwaldAuto_;
}

// Flag the Ewald auto params as invalid
void Prefs::invalidateEwaldAuto()
{
	validEwaldAuto_ = FALSE;
}

// Sets the VDW cutoff radius to use
void Prefs::setVdwCutoff(double d)
{
	vdwCutoff_ = d;
}

// Return the VDW cutoff radius
double Prefs::vdwCutoff()
{
	return vdwCutoff_;
}

// Sets the electrostatic cutoff radius to use
void Prefs::setElecCutoff(double d)
{
	elecCutoff_ = d;
}

// Return the electrostatic cutoff radius
double Prefs::elecCutoff()
{
	return elecCutoff_;
}

// Sets the vdw radius scaling factor
void Prefs::setVdwScale(double d)
{
	vdwScale_ = d;
}

// Return the VDW radius scaling factor
double Prefs::vdwScale()
{
	return vdwScale_;
}

/*
// Undo levels
*/

// Set the maximum number of undo levels allowed
void Prefs::setMaxUndoLevels(int n)
{
	maxUndoLevels_ = n;
}

// Return the maximum number of undo levels allowed
int Prefs::maxUndoLevels()
{
	return maxUndoLevels_;
}


