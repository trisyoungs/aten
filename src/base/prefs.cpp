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

PrefsData prefs;

// GL Options
const char *GO_keywords[GO_NITEMS] = { "fog", "linealias", "polyalias", "backcull", "__DUMMY__" };
GlOption GO_from_text(const char *s)
	{ return (GlOption) int(pow(2,enumSearch("GL option",GO_NITEMS,GO_keywords,s))); }

// Mouse buttons
const char *MB_keywords[MB_NITEMS] = { "left", "middle", "right", "wheel" };
MouseButton MB_from_text(const char *s)
	{ return (MouseButton) enumSearch("mouse button",MB_NITEMS,MB_keywords,s); }
const char *text_from_MB(MouseButton i)
	{ return MB_keywords[i]; }

// Mouse actions
const char *MA_strings[MA_NITEMS] = { "None", "Rotate", "Translate", "Interact", "Zoom", "Z-Rotate" };
MouseAction MA_from_text(const char *s)
	{ return (MouseAction) enumSearch("mouse action",MA_NITEMS,MA_strings,s); }
const char *text_from_MA(MouseAction i)
	{ return MA_strings[i]; }
const char **get_MA_strings()
	{ return MA_strings; }

// Key modifiers
const char *MK_strings[MK_NITEMS] = { "Shift", "Ctrl", "Alt" };
const char **get_MK_strings()
	{ return MK_strings; }
ModifierKey MK_from_text(const char *s)
	{ return (ModifierKey) enumSearch("modifier key",MK_NITEMS,MK_strings,s); }
const char *KA_strings[KA_NITEMS] = { "None", "Transform", "ZRotate" };
const char **get_KA_strings()
	{ return KA_strings; }
KeyAction KA_from_text(const char *s)
	{ return (KeyAction) enumSearch("key action",KA_NITEMS,KA_strings,s); }

// Colours
const char *COL_strings[COL_NITEMS] = { "General pen colour", "Background", "Atom Colour Scheme - Lo",
	"Atom Colour Scheme = Mid", "Atom Colour Scheme = Hi", "Specular reflection" };
const char *COL_keywords[COL_NITEMS] = { "pen", "bg", "schemelo", "schememid", "schemehi", "specular" };
const char *text_from_COL(Colour i)
	{ return COL_strings[i]; }
Colour COL_from_text(const char *s)
	{ return (Colour) enumSearch("colour",COL_NITEMS,COL_keywords,s); }

// Density calculation units
const char *DU_strings[DU_NITEMS] = { "g / cm**3", "atoms / A**3" };
const char *DU_keywords[DU_NITEMS] = { "gpercm", "atomsperang" };
const char *text_from_DU(DensityUnit i)
	{ return DU_strings[i]; }
DensityUnit DU_from_text(const char *s)
	{ return (DensityUnit) enumSearch("density units",DU_NITEMS,DU_keywords,s); }

// Energy Units
const char *EU_strings[EU_NITEMS] = { "J/mol", "kJ/mol", "cal/mol", "kcal/mol", "eV/mol", "Ha/mol" };
const char *EU_keywords[EU_NITEMS] = { "j", "kj", "cal", "kcal", "ev", "ha" };
const char *text_from_EU(EnergyUnit i)
	{ return EU_strings[i]; }
EnergyUnit EU_from_text(const char *s)
	{ return (EnergyUnit) enumSearch("energy units",EU_NITEMS,EU_keywords,s); }

// ZMapping types
const char *ZM_keywords[ZM_NITEMS] = { "alpha", "firstalpha", "name", "numeric", "ff", "auto" };
ZmapType ZM_from_text(const char *s)
	{ return (ZmapType) enumSearch("element mapping style",ZM_NITEMS,ZM_keywords,s); }
const char **get_ZM_keywords()
	{ return ZM_keywords; }

// View Objects
const char *VO_keywords[VO_NITEMS] = { "atoms", "cell", "cellaxes", "cellrepeat", "forcearrows", "globe", "labels", "measurements", "regions" };
ViewObject VO_from_text(const char *s)
	{ return (ViewObject) enumSearch("view object",VO_NITEMS,VO_keywords,s); }

// Guide Geometries
const char *GG_strings[GG_NITEMS] = { "Square", "Hexagonal" };
const char **get_GG_strings()
	{ return GG_strings; }

// Constructor
PrefsData::PrefsData()
{
	// Rendering - Style
	colourScheme_ = AC_ELEMENT;
	nScaleSegments_ = 2;
	scaleColours_ = NULL;
	setScaleColours();
	atomSize_[DS_STICK] = 0.1;      // Only used as a selection radius
	atomSize_[DS_TUBE] = 0.095;
	atomSize_[DS_SPHERE] = 0.35;
	atomSize_[DS_SCALED] = 1.0;     // Used as a general scaling factor for all atoms
	tubeSize_ = 0.1;
	selectionScale_ = 1.5;
	globeSize_ = 75;
	atomDetail_ = 10;
	bondDetail_ = 6;
	perspective_ = TRUE;
	perspectiveFov_ = 20.0;
	spotlightActive_ = TRUE;
	spotlightColour_[SL_AMBIENT][0] = 0.0f;
	spotlightColour_[SL_AMBIENT][1] = 0.0f;
	spotlightColour_[SL_AMBIENT][2] = 0.0f;
	spotlightColour_[SL_AMBIENT][3] = 1.0f;
	spotlightColour_[SL_DIFFUSE][0] = 0.8f;
	spotlightColour_[SL_DIFFUSE][1] = 0.8f;
	spotlightColour_[SL_DIFFUSE][2] = 0.8f;
	spotlightColour_[SL_DIFFUSE][3] = 1.0f;
	spotlightColour_[SL_SPECULAR][0] = 0.7f;
	spotlightColour_[SL_SPECULAR][1] = 0.7f;
	spotlightColour_[SL_SPECULAR][2] = 0.7f;
	spotlightColour_[SL_SPECULAR][3] = 1.0f;
	spotlightPosition_[0] = 1.0f;
	spotlightPosition_[1] = 1.0f;
	spotlightPosition_[2] = 1.0f;

	// GL Options
	glOptions_ = 0;
	shininess_ = 10;
	clipNear_ = 1.0;
	clipFar_ = 1000.0;
	fogNear_ = 1;
	fogFar_ = 200;

	// Rendering - Objects
	labelScale_ = 4.0;
	renderObjects_[VO_ATOMS] = TRUE;//TGAY
	renderObjects_[VO_LABELS] = TRUE;
	renderObjects_[VO_MEASUREMENTS] = TRUE;
	renderObjects_[VO_GLOBE] = TRUE;
	renderObjects_[VO_CELL] = TRUE;
	renderObjects_[VO_CELLAXES] = TRUE;
	renderObjects_[VO_CELLREPEAT] = FALSE;
	renderObjects_[VO_REGIONS] = TRUE;
	renderObjects_[VO_FORCEARROWS] = FALSE;
	renderObjects_[VO_SURFACES] = TRUE;
	renderStyle_ = DS_STICK;

	// Build
	showGuide_ = FALSE;
	bondTolerance_ = 1.1;
	drawDepth_ = 0.0;
	guideSpacing_ = 1.0;
	guideTicks_ = 5;
	guideExtent_ = 10;
	guideShape_ = GG_SQUARE;
	
	// Input
	mouseAction_[MB_LEFT] = MA_INTERACT;
	mouseAction_[MB_MIDDLE] = MA_VIEWTRANSLATE;
	mouseAction_[MB_RIGHT] = MA_VIEWROTATE;
	mouseAction_[MB_WHEEL] = MA_VIEWZOOM;
	keyAction_[MK_SHIFT] = KA_ZROTATE;
	keyAction_[MK_CTRL] = KA_MANIPULATE;
	keyAction_[MK_ALT] = KA_NONE;

	// Colours
	setColour(COL_SPECREFLECT, 0.9f, 0.9f, 0.9f, 1.0f);
	setColour(COL_PEN, 0.0f, 0.0f, 0.0f, 1.0f);
	setColour(COL_BG, 1.0f, 1.0f, 1.0f, 1.0f);
	setColour(COL_SCHEMELO, 1.0f, 0.0f, 0.0f, 1.0f);
	setColour(COL_SCHEMEMID, 0.7f, 0.7f, 0.7f, 1.0f);
	setColour(COL_SCHEMEHI, 0.0f, 0.0f, 1.0f, 1.0f);
	colourSchemeLo_[AC_ELEMENT] = 0.0;
	colourSchemeLo_[AC_CHARGE] = -1.0;
	colourSchemeLo_[AC_VELOCITY] = 0.0;
	colourSchemeLo_[AC_FORCE] = 0.0;
	colourSchemeHi_[AC_ELEMENT] = 0.0;
	colourSchemeHi_[AC_CHARGE] = 1.0;
	colourSchemeHi_[AC_VELOCITY] = 200.0;
	colourSchemeHi_[AC_FORCE] = 10000.0;

	// Methods
	modelUpdate_ = 5;
	energyUpdate_ = 1;
	maxRingSize_ = 6;

	// File
	bondOnLoad_ = PS_ASFILTER;
	foldOnLoad_ = PS_ASFILTER;
	centreOnLoad_ = PS_ASFILTER;
	packOnLoad_ = PS_ASFILTER;
	loadAllCoords_ = TRUE;
	cacheLimit_ = 1024;
	zmapType_ = ZM_AUTO;
	coordsInBohr_ = FALSE;

	// Units
	energyConversions_[EU_J] = 1.0;
	energyConversions_[EU_KJ] = 1000.0;
	energyConversions_[EU_CAL] = 4.184;
	energyConversions_[EU_KCAL] = 4184.0;
	energyConversions_[EU_EV] = 96485.14925;
	energyConversions_[EU_HARTREE] = 2625494.616;
	setEnergyUnit(EU_KJ);
	densityUnit_ = DU_GPERCM;

	// Energy
	electrostaticsMethod_ = EM_EWALDAUTO;
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

// Destructor
PrefsData::~PrefsData()
{
	if (scaleColours_ != NULL)
	{
		for (int n=0; n<(3+nScaleSegments_*2); n++) delete[] scaleColours_[n];
		delete[] scaleColours_;
	}
}

// Load user preferences file
void PrefsData::load(const char *filename)
{
	dbgBegin(DM_CALLS,"PrefsData::load");
	int success;
	// Open the file
	ifstream prefsfile(filename,ios::in);
	if (!prefsfile.good())
	{
		printf("Couldn't open preferences file in '%s'\n",filename);
		prefsfile.close();
		dbgEnd(DM_CALLS,"PrefsData::load");
		return;
	}
	// Create script structure and initialise
	CommandList prefcmds;
	prefcmds.clear();
	while (!prefsfile.eof())
	{
		success = parser.getArgsDelim(&prefsfile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(DM_NONE,"prefs::load - Error reading file.\n");
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
		dbgEnd(DM_CALLS,"prefs::load");
		return;
	}
	prefcmds.execute();
	dbgEnd(DM_CALLS,"prefs::load");
}

/*
// Rendering - View Objects
*/

// Set the visibility of an object
void PrefsData::setVisible(ViewObject vo, bool b)
{
	renderObjects_[vo] = b;
}

// Return whether the specified object is visible (i.e. should be rendered)
bool PrefsData::shouldRender(ViewObject vo)
{
	return renderObjects_[vo];
}

// Set the drawing style of models
void PrefsData::setRenderStyle(DrawStyle ds)
{
	renderStyle_ = ds;
}

// Return the current drawing style of models
DrawStyle PrefsData::renderStyle()
{
	return renderStyle_;
}

// Set the scale of labels in the model
void PrefsData::setLabelScale(double v)
{
	labelScale_ = v;
}

// Return the current label scale
double PrefsData::labelScale()
{
	return labelScale_;
}

// Return the current rotation globe size in pixels
int PrefsData::globeSize()
{
	return globeSize_;
}

// Set positive repeat cell value
void PrefsData::setRepeatCellsPos(int i, int r)
{
	repeatCellsPos_.set(i,r);
}

// Get positive repeat cell value
int PrefsData::repeatCellsPos(int i)
{
	return repeatCellsPos_.get(i);
}

// Set negative repeat cell value
void PrefsData::setRepeatCellsNeg(int i, int r)
{
	repeatCellsNeg_.set(i,r);
}

// Get negative repeat cell value
int PrefsData::repeatCellsNeg(int i)
{
	return repeatCellsNeg_.get(i);
}

double PrefsData::screenRadius(Atom *i)
{
	// Simple routine that returns the screen 'radius' of the supplied atom, which depends on its drawing style
	DrawStyle dstyle;
	double radius;
	renderStyle_ == DS_INDIVIDUAL ? dstyle = i->style() : dstyle = renderStyle_;
	return (dstyle == DS_SCALED) ? (elements.atomicRadius(i) * atomSize_[DS_SCALED]) : atomSize_[dstyle];
}

/*
// Rendering - Style
*/

// Sets the specified atom size to the given value
void PrefsData::setAtomSize(DrawStyle ds, double f)
{
	atomSize_[(int)ds] = f;
}

// Return the specified atom size
GLdouble PrefsData::atomSize(DrawStyle ds)
{
	return atomSize_[(int)ds];
}

// Sets the tube size in DS_TUBE
void PrefsData::setTubeSize(double f)
{
	tubeSize_ = f;
}

// Return the tube size used in DS_TUBE
GLdouble PrefsData::tubeSize()
{
	return tubeSize_;
}

// Sets the detail for atom quadrics
void PrefsData::setAtomDetail(int n)
{
	atomDetail_ = n;
}

// Return the current detail of atom quadrics
int PrefsData::atomDetail()
{
	return atomDetail_;
}

// Sets the detail for bond quadrics
void PrefsData::setBondDetail(int n)
{
	bondDetail_ = n;
}

// Return the current detail of bond quadrics
int PrefsData::bondDetail()
{
	return bondDetail_;
}

// Sets the scale of selected atoms
void PrefsData::setSelectionScale(double f)
{
	selectionScale_ = f;
}

// Return the scale of selected atoms
GLdouble PrefsData::selectionScale()
{
	return selectionScale_;
}

// Return whether perspective viewing is enabled
bool PrefsData::hasPerspective()
{
	return perspective_;
}

// Sets perspective viewing on/off
void PrefsData::setPerspective(bool b)
{
	perspective_ = b;
}

// Set the perspective field of view angle
void PrefsData::setPerspectiveFov(double fov)
{
	perspectiveFov_ = fov;
}

// Return the perspective field of view angle
double PrefsData::perspectiveFov()
{
	return perspectiveFov_;
}

// Set status of spotlight
void PrefsData::setSpotlightActive(bool status)
{
	spotlightActive_ = status;
}

// Return status of spotlight
bool PrefsData::spotlightActive()
{
	return spotlightActive_;
}

// Set element of spotlight colour component
void PrefsData::setSpotlightColour(SpotlightComponent sc, int i, GLfloat value)
{
	spotlightColour_[sc][i] = value;
}

// Set spotlight colour component
void PrefsData::setSpotlightColour(SpotlightComponent sc, GLfloat r, GLfloat g, GLfloat b)
{
	spotlightColour_[sc][0] = r;
	spotlightColour_[sc][1] = g;
	spotlightColour_[sc][2] = b;
}

// Return spotlight colour component
GLfloat *PrefsData::spotlightColour(SpotlightComponent sc)
{
	return spotlightColour_[sc];
}

// Set spotlight position
void PrefsData::setSpotlightPosition(GLfloat x, GLfloat y, GLfloat z)
{
	spotlightPosition_[0] = x;
	spotlightPosition_[1] = y;
	spotlightPosition_[2] = z;
}

// Set individual element of spotlight position
void PrefsData::setSpotlightPosition(int component, GLfloat f)
{
	spotlightPosition_[component] = f;
}

// Return spotlight position
GLfloat *PrefsData::spotlightPosition()
{
	return spotlightPosition_;
}

// Set atom colour scheme
void PrefsData::setColourScheme(AtomColours ac)
{
	colourScheme_ = ac;
}

// Return atom colour scheme
AtomColours PrefsData::colourScheme()
{
	return colourScheme_;
}

// Get number of segments in colour scale
int PrefsData::nScaleSegments()
{
	return nScaleSegments_; 
}

// Set number of segments in colour scale
void PrefsData::setScaleSegments(int nsegments)
{
	nScaleSegments_ = nsegments;
	setScaleColours();
}

// Copy colour scale segment into supplied array
void PrefsData::copyScaleColour(int n, GLfloat *v)
{
	// Check range of requested colour
	if ((n < 0) || (n > (3+2*nScaleSegments_))) 
	{
		printf("prefs::get_scale_colour - Requested colour is out of range.\n");
		v[0] = scaleColours_[0][0];
		v[1] = scaleColours_[0][1];
		v[2] = scaleColours_[0][2];
		v[3] = scaleColours_[0][3];
	}
	else
	{
		v[0] = scaleColours_[n][0];
		v[1] = scaleColours_[n][1];
		v[2] = scaleColours_[n][2];
		v[3] = scaleColours_[n][3];
	}
}

// Set colours in colour scale
void PrefsData::setScaleColours()
{
	static int lastnsegments = -1, n;
	static double delta;
	// Check current value of nScaleSegments_ against last value. If different, recreate array
	if (lastnsegments != nScaleSegments_)
	{
		if (scaleColours_ != NULL)
		{
			for (n=0; n<(3+nScaleSegments_*2); n++) delete[] scaleColours_[n];
			delete[] scaleColours_;
		}
		// Create new array
		scaleColours_ = new GLfloat*[3+nScaleSegments_*2];
		for (n=0; n<(3+nScaleSegments_*2); n++) scaleColours_[n] = new GLfloat[4];
		lastnsegments = nScaleSegments_;
	}
	// Set values of lo, mid, and hi colours.
	scaleColours_[0][0] = colours_[COL_SCHEMELO][0];
	scaleColours_[0][1] = colours_[COL_SCHEMELO][1];
	scaleColours_[0][2] = colours_[COL_SCHEMELO][2];
	scaleColours_[0][3] = colours_[COL_SCHEMELO][3];
	scaleColours_[nScaleSegments_+1][0] = colours_[COL_SCHEMEMID][0];
	scaleColours_[nScaleSegments_+1][1] = colours_[COL_SCHEMEMID][1];
	scaleColours_[nScaleSegments_+1][2] = colours_[COL_SCHEMEMID][2];
	scaleColours_[nScaleSegments_+1][3] = colours_[COL_SCHEMEMID][3];
	scaleColours_[nScaleSegments_*2+2][0] = colours_[COL_SCHEMEHI][0];
	scaleColours_[nScaleSegments_*2+2][1] = colours_[COL_SCHEMEHI][1];
	scaleColours_[nScaleSegments_*2+2][2] = colours_[COL_SCHEMEHI][2];
	scaleColours_[nScaleSegments_*2+2][3] = colours_[COL_SCHEMEHI][3];
	// Interpolate between the lo and mid points.
	delta = 1.0 / (nScaleSegments_ + 1);	
	for (n=0; n<nScaleSegments_; n++)
	{
		scaleColours_[n+1][0] = (GLint) (scaleColours_[0][0] + (colours_[COL_SCHEMEMID][0]-scaleColours_[0][0]) * n * delta);
		scaleColours_[n+1][1] = (GLint) (scaleColours_[0][1] + (colours_[COL_SCHEMEMID][1]-scaleColours_[0][1]) * n * delta);
		scaleColours_[n+1][2] = (GLint) (scaleColours_[0][2] + (colours_[COL_SCHEMEMID][2]-scaleColours_[0][2]) * n * delta);
		scaleColours_[n+1][3] = (GLint) (scaleColours_[0][3] + (colours_[COL_SCHEMEMID][3]-scaleColours_[0][3]) * n * delta);
	}
}

/*
// GL Options
*/

// Set the bit for the specified option (if it is not set already)
void PrefsData::addGlOption(GlOption go)
{
	if (!(glOptions_&go)) glOptions_ += go;
}

// Unsets the bit for the specified option (if it is not unset already)
void PrefsData::removeGlOption(GlOption go)
{
	if (glOptions_&go) glOptions_ -= go;
}

// Return whether a given option is set
bool PrefsData::hasGlOption(GlOption go)
{
	return (glOptions_&go ? TRUE : FALSE);
}

// Sets the start depth of depth cueing
void PrefsData::setFogNnear(int i)
{
	fogNear_ = i;
}

// Return depth cue start depth
GLint PrefsData::fogNear()
{
	return fogNear_;
}

// Sets the end depth of depth cueing
void PrefsData::setFogFar(int i)
{
	fogFar_ = i;
}

// Return depth cue end depth
GLint PrefsData::fogFar()
{
	return fogFar_;
}

// Return the Z depth of the near clipping plane
GLdouble PrefsData::clipNear()
{
	return clipNear_;
}

// Return the Z depth of the far clipping plane
GLdouble PrefsData::clipFar()
{
	return clipFar_;
}

// Sets the shininess of GL objects
void PrefsData::setShininess(int n)
{
	shininess_ = n;
}

// Return the current shininess of GL objects
GLint PrefsData::shininess()
{
	return shininess_;
}

/*
// Colours
*/

// Return the specified colour
GLfloat *PrefsData::colour(Colour c)
{ return colours_[c];
}

// Return the low limit for the scheme specified
double PrefsData::colourSchemeLo(int i)
{
	return colourSchemeLo_[i];
}

// Sets the low limit for the scheme specified
void PrefsData::setColourSchemeLo(int i, double d)
{
	colourSchemeLo_[i] = d;
}

// Return the high limit for the scheme specified
double PrefsData::colourSchemeHi(int i)
{
	return colourSchemeHi_[i];
}

// Sets the high limit for the scheme specified
void PrefsData::setColourSchemeHi(int i, double d)
{
	colourSchemeHi_[i] = d;
}

void PrefsData::setColour(Colour c, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	colours_[c][0] = r;
	colours_[c][1] = g;
	colours_[c][2] = b;
	colours_[c][3] = a;
}

/*
// Edit Preferences
*/

// Return the bonding tolerance for automatic calculation
double PrefsData::bondTolerance()
{
	return bondTolerance_;
}

// Sets the bonding tolerance
void PrefsData::setBondTolerance ( double v )
{
	bondTolerance_ = v;
}

// Sets the position of the drawing guide
void PrefsData::setDrawDepth ( double v )
{
	drawDepth_ = v;
}

// Return the current position of the drawing guide
double PrefsData::drawDepth()
{
	return drawDepth_;
}

// Set guide spacing
void PrefsData::setGuideSpacing(double spacing)
{
	guideSpacing_ = spacing;
}

// Spacing of grid on drawing guide
double PrefsData::guideSpacing()
{
	return guideSpacing_;
}

// Extent (+- guide_spacing in xy plane) of drawing guide 
void PrefsData::setGuideExtent(int extent)
{
	guideExtent_ = extent;
}

// Extent (+- guide_spacing in xy plane) of drawing guide 
int PrefsData::guideExtent()
{
	return guideExtent_;
}

// Number of ticks between gridpoints of guide
void PrefsData::setGuideTicks(int nticks)
{
	guideTicks_ = nticks;
}

// Number of ticks between gridpoints of guide
int PrefsData::guideTicks()
{
	return guideTicks_;
}

// Sets the visibility of the drawing guide
void PrefsData::setGuideVisible ( bool b )
{
	showGuide_ = b;
}

//Return whether the draw guide is visible
bool PrefsData::isGuideVisible()
{
	return showGuide_;
}

// Sets the shape of the drawing guide
void PrefsData::setGuideShape(GuideGeometry g)
{
	guideShape_ = g;
}

// Sets the action for the specified mouse button
void PrefsData::setMouseAction(MouseButton mb, MouseAction ma)
{
	mouseAction_[mb] = ma;
}

// Return the action associated with the specified mouse button
MouseAction PrefsData::mouseAction(MouseButton mb)
{
	return mouseAction_[mb];
}

// Sets the modifier key for the specified action
void PrefsData::setKeyAction(ModifierKey mk, KeyAction ka)
{
	keyAction_[mk] = ka;
}

// Return the action associated with the specified keymod button
KeyAction PrefsData::keyAction(ModifierKey mk)
{
	return keyAction_[mk];
}

/*
// File Preferences
*/

// Sets whether to calculate bonding on model load
void PrefsData::setBondOnLoad(PrefSwitch s)
{
	bondOnLoad_ = s;
}

// Whether bonding should be recalculated on model load
PrefSwitch PrefsData::bondOnLoad()
{
	return bondOnLoad_;
}

// Sets whether to centre molecule on load
void PrefsData::setCentreOnLoad(PrefSwitch s)
{
	centreOnLoad_ = s;
}

// Whether molecule should be centred on model load
PrefSwitch PrefsData::centreOnLoad()
{
	return centreOnLoad_;
}

// Sets whether to fold atomic positions after model load
void PrefsData::setFoldOnLoad(PrefSwitch s)
{
	foldOnLoad_ = s;
}

// Whether atoms should be folded after model load
PrefSwitch PrefsData::foldOnLoad()
{
	return foldOnLoad_;
}

// Sets whether to apply symmetry operators (pack) on load
void PrefsData::setPackOnLoad(PrefSwitch s)
{
	packOnLoad_ = s;
}

// Whether atoms should be packed (with symmetry operations) after model load
PrefSwitch PrefsData::packOnLoad()
{
	return packOnLoad_;
}

// Sets whether to load all coordinate sets on model load
void PrefsData::setLoadAllCoords(bool b)
{
	loadAllCoords_ = b;
}

// Whether all geometries in a non-trajectory file should be loaded
bool PrefsData::loadAllCoords()
{
	return loadAllCoords_;
}

// Set the cache limit (in kb) for trajectory files
void PrefsData::setCacheLimit(int i)
{
	cacheLimit_ = i;
}

// Return the cache limit for trajectory files
int PrefsData::cacheLimit()
{
	return cacheLimit_;
}

// Sets the style of element conversion to use
void PrefsData::setZmapType(ZmapType i)
{
	zmapType_ = i;
}

// Return the style of element conversion in use
ZmapType PrefsData::zmapType()
{
	return zmapType_;
}

// Sets whether to convert coords from Bohr to Angstrom on load
void PrefsData::setCoordsInBohr(bool b)
{
	coordsInBohr_ = b;
}

// Whether coordinates should be converted from Bohr to Angstrom
bool PrefsData::coordsInBohr()
{
	return coordsInBohr_;
}

/*
// Energy Units
*/

// Return the working energy units
EnergyUnit PrefsData::energyUnit()
{
	return energyUnit_;
}

// Set the density unit to use
void PrefsData::setDensityUnits(DensityUnit du)
{
	densityUnit_ = du;
}

// Return the current density units to use
DensityUnit PrefsData::densityUnit()
{
	return densityUnit_;
}

// Return the electrostastic energy conversion factor
double PrefsData::elecConvert()
{
	return elecConvert_;
}

// Set the internal energy units to use
void PrefsData::setEnergyUnit(EnergyUnit eu)
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
double PrefsData::convertEnergy(double energy, EnergyUnit from)
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
void PrefsData::setModelUpdate(int n)
{
	modelUpdate_ = n;
}

// Return the model update frequency
int PrefsData::modelUpdate()
{
	return modelUpdate_;
}

// Set the energy update frequency
void PrefsData::setEnergyUpdate(int n)
{
	energyUpdate_ = n;
}

// Return the energy update frequency
int PrefsData::energyUpdate()
{
	return energyUpdate_;
}

// Return whether to update the energy, given the cycle number
bool PrefsData::shouldUpdateEnergy(int n)
{
	return (n%energyUpdate_ == 0 ? TRUE : FALSE);
}

// Return the maximum ring size allowed
int PrefsData::maxRingSize()
{
	return maxRingSize_;
}


/*
// Expression (general parameters)
*/

// Sets the electrostatic model to use in energy/force calculation
void PrefsData::setElectrostaticsMethod(ElecMethod em)
{
	electrostaticsMethod_ = em;
}

// Return the type of electrostatic treatment to use
ElecMethod PrefsData::electrostaticsMethod()
{
	return electrostaticsMethod_;
}

// Sets whether to calculate intramolecular interactions
void PrefsData::setCalculateIntra(bool b)
{
	calculateIntra_ = b;
}

// Return whether to calculate intramolocular interactions
bool PrefsData::calculateIntra()
{
	return calculateIntra_;
}

// Sets whether to calculate VDW interactions
void PrefsData::setCalculateVdw(bool b)
{
	calculateVdw_ = b;
}

// Return whether to calculate VDW interactions
bool PrefsData::calculateVdw()
{
	return calculateVdw_;
}

// Sets whether to calculate electrostatic interactions
void PrefsData::setCalculateElec(bool b)
{
	calculateElec_ = b;
}

// Return whether to calculate electrostatic interactions
bool PrefsData::calculateElec()
{
	return calculateElec_;
}

// Sets the Ewald k-vector extents
void PrefsData::setEwaldKvec(int a, int b, int c)
{
	ewaldKvec_.set(a,b,c);
}

void PrefsData::setEwaldKvec(Vec3<int> v)
{
	ewaldKvec_ = v;
}

// Return the Ewald k-vector extents
Vec3<int> PrefsData::ewaldKvec()
{
	return ewaldKvec_;
}

// Sets the Ewald precision
void PrefsData::setEwaldPrecision(double d)
{
	ewaldPrecision_ = d;
}

// Return the Ewald precision
double PrefsData::ewaldPrecision()
{
	return ewaldPrecision_;
}

// Set the Gaussian width to use in the Ewald sum
void PrefsData::setEwaldAlpha(double d)
{
	ewaldAlpha_ = d;
}

// Return the Ewald alpha value
double PrefsData::ewaldAlpha()
{
	return ewaldAlpha_;
}

// Flag to indicate validity of automatic Ewald params (invalidated on cell change)
bool PrefsData::hasValidEwaldAuto()
{
	return validEwaldAuto_;
}

// Flag the Ewald auto params as invalid
void PrefsData::invalidateEwaldAuto()
{
	validEwaldAuto_ = FALSE;
}

// Sets the VDW cutoff radius to use
void PrefsData::setVdwCutoff(double d)
{
	vdwCutoff_ = d;
}

// Return the VDW cutoff radius
double PrefsData::vdwCutoff()
{
	return vdwCutoff_;
}

// Sets the electrostatic cutoff radius to use
void PrefsData::setElecCutoff(double d)
{
	elecCutoff_ = d;
}

// Return the electrostatic cutoff radius
double PrefsData::elecCutoff()
{
	return elecCutoff_;
}

// Sets the vdw radius scaling factor
void PrefsData::setVdwScale(double d)
{
	vdwScale_ = d;
}

// Return the VDW radius scaling factor
double PrefsData::vdwScale()
{
	return vdwScale_;
}

// Set the charge source for the model
void PrefsData::setChargeSource(ChargeSource cs)
{
	chargeSource_ = cs;
}

// Get the charge source for the model
ChargeSource PrefsData::chargeSource()
{
	return chargeSource_;
}

/*
// Undo levels
*/

// Set the maximum number of undo levels allowed
void PrefsData::setMaxUndoLevels(int n)
{
	maxUndoLevels_ = n;
}

// Return the maximum number of undo levels allowed
int PrefsData::maxUndoLevels()
{
	return maxUndoLevels_;
}


