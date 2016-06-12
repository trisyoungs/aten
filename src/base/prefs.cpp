/*
	*** Preferences Storage
	*** src/base/prefs.cpp
	Copyright T. Youngs 2007-2016

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

#include "base/prefs.h"
#include "base/sysfunc.h"
#include "methods/mc.h"

ATEN_BEGIN_NAMESPACE

// Static singleton
Prefs prefs;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Drawing styles
const char* DrawStyleKeywords[Prefs::nDrawStyles] = { "Line", "Tube", "Sphere", "Scaled", "Own" };
Prefs::DrawStyle Prefs::drawStyle(QString s, bool reportError)
{
	Prefs::DrawStyle ds = (Prefs::DrawStyle) enumSearch("draw style", Prefs::nDrawStyles, DrawStyleKeywords, s, reportError);
	if ((ds == Prefs::nDrawStyles) && reportError) enumPrintValid(Prefs::nDrawStyles,DrawStyleKeywords);
	return ds;
}
const char* Prefs::drawStyle(Prefs::DrawStyle i)
{
	return DrawStyleKeywords[i];
}

// Colour Schemes
const char* ColouringSchemeKeywords[Prefs::nColouringSchemes] = { "Charge", "Element", "Force", "Velocity", "Custom" };
Prefs::ColouringScheme Prefs::colouringScheme(QString s, bool reportError)
{
	Prefs::ColouringScheme cs = (Prefs::ColouringScheme) enumSearch("colour scheme",Prefs::nColouringSchemes,ColouringSchemeKeywords,s);
	if ((cs == Prefs::nColouringSchemes) && reportError) enumPrintValid(Prefs::nColouringSchemes,ColouringSchemeKeywords);
	return cs;
}
const char* Prefs::colouringScheme(ColouringScheme cs)
{
	return ColouringSchemeKeywords[cs];
}

// Mouse buttons
const char* MouseButtonKeywords[Prefs::nMouseButtons] = { "Left", "Middle", "Right", "Wheel" };
Prefs::MouseButton Prefs::mouseButton(QString s, bool reportError)
{
	Prefs::MouseButton mb = (Prefs::MouseButton) enumSearch("mouse button", Prefs::nMouseButtons, MouseButtonKeywords, s);
	if ((mb == Prefs::nMouseButtons) && reportError) enumPrintValid(Prefs::nMouseButtons,MouseButtonKeywords);
	return mb;
}
const char* Prefs::mouseButton(Prefs::MouseButton i)
{
	return MouseButtonKeywords[i];
}

// Mouse actions
const char* MouseActionKeywords[Prefs::nMouseActions] = { "None", "Rotate", "Translate", "Interact", "Zoom", "Z-Rotate" };
Prefs::MouseAction Prefs::mouseAction(QString s, bool reportError)
{
	Prefs::MouseAction ma = (Prefs::MouseAction) enumSearch("mouse action", Prefs::nMouseActions,  MouseActionKeywords, s);
	if ((ma == Prefs::nMouseActions) && reportError) enumPrintValid(Prefs::nMouseActions,MouseActionKeywords);
	return ma;
}
const char* Prefs::mouseAction(Prefs::MouseAction i)
{
	return MouseActionKeywords[i];
}

// Key modifiers
const char* ModifierKeyKeywords[Prefs::nModifierKeys] = { "Shift", "Ctrl", "Alt" };
Prefs::ModifierKey Prefs::modifierKey(QString s, bool reportError)
{
	Prefs::ModifierKey mk = (Prefs::ModifierKey) enumSearch("modifier key", Prefs::nModifierKeys, ModifierKeyKeywords, s);
	if ((mk == Prefs::nModifierKeys) && reportError) enumPrintValid(Prefs::nModifierKeys,ModifierKeyKeywords);
	return mk;
}
const char* Prefs::modifierKey(Prefs::ModifierKey i)
{
	return ModifierKeyKeywords[i];
}

// Key actions
const char* KeyActionKeywords[Prefs::nKeyActions] = { "None", "Transform", "ZRotate" };
Prefs::KeyAction Prefs::keyAction(QString s, bool reportError)
{
	Prefs::KeyAction ka = (Prefs::KeyAction) enumSearch("key action", Prefs::nKeyActions, KeyActionKeywords, s);
	if ((ka == Prefs::nKeyActions) && reportError) enumPrintValid(Prefs::nKeyActions,KeyActionKeywords);
	return ka;
}
const char* Prefs::keyAction(Prefs::KeyAction i)
{
	return KeyActionKeywords[i];
}

// Colours
const char* ObjectColourKeywords[Prefs::nObjectColours] = { "ring", "bg", "fixedatom", "fg", "glyph", "hbond", "specular", "vibrationarrow" };
const char* ObjectColourNames[Prefs::nObjectColours] = { "Aromatic Ring", "Background", "Foreground", "Fixed Atom", "Glyph Default", "Hydrogen Bond", "Specular", "Vibration Arrow" };
Prefs::ObjectColour Prefs::objectColour(QString s, bool reportError)
{
	Prefs::ObjectColour pc = (Prefs::ObjectColour) enumSearch("colour", Prefs::nObjectColours, ObjectColourKeywords, s);
	if ((pc == Prefs::nObjectColours) && reportError) enumPrintValid(Prefs::nObjectColours,ObjectColourKeywords);
	return pc;
}
const char* Prefs::objectColour(Prefs::ObjectColour i)
{
	return ObjectColourKeywords[i];
}
const char* Prefs::objectColourName(Prefs::ObjectColour i)
{
	return ObjectColourNames[i];
}

// Density calculation units
const char* DensityUnitKeywords[Prefs::nDensityUnits] = { "gpercm", "atomsperang" };
const char* Prefs::densityUnit(Prefs::DensityUnit i)
{
	return DensityUnitKeywords[i];
}
Prefs::DensityUnit Prefs::densityUnit(QString s, bool reportError)
{
	Prefs::DensityUnit du = (Prefs::DensityUnit) enumSearch("density unit", Prefs::nDensityUnits, DensityUnitKeywords, s);
	if ((du == Prefs::nDensityUnits) && reportError) enumPrintValid(Prefs::nDensityUnits,DensityUnitKeywords);
	return du;
}

// Energy Units
const char* EnergyUnitFormatted[Prefs::nEnergyUnits] = { "J/mol", "kJ/mol", "cal/mol", "kcal/mol", "K", "eV/mol", "Ha/mol" };
const char* EnergyUnitKeywords[Prefs::nEnergyUnits] = { "j", "kj", "cal", "kcal", "K", "ev", "ha" };
const char* Prefs::energyUnit(Prefs::EnergyUnit i)
{
	return EnergyUnitKeywords[i];
}
Prefs::EnergyUnit Prefs::energyUnit(QString s, bool reportError)
{
	Prefs::EnergyUnit eu = (Prefs::EnergyUnit) enumSearch("energy unit", Prefs::nEnergyUnits, EnergyUnitKeywords, s);
	if ((eu == Prefs::nEnergyUnits) && reportError) enumPrintValid(Prefs::nEnergyUnits,EnergyUnitKeywords);
	return eu;
}

// Guide Geometries
const char* GG_strings[Prefs::nGuideGeometries] = { "Square", "Hexagonal" };

// GUI history types
const char* HistoryTypeKeywords[Prefs::nHistoryTypes] = { "Command", "Script", "Select" };
const char* Prefs::historyType(Prefs::HistoryType i)
{
	return HistoryTypeKeywords[i];
}
Prefs::HistoryType Prefs::historyType(QString s, bool reportError)
{
	Prefs::HistoryType ht = (Prefs::HistoryType) enumSearch("history type", Prefs::nHistoryTypes, HistoryTypeKeywords, s);
	if ((ht == Prefs::nHistoryTypes) && reportError) enumPrintValid(Prefs::nHistoryTypes,HistoryTypeKeywords);
	return ht;
}

// View Lock types
const char* ViewLockKeywords[Prefs::nViewLockTypes] = { "None", "Full" };
const char* Prefs::viewLock(ViewLock vl)
{
	return ViewLockKeywords[vl];
}
Prefs::ViewLock Prefs::viewLock(QString s, bool reportError)
{
	Prefs::ViewLock vl = (Prefs::ViewLock) enumSearch("view lock type", Prefs::nViewLockTypes, ViewLockKeywords, s);
	if ((vl == Prefs::nViewLockTypes) && reportError) enumPrintValid(Prefs::nViewLockTypes, ViewLockKeywords);
	return vl;
}

// Constructor
Prefs::Prefs()
{
	// Rendering - Style
	renderStyle_ = Prefs::SphereStyle;
	colourScheme_ = Prefs::ElementScheme;
	atomStyleRadius_[Prefs::LineStyle] = 0.15;	// Only used as a selection radius
	atomStyleRadius_[Prefs::TubeStyle] = 0.15;
	atomStyleRadius_[Prefs::SphereStyle] = 0.35;
	atomStyleRadius_[Prefs::ScaledStyle] = 1.0;	// Used as a general scaling factor for all atoms
	atomStyleRadius_[Prefs::OwnStyle] = 0.0;
	bondStyleRadius_[Prefs::LineStyle] = 0.1;	// Unused
	bondStyleRadius_[Prefs::TubeStyle] = 0.15;
	bondStyleRadius_[Prefs::SphereStyle] = 0.15;
	bondStyleRadius_[Prefs::ScaledStyle] = 0.15;
	bondStyleRadius_[Prefs::OwnStyle] = 0.0;
	stickLineNormalWidth_ = 2.0;
	stickLineSelectedWidth_ = 4.0;
	selectionScale_ = 1.5;
	perspective_ = true;
	perspectiveFov_ = 20.0;

	// Rendering / Quality Options
	globeSize_ = 75;
	viewRotationGlobe_ = true;
	spotlightActive_ = true;
	spotlightColour_[Prefs::AmbientComponent][0] = 0.0;
	spotlightColour_[Prefs::AmbientComponent][1] = 0.0;
	spotlightColour_[Prefs::AmbientComponent][2] = 0.0;
	spotlightColour_[Prefs::AmbientComponent][3] = 1.0;
	spotlightColour_[Prefs::DiffuseComponent][0] = 0.8;
	spotlightColour_[Prefs::DiffuseComponent][1] = 0.8;
	spotlightColour_[Prefs::DiffuseComponent][2] = 0.8;
	spotlightColour_[Prefs::DiffuseComponent][3] = 1.0;
	spotlightColour_[Prefs::SpecularComponent][0] = 0.7;
	spotlightColour_[Prefs::SpecularComponent][1] = 0.7;
	spotlightColour_[Prefs::SpecularComponent][2] = 0.7;
	spotlightColour_[Prefs::SpecularComponent][3] = 1.0;
	spotlightPosition_[0] = 100.0;
	spotlightPosition_[1] = 100.0;
	spotlightPosition_[2] = 100.0;
	spotlightPosition_[3] = 0.0;
	depthCue_ = false;
	lineAliasing_ = true;
	polygonAliasing_ = false;
	multiSampling_ = true;
	backfaceCulling_ = false;
	shininess_ = 100;
	clipNear_ = 0.5;
	clipFar_ = 2000.0;
	depthNear_ = 1;
	depthFar_ = 200;
	primitiveQuality_ = 10;
	imagePrimitiveQuality_ = 50;
	reusePrimitiveQuality_ = false;

	// Build
	showGuide_ = false;
	bondTolerance_ = 1.15;
	drawDepth_ = -15.0;
	guideSpacing_ = 1.0;
	guideTicks_ = 5;
	guideExtent_ = 10;
	guideShape_ = Prefs::SquareGuide;
	hydrogenDistance_ = 1.08;

	// Interaction Preferences
	mouseAction_[Prefs::LeftButton] = Prefs::InteractAction;
	mouseAction_[Prefs::MiddleButton] = Prefs::TranslateAction;
	mouseAction_[Prefs::RightButton] = Prefs::RotateAction;
	mouseAction_[Prefs::WheelButton] = Prefs::ZoomAction;
	for (int i=0; i<Prefs::nMouseButtons; ++i) mouseActionTexts_[i] = MouseActionKeywords[mouseAction_[i]];
	keyAction_[Prefs::ShiftKey] = Prefs::ZrotateKeyAction;
	keyAction_[Prefs::CtrlKey] = Prefs::ManipulateKeyAction;
	keyAction_[Prefs::AltKey] = Prefs::NoKeyAction;
	for (int i=0; i<Prefs::nModifierKeys; ++i) keyActionTexts_[i] = KeyActionKeywords[keyAction_[i]];
	zoomThrottle_ = 0.15;
	viewLock_ = Prefs::NoLock;

	// Colours
	setColour(Prefs::AromaticRingColour, 0.4, 0.4, 0.7, 1.0);
	setColour(Prefs::BackgroundColour, 1.0, 1.0, 1.0, 1.0);
	setColour(Prefs::FixedAtomColour, 0.0, 0.0, 0.0, 1.0);
	setColour(Prefs::ForegroundColour, 0.0, 0.0, 0.0, 1.0);
	setColour(Prefs::GlyphDefaultColour, 0.0, 0.0, 1.0, 0.7);
	setColour(Prefs::HydrogenBondColour, 0.7, 0.7, 0.7, 1.0);
	setColour(Prefs::SpecularColour, 0.9, 0.9, 0.9, 1.0);
	setColour(Prefs::VibrationArrowColour, 0.8, 0.4, 0.4, 1.0);

	// Colour scales
	colourScale[0].setName("Charge");
	colourScale[0].addPoint(0, -1.0, 1.0f, 0.0f, 0.0f);
	colourScale[0].addPoint(1, 0.0, 1.0f, 1.0f, 1.0f);
	colourScale[0].addPoint(2, 1.0, 0.0f, 0.0f, 1.0f);
	colourScale[1].setName("Velocity");
	colourScale[1].addPoint(0, -100.0, 1.0f, 0.0f, 0.0f);
	colourScale[1].addPoint(1, 0.0, 1.0f, 1.0f, 1.0f);
	colourScale[1].addPoint(2, 100.0, 1.0f, 0.0f, 0.0f);
	colourScale[2].setName("Force");
	colourScale[2].addPoint(0, -1000.0, 1.0f, 0.0f, 0.0f);
	colourScale[2].addPoint(1, 0.0, 1.0f, 1.0f, 1.0f);
	colourScale[2].addPoint(2, 1000.0, 1.0f, 0.0f, 0.0f);

	// General Prefs / Methods
	maxRingSize_ = 6;
	maxRings_ = 100;
	maxCuboids_ = 100;
	forceRhombohedral_ = false;
	augmentAfterRebond_ = true;
	loadPlugins_ = true;
	loadIncludes_ = true;
	loadPartitions_ = true;
	loadFragments_ = true;
	generateFragmentIcons_ = true;
	maxUndoLevels_ = -1;
	loadQtSettings_ = true;
	maxImproperDist_ = 5.0;
	readPipe_ = false;
	allowDialogs_ = false;
	
	// File
	cacheLimit_ = 512000;
	zMapType_ = ElementMap::AutoZMap;

	// Energy unit conversion factors to J
	energyConversions_[Prefs::Joules] = 1.0;
	energyConversions_[Prefs::KiloJoules] = 1000.0;
	energyConversions_[Prefs::Calories] = 4.184;
	energyConversions_[Prefs::KiloCalories] = 4184.0;
	energyConversions_[Prefs::Kelvin] = 1.0 / (503.2166 / 4184.0);
	energyConversions_[Prefs::ElectronVolts] = 96485.14925;
	energyConversions_[Prefs::Hartree] = 2625494.616;
	energyUnit_ = Prefs::KiloJoules;
	autoConversionUnit_ = Prefs::nEnergyUnits;
	setEnergyUnit(Prefs::KiloJoules);
	densityUnit_ = Prefs::GramsPerCm;

	// Energy
	electrostaticsMethod_ = Electrostatics::Coulomb;
	calculateIntra_ = true;
	calculateVdw_ = true;
	ewaldKMax_.set(5,5,5);
	ewaldAlpha_ = 0.5;
	ewaldPrecision_.set(5.0, -6);
	vdwCutoff_ = 50.0;
	elecCutoff_ = 50.0;
	validEwaldAuto_ = false;
	combinationRules_[CombinationRules::ArithmeticRule] = "c = (a+b)*0.5";
	combinationRules_[CombinationRules::GeometricRule] = "c = sqrt(a*b)";
	combinationRules_[CombinationRules::CustomRule1] = "c = a+b";
	combinationRules_[CombinationRules::CustomRule2] = "c = a+b";
	combinationRules_[CombinationRules::CustomRule3] = "c = a+b";
	partitionGridSize_.set(50,50,50);

	// Rendering Options
	distanceLabelFormat_ = "%0.3f ";
	angleLabelFormat_ = "%0.2f";
	chargeLabelFormat_ = "(%0.3f e)";
	labelSize_ = 1.5;
	mouseMoveFilter_ = 10;
	renderDashedAromatics_ = true;
	nModelsPerRow_ = 2;
	drawHydrogenBonds_ = false;
	hydrogenBondDotRadius_ = 0.075;
	viewerFontFileName_ = "";
	messagesFont_.setStyleHint(QFont::TypeWriter);
	messagesFont_.setPixelSize(12);
	correctTransparentGrids_ = false;

	// External programs
#ifdef _WIN32
	tempDir_ = "C:\\";
#else
	tempDir_ = "/tmp";
#endif
}

/*
 * Rendering
 */

// Return whether to draw rotation globe
bool Prefs::viewRotationGlobe()
{
	return viewRotationGlobe_;
}

// Set whether to draw rotation globe
void Prefs::setViewRotationGlobe(bool b)
{
	viewRotationGlobe_ = b;
}

// Set the drawing style of models
void Prefs::setRenderStyle(Prefs::DrawStyle ds)
{
	renderStyle_ = ds;
}

// Return the current drawing style of models
Prefs::DrawStyle Prefs::renderStyle() const
{
	return renderStyle_;
}

// Return the current rotation globe size in pixels
int Prefs::globeSize() const
{
	return globeSize_;
}

// Set the current rotation globe size in pixels
void Prefs::setGlobeSize(int i)
{
	globeSize_ = i;
}

// Set the general primitive detail
void Prefs::setPrimitiveQuality(int n)
{
	primitiveQuality_ = n;
}

// Return the general primitive detail
int Prefs::primitiveQuality() const
{
	return primitiveQuality_;
}

// Set whether to use separate primitive quality for saved images
void Prefs::setReusePrimitiveQuality(bool b)
{
	reusePrimitiveQuality_ = b;
}

// Whether to use separate primitive quality for saved images
bool Prefs::reusePrimitiveQuality() const
{
	return reusePrimitiveQuality_;
}

// Sets the saved image primitive quality
void Prefs::setImagePrimitiveQuality(int n)
{
	imagePrimitiveQuality_ = n;
}

// Return the current save image primitive quality
int Prefs::imagePrimitiveQuality() const
{
	return imagePrimitiveQuality_;
}

// Return styled radius of specified atom
double Prefs::styleRadius(Prefs::DrawStyle ds, int el) const
{
	Prefs::DrawStyle dstyle;
	renderStyle_ == Prefs::OwnStyle ? dstyle = ds : dstyle = renderStyle_;
	return (dstyle == Prefs::ScaledStyle) ? (Elements().atomicRadius(el) * atomStyleRadius_[Prefs::ScaledStyle]) : atomStyleRadius_[dstyle];
}

/*
 * Rendering - Style
 */

// Sets the specified atom size to the given value
void Prefs::setAtomStyleRadius(Prefs::DrawStyle ds, double radius)
{
	atomStyleRadius_[ds] = radius;
}

// Return the specified atom size
GLdouble Prefs::atomStyleRadius(Prefs::DrawStyle ds) const
{
	return atomStyleRadius_[(int)ds];
}

// Return atom radii array
GLdouble* Prefs::atomStyleRadii()
{
	return atomStyleRadius_;
}

// Sets the bond tube radius in the specified rendering style
void Prefs::setBondStyleRadius(Prefs::DrawStyle ds, double radius)
{
	bondStyleRadius_[ds] = radius;
}

// Return bond radius for the specified style
GLdouble Prefs::bondStyleRadius(Prefs::DrawStyle ds) const
{
	return bondStyleRadius_[ds];
}

// Return bond radii array
GLdouble* Prefs::bondStyleRadii()
{
	return bondStyleRadius_;
}

// Sets the scale of selected atoms
void Prefs::setSelectionScale(double f)
{
	selectionScale_ = f;
}

// Return the scale of selected atoms
GLdouble Prefs::selectionScale() const
{
	return selectionScale_;
}

// Return whether perspective viewing is enabled
bool Prefs::hasPerspective() const
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
double Prefs::perspectiveFov() const
{
	return perspectiveFov_;
}

// Set status of spotlight
void Prefs::setSpotlightActive(bool status)
{
	spotlightActive_ = status;
}

// Return status of spotlight
bool Prefs::spotlightActive() const
{
	return spotlightActive_;
}

// Set element of spotlight colour component
void Prefs::setSpotlightColour(Prefs::ColourComponent sc, int i, double value)
{
	spotlightColour_[sc][i] = value;
}

// Set spotlight colour component
void Prefs::setSpotlightColour(Prefs::ColourComponent sc, double r, double g, double b)
{
	spotlightColour_[sc][0] = r;
	spotlightColour_[sc][1] = g;
	spotlightColour_[sc][2] = b;
}

// Return spotlight colour component
double* Prefs::spotlightColour(Prefs::ColourComponent sc)
{
	return spotlightColour_[sc];
}

// Return spotlight colour component in provided array
void Prefs::copySpotlightColour(ColourComponent sc, GLfloat* col)
{
	col[0] = (GLfloat) spotlightColour_[sc][0];
	col[1] = (GLfloat) spotlightColour_[sc][1];
	col[2] = (GLfloat) spotlightColour_[sc][2];
	col[3] = (GLfloat) spotlightColour_[sc][3];
}

// Set spotlight position
void Prefs::setSpotlightPosition(double x, double y, double z)
{
	spotlightPosition_[0] = x;
	spotlightPosition_[1] = y;
	spotlightPosition_[2] = z;
}

// Set individual element of spotlight position
void Prefs::setSpotlightPosition(int component, double f)
{
	spotlightPosition_[component] = f;
}

// Return spotlight position
double* Prefs::spotlightPosition()
{
	return spotlightPosition_;
}

// Return spotlight position in provided array
void Prefs::copySpotlightPosition(GLfloat* col)
{
	col[0] = (GLfloat) spotlightPosition_[0];
	col[1] = (GLfloat) spotlightPosition_[1];
	col[2] = (GLfloat) spotlightPosition_[2];
	col[3] = (GLfloat) spotlightPosition_[3];
}

// Set atom colour scheme
void Prefs::setColourScheme(Prefs::ColouringScheme cs)
{
	colourScheme_ = cs;
}

// Return atom colour scheme
Prefs::ColouringScheme Prefs::colourScheme() const
{
	return colourScheme_;
}

// Set line width for normal stick atoms
void Prefs::setStickLineNormalWidth(double width)
{
	stickLineNormalWidth_ = width;
}

// Return line width for normal stick atoms
double Prefs::stickLineNormalWidth()
{
	return stickLineNormalWidth_;
}

// Set line width for selected stick atoms
void Prefs::setStickLineSelectedWidth(double width)
{
	stickLineSelectedWidth_ = width;
}

// Return line width for selected stick atoms
double Prefs::stickLineSelectedWidth()
{
	return stickLineSelectedWidth_;
}

/*
 * GL Options
 */

// Set status of fog (depth cueing)
void Prefs::setDepthCue(bool status)
{
	depthCue_ = status;
}

// Return status of depth cueing
bool Prefs::depthCue() const
{
	return depthCue_;
}

// Sets the start depth of depth cueing
void Prefs::setDepthNear(int i)
{
	depthNear_ = i;
}

// Return depth cue start depth
GLint Prefs::depthNear() const
{
	return depthNear_;
}

// Sets the end depth of depth cueing
void Prefs::setDepthFar(int i)
{
	depthFar_ = i;
}

// Return depth cue end depth
GLint Prefs::depthFar() const
{
	return depthFar_;
}

// Set status of line aliasing
void Prefs::setLineAliasing(bool status)
{
	lineAliasing_ = status;
}

// Return status of line aliasing
bool Prefs::lineAliasing() const
{
	return lineAliasing_;
}

// Set status of polygon aliasing
void Prefs::setPolygonAliasing(bool status)
{
	polygonAliasing_ = status;
}

// Return status of polygon aliasing
bool Prefs::polygonAliasing() const
{
	return polygonAliasing_;
}

// Set status of polygon aliasing
void Prefs::setMultiSampling(bool status)
{
	multiSampling_ = status;
}

// Return status of polygon aliasing
bool Prefs::multiSampling() const
{
	return multiSampling_;
}

// Set status of backface culling
void Prefs::setBackfaceCulling(bool status)
{
	backfaceCulling_ = status;
}

// Return status of depth cueing
bool Prefs::backfaceCulling() const
{
	return backfaceCulling_;
}

// Return the Z depth of the near clipping plane
GLdouble Prefs::clipNear() const
{
	return clipNear_;
}

// Set the Z-depth of the near clipping plane
void Prefs::setClipNear(double d)
{
	clipNear_ = d;
}

// Return the Z depth of the far clipping plane
GLdouble Prefs::clipFar() const
{
	return clipFar_;
}

// Set the Z-depth of the far clipping plane
void Prefs::setClipFar(double d)
{
	clipFar_ = d;
}

// Sets the shininess of GL objects
void Prefs::setShininess(int n)
{
	if ((n < 0) || (n > 127)) Messenger::print("The option 'shininess' must be an integer between 0 and 127.");
	else shininess_ = n;
}

// Return the current shininess of GL objects
GLint Prefs::shininess() const
{
	return shininess_;
}

/*
 * Colours
 */

// Return the specified colour
double* Prefs::colour(ObjectColour c)
{
	return colours_[c];
}

// Copy the specified colour
void Prefs::copyColour(ObjectColour c, GLfloat* target) const
{
	target[0] = (GLfloat) colours_[c][0];
	target[1] = (GLfloat) colours_[c][1];
	target[2] = (GLfloat) colours_[c][2];
	target[3] = (GLfloat) colours_[c][3];
}

// Copy the specified colour to Vec4<GLfloat>
void Prefs::copyColour(ObjectColour c, Vec4<GLfloat>& col) const
{
	col.x = (GLfloat) colours_[c][0];
	col.y = (GLfloat) colours_[c][1];
	col.z = (GLfloat) colours_[c][2];
	col.w = (GLfloat) colours_[c][3];
}

// Set the specified colour
void Prefs::setColour(ObjectColour c, double r, double g, double b, double a)
{
	colours_[c][0] = r;
	colours_[c][1] = g;
	colours_[c][2] = b;
	colours_[c][3] = a;
}

// Set the supplied element of the specified colour
void Prefs::setColour(ObjectColour c, int i, double value)
{
	if ((i < 0) || (i > 3)) printf("Colour element index out of range (%i)\n", i);
	else colours_[c][i] = value;
}

/*
 * Edit Preferences
 */

// Return the bonding tolerance for automatic calculation
double Prefs::bondTolerance() const
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
double Prefs::drawDepth() const
{
	return drawDepth_;
}

// Set guide spacing
void Prefs::setGuideSpacing(double spacing)
{
	guideSpacing_ = spacing;
}

// Spacing of grid on drawing guide
double Prefs::guideSpacing() const
{
	return guideSpacing_;
}

// Extent (+- guide_spacing in xy plane) of drawing guide 
void Prefs::setGuideExtent(int extent)
{
	guideExtent_ = extent;
}

// Extent (+- guide_spacing in xy plane) of drawing guide 
int Prefs::guideExtent() const
{
	return guideExtent_;
}

// Number of ticks between gridpoints of guide
void Prefs::setGuideTicks(int nticks)
{
	guideTicks_ = nticks;
}

// Number of ticks between gridpoints of guide
int Prefs::guideTicks() const
{
	return guideTicks_;
}

// Sets the visibility of the drawing guide
void Prefs::setGuideVisible ( bool b )
{
	showGuide_ = b;
}

//Return whether the draw guide is visible
bool Prefs::isGuideVisible() const
{
	return showGuide_;
}

// Sets the shape of the drawing guide
void Prefs::setGuideShape(Prefs::GuideGeometry g)
{
	guideShape_ = g;
}

// Set hydrogen add distance
void Prefs::setHydrogenDistance(double d)
{
	hydrogenDistance_ = d;
}

// Return hydrogen add distance
double Prefs::hydrogenDistance() const
{
	return hydrogenDistance_;
}

/*
 * Interaction Preferences
 */

// Sets the action for the specified mouse button
void Prefs::setMouseAction(Prefs::MouseButton mb, Prefs::MouseAction ma)
{
	mouseAction_[mb] = ma;
	mouseActionTexts_[mb] = MouseActionKeywords[ma];
}

// Return the action associated with the specified mouse button
Prefs::MouseAction Prefs::mouseAction(Prefs::MouseButton mb) const
{
	return mouseAction_[mb];
}

// Return array of (derived) mouse action texts
QString* Prefs::mouseActionTexts()
{
	return mouseActionTexts_;
}

// Sets the modifier key for the specified action
void Prefs::setKeyAction(Prefs::ModifierKey mk, Prefs::KeyAction ka)
{
	keyAction_[mk] = ka;
	keyActionTexts_[mk] = KeyActionKeywords[ka];
}

// Return the action associated with the specified keymod button
Prefs::KeyAction Prefs::keyAction(Prefs::ModifierKey mk) const
{
	return keyAction_[mk];
}

// Return array of (derived) key action texts
QString* Prefs::keyActionTexts()
{
	return keyActionTexts_;
}

// Sets the zoom throttle
void Prefs::setZoomThrottle(double throtvalue)
{
	zoomThrottle_ = throtvalue;
}

// Returns the zoom throttle
double Prefs::zoomThrottle() const
{
	return zoomThrottle_;
}

// Set view lock type
void Prefs::setViewLock(ViewLock vl)
{
	viewLock_ = vl;
}

// Return view lock type
Prefs::ViewLock Prefs::viewLock()
{
	return viewLock_;
}

// Set rotation matrix for common view types
void Prefs::setCommonViewMatrix(Matrix mat)
{
	commonViewMatrix_ = mat;
}

// Return rotation matrix for common view types
const Matrix& Prefs::commonViewMatrix()
{
	return commonViewMatrix_;
}

/*
 * File Preferences
 */

// Set the cache limit (in kb) for trajectory files
void Prefs::setCacheLimit(int i)
{
	cacheLimit_ = i;
}

// Return the cache limit for trajectory files
int Prefs::cacheLimit() const
{
	return cacheLimit_;
}

// Sets the style of element conversion to use
void Prefs::setZMapType(ElementMap::ZMapType zmt)
{
	zMapType_ = zmt;
	Messenger::print(Messenger::Verbose, "ZMapping type is now %s", ElementMap::zMapType(zMapType_));
}

// Return the style of element conversion in use
ElementMap::ZMapType Prefs::zMapType() const
{
	return zMapType_;
}

/*
 * Units and Conversion
 */

// Set the density unit to use
void Prefs::setDensityUnit(Prefs::DensityUnit du)
{
	densityUnit_ = du;
}

// Return the current density units to use
Prefs::DensityUnit Prefs::densityUnit() const
{
	return densityUnit_;
}

// Set the internal energy units to use
void Prefs::setEnergyUnit(EnergyUnit eu)
{
	// Store old, and set new energy unit
	Prefs::EnergyUnit oldunit = energyUnit_;
	energyUnit_ = eu;
	// Convert MC acceptance values to new units
	mc.setAcceptanceEnergy(MonteCarlo::Translate, convertEnergy(mc.acceptanceEnergy(MonteCarlo::Translate), oldunit));
	mc.setAcceptanceEnergy(MonteCarlo::Rotate, convertEnergy(mc.acceptanceEnergy(MonteCarlo::Rotate), oldunit));
	mc.setAcceptanceEnergy(MonteCarlo::ZMatrix, convertEnergy(mc.acceptanceEnergy(MonteCarlo::ZMatrix), oldunit));
	mc.setAcceptanceEnergy(MonteCarlo::Insert, convertEnergy(mc.acceptanceEnergy(MonteCarlo::Insert), oldunit));
	mc.setAcceptanceEnergy(MonteCarlo::Delete, convertEnergy(mc.acceptanceEnergy(MonteCarlo::Delete), oldunit));
	// Calculate Electrostatic conversion factor
	// COULCONVERT is stored in J/mol. Use this to calculate new elec_convert
	elecConvert_ = COULCONVERT / energyConversions_[energyUnit_];
}

// Return the working energy units
Prefs::EnergyUnit Prefs::energyUnit() const
{
	return energyUnit_;
}


// Set energy unit to use for automatic conversion of forcefield parameters when accessed through filters
void Prefs::setAutoConversionUnit(Prefs::EnergyUnit eu)
{
	autoConversionUnit_ = eu;
}

// Return energy unit to use for automatic conversion of forcefield parameters when accessed through filters
Prefs::EnergyUnit Prefs::autoConversionUnit() const
{
	return autoConversionUnit_;
}


// Return the electrostastic energy conversion factor
double Prefs::elecConvert() const
{
	return elecConvert_;
}

// Return the gas constant in the current unit of energy
double Prefs::gasConstant() const
{
	return 8.314472 / energyConversions_[energyUnit_];
}

// Convert energy from specified unit to current internal unit
double Prefs::convertEnergy(double energy, EnergyUnit fromUnit, EnergyUnit toUnit) const
{
	double result;
	// Convert supplied value to units of J/mol
	result = energy * energyConversions_[fromUnit];
	// Then, convert to internal units
	result /= energyConversions_[toUnit == Prefs::nEnergyUnits ? energyUnit_ : toUnit];
	return result;
}

/*
 * Method Preferences
 */

// Return the maximum ring size allowed
int Prefs::maxRingSize() const
{
	return maxRingSize_;
}

// Set the maximum ring size allowed
void Prefs::setMaxRingSize(int i)
{
	if (i < 3) Messenger::print("The option 'maxringsize' cannot be set to less than 3.");
	else maxRingSize_ = i;
}

// Return the maximum number of rings to detect per pattern
int Prefs::maxRings() const
{
	return maxRings_;
}

// Set the maximum number of rings to detect per pattern
void Prefs::setMaxRings(int i)
{
	maxRings_ = i;
}

// Return maximum number of bonding cuboids in each direction
int Prefs::maxCuboids() const
{
	return maxCuboids_;
}

// Set maximum number of bonding cuboids in each direction
void Prefs::setMaxCuboids(int i)
{
	maxCuboids_ = i;
}

// Return whether to augment when rebonding
bool Prefs::augmentAfterRebond() const
{
	return augmentAfterRebond_;
}

// Set whether to augment when rebonding
void Prefs::setAugmentAfterRebond(bool b)
{
	augmentAfterRebond_ = b;
}

// Set whether rhombohedral (over hexagonal) spacegroup basis is to be forced
void Prefs::setForceRhombohedral(bool b)
{
	forceRhombohedral_ = b;
}

// Return whether rhombohedral (over hexagonal) spacegroup basis is to be forced
bool Prefs::forceRhombohedral() const
{
	return forceRhombohedral_;
}

// Return whether to load plugins on startup
bool Prefs::loadPlugins() const
{
	return loadPlugins_;
}

// Set whether to load plugins on startup
void Prefs::setLoadPlugins(bool b)
{
	loadPlugins_ = b;
}

// Whether to load includes on startup
bool Prefs::loadIncludes() const
{
	return loadIncludes_;
}

// Set whether to load includes on startup
void Prefs::setLoadIncludes(bool b)
{
	loadIncludes_ = b;
}

// Whether to load partitions on startup
bool Prefs::loadPartitions() const
{
	return loadPartitions_;
}

// Set whether to load partitions on startup
void Prefs::setLoadPartitions(bool b)
{
	loadPartitions_ = b;
}

// Whether to load fragments on startup
bool Prefs::loadFragments() const
{
	return loadFragments_;
}

// Set whether to load fragments on startup
void Prefs::setLoadFragments(bool b)
{
	loadFragments_ = b;
}

// Whether to generate icons for loaded fragments
bool Prefs::generateFragmentIcons() const
{
	return generateFragmentIcons_;
}

// Set whether to generate icons for loaded fragments
void Prefs::setGenerateFragmentIcons(bool b)
{
	generateFragmentIcons_ = b;
}

// Whether to read and execute commands from piped input on startup
bool Prefs::readPipe() const
{
	return readPipe_;
}

// Set whether to read and execute commands from piped input on startup
void Prefs::setReadPipe(bool b)
{
	readPipe_ = b;
}

// Return whether to allow dialogs to be shown if the GUI is not yet active
bool Prefs::allowDialogs()
{
	return allowDialogs_;
}

// Set whether to allow dialogs to be shown if the GUI is not yet active
void Prefs::setAllowDialogs(bool b)
{
	allowDialogs_ = b;
}

// Set the maximum number of undo levels allowed
void Prefs::setMaxUndoLevels(int n)
{
	maxUndoLevels_ = n;
}

// Return the maximum number of undo levels allowed
int Prefs::maxUndoLevels() const
{
	return maxUndoLevels_;
}

// Return whether to load Qt window/toolbar settings on startup
bool Prefs::loadQtSettings()
{
	return loadQtSettings_;
}

// Whether to load Qt window/toolbar settings on startup
void Prefs::setLoadQtSettings(bool b)
{
	loadQtSettings_ = b;
}

// Return maximum distance allowed between consecutive improper torsion atoms
double Prefs::maxImproperDist() const
{
	return maxImproperDist_;
}

// Set maximum distance allowed between consecutive improper torsion atoms
void Prefs::setMaxImproperDist(double r)
{
	maxImproperDist_ = r;
}

/*
// Expression (general parameters)
*/

// Sets the electrostatic model to use in energy/force calculation
void Prefs::setElectrostaticsMethod(Electrostatics::ElecMethod em)
{
	electrostaticsMethod_ = em;
}

// Return the type of electrostatic treatment to use
Electrostatics::ElecMethod Prefs::electrostaticsMethod() const
{
	return electrostaticsMethod_;
}

// Sets whether to calculate intramolecular interactions
void Prefs::setCalculateIntra(bool b)
{
	calculateIntra_ = b;
}

// Return whether to calculate intramolocular interactions
bool Prefs::calculateIntra() const
{
	return calculateIntra_;
}

// Sets whether to calculate VDW interactions
void Prefs::setCalculateVdw(bool b)
{
	calculateVdw_ = b;
}

// Return whether to calculate VDW interactions
bool Prefs::calculateVdw() const
{
	return calculateVdw_;
}

// Sets the Ewald k-vector extents
void Prefs::setEwaldKMax(int element, int i)
{
	ewaldKMax_.set(element,i);
}
void Prefs::setEwaldKMax(int a, int b, int c)
{
	ewaldKMax_.set(a,b,c);
}
void Prefs::setEwaldKMax(Vec3<int> v)
{
	ewaldKMax_ = v;
}

// Return the Ewald k-vector extents
Vec3<int> Prefs::ewaldKMax() const
{
	return ewaldKMax_;
}

// Return the Ewald precision
DoubleExp &Prefs::ewaldPrecision()
{
	return ewaldPrecision_;
}

// Set the Gaussian width to use in the Ewald sum
void Prefs::setEwaldAlpha(double d)
{
	ewaldAlpha_ = d;
}

// Return the Ewald alpha value
double Prefs::ewaldAlpha() const
{
	return ewaldAlpha_;
}

// Flag to indicate validity of automatic Ewald params (invalidated on cell change)
bool Prefs::hasValidEwaldAuto() const
{
	return validEwaldAuto_;
}

// Flag the Ewald auto params as invalid
void Prefs::invalidateEwaldAuto()
{
	validEwaldAuto_ = false;
}

// Sets the VDW cutoff radius to use
void Prefs::setVdwCutoff(double d)
{
	vdwCutoff_ = d;
}

// Return the VDW cutoff radius
double Prefs::vdwCutoff() const
{
	return vdwCutoff_;
}

// Sets the electrostatic cutoff radius to use
void Prefs::setElecCutoff(double d)
{
	elecCutoff_ = d;
}

// Return the electrostatic cutoff radius
double Prefs::elecCutoff() const
{
	return elecCutoff_;
}

// Set combination rule equation
void Prefs::setCombinationRule(CombinationRules::CombinationRule cr, QString rule)
{
	combinationRules_[cr] = rule;
}

// Return combination rule equation
QString Prefs::combinationRule(CombinationRules::CombinationRule cr) const
{
	return combinationRules_[cr];
}

// Return array of combination rule equations
QString* Prefs::combinationRules()
{
	return combinationRules_;
}

// Set grid size for PartitioningSchemes
void Prefs::setPartitionGridSize(Vec3<int> newSize)
{
	partitionGridSize_ = newSize;
}

// Set grid size for PartitioningSchemes (element)
void Prefs::setPartitionGridSize(int element, int value)
{
	partitionGridSize_.set(element, value);
}

// Return grid size for PartitioningSchemes
Vec3<int> Prefs::partitionGridSize()
{
	return partitionGridSize_;
}

/*
// Rendering (and compatibility) Options
*/

// Set C-style format for distance label values
void Prefs::setDistanceLabelFormat(QString cFormat)
{
	distanceLabelFormat_ = cFormat;
}

// Return C-style format for distance label values
QString Prefs::distanceLabelFormat()
{
	return distanceLabelFormat_;
}

// Set C-style format for angle label values
void Prefs::setAngleLabelFormat(QString cFormat)
{
	angleLabelFormat_ = cFormat;
}

// Return C-style format for angle label values
QString Prefs::angleLabelFormat()
{
	return angleLabelFormat_;
}

// Set C-style format for charge label values
void Prefs::setChargeLabelFormat(QString cFormat)
{
	chargeLabelFormat_ = cFormat;
}

// Return C-style format for charge label values
QString Prefs::chargeLabelFormat()
{
	return chargeLabelFormat_;
}

// Set the scale of labels in the model
void Prefs::setLabelSize(double size)
{
	labelSize_ = size;
}

// Return the current label scale
double Prefs::labelSize() const
{
	return labelSize_;
}

// Return whether to use solid or dashed circles for aromatic ring rendering
bool Prefs::renderDashedAromatics()
{
	return renderDashedAromatics_;
}

// Set  whether to use solid or dashed circles for aromatic ring rendering
void Prefs::setRenderDashedAromatics(bool b)
{
	renderDashedAromatics_ = b;
}

// Return mouse move event filter rate
int Prefs::mouseMoveFilter()
{
	return mouseMoveFilter_;
}

// Set mouse move event filter ratio
void Prefs::setMouseMoveFilter(int i)
{
	mouseMoveFilter_ = i;
}

// Return number of models per row when viewing multiple models
int Prefs::nModelsPerRow()
{
	return nModelsPerRow_;
}

// Set number of models per row when viewing multiple models
void Prefs::setNModelsPerRow(int n)
{
	nModelsPerRow_ = n;
}

// Return whether to auto-detect and draw hydrogen bonds
bool Prefs::drawHydrogenBonds()
{
	return drawHydrogenBonds_;
}

// Set whether to auto-detect and draw hydrogen bonds
void Prefs::setDrawHydrogenBonds(bool b)
{
	drawHydrogenBonds_ = b;
}

// Return radius of hydrogen bond dots
double Prefs::hydrogenBondDotRadius()
{
	return hydrogenBondDotRadius_;
}

// Set radius of hydrogen bond dots
void Prefs::setHydrogenBondDotRadius(double r)
{
	hydrogenBondDotRadius_ = r;
}

// Set viewer font filename
void Prefs::setViewerFontFileName(QString fileName)
{
	viewerFontFileName_ = fileName;
}

// Return viewer font filename
QString Prefs::viewerFontFileName()
{
	return viewerFontFileName_;
}

// Set messages font
void Prefs::setMessagesFont(QFont& font)
{
	messagesFont_ = font;
}

// Return messages font
QFont& Prefs::messagesFont()
{
	return messagesFont_;
}

// Set whether to correct grids for transparency artefacts
void Prefs::setCorrectTransparentGrids(bool b)
{
	correctTransparentGrids_ = b;
}

// Return whether to correct grids for transparency artefacts
bool Prefs::correctTransparentGrids()
{
	return correctTransparentGrids_;
}

/*
 * External Programs
 */

// Set temp directory
void Prefs::setTempDir(QDir path)
{
	tempDir_ = path;
}

// Return the temp directory path
QDir Prefs::tempDir() const
{
	return tempDir_;
}

// Location of MOPAC executable
void Prefs::setMopacExe(QString exe)
{
	mopacExe_ = exe;
}

// Return the location of the MOPAC executable
QString Prefs::mopacExe() const
{
	return mopacExe_;
}
