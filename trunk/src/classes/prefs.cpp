/*
	*** Preferences storage
	*** src/base/prefs.cpp
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

#include "classes/prefs.h"
#include "base/sysfunc.h"
#include "base/elements.h"
#include "base/lineparser.h"
#include "main/aten.h"
#include "parser/prefs.h"
#include "ff/forcefield.h"
#include "methods/mc.h"
#include <iostream>

Prefs prefs;

// Colour Schemes
const char *ColouringSchemeKeywords[Prefs::nColouringSchemes] = { "Charge", "Element", "Force", "Velocity", "Custom" };
Prefs::ColouringScheme Prefs::colouringScheme(const char *s, bool reporterror)
{
	Prefs::ColouringScheme cs = (Prefs::ColouringScheme) enumSearch("colour scheme",Prefs::nColouringSchemes,ColouringSchemeKeywords,s);
	if ((cs == Prefs::nColouringSchemes) && reporterror) enumPrintValid(Prefs::nColouringSchemes,ColouringSchemeKeywords);
	return cs;
}
const char *Prefs::colouringScheme(ColouringScheme cs)
{
	return ColouringSchemeKeywords[cs];
}

// Mouse buttons
const char *MouseButtonKeywords[Prefs::nMouseButtons] = { "Left", "Middle", "Right", "Wheel" };
Prefs::MouseButton Prefs::mouseButton(const char *s, bool reporterror)
{
	Prefs::MouseButton mb = (Prefs::MouseButton) enumSearch("mouse button", Prefs::nMouseButtons, MouseButtonKeywords, s);
	if ((mb == Prefs::nMouseButtons) && reporterror) enumPrintValid(Prefs::nMouseButtons,MouseButtonKeywords);
	return mb;
}
const char *Prefs::mouseButton(Prefs::MouseButton i)
{
	return MouseButtonKeywords[i];
}

// Mouse actions
const char *MouseActionKeywords[Prefs::nMouseActions] = { "None", "Rotate", "Translate", "Interact", "Zoom", "Z-Rotate" };
Prefs::MouseAction Prefs::mouseAction(const char *s, bool reporterror)
{
	Prefs::MouseAction ma = (Prefs::MouseAction) enumSearch("mouse action", Prefs::nMouseActions,  MouseActionKeywords, s);
	if ((ma == Prefs::nMouseActions) && reporterror) enumPrintValid(Prefs::nMouseActions,MouseActionKeywords);
	return ma;
}
const char *Prefs::mouseAction(Prefs::MouseAction i)
{
	return MouseActionKeywords[i];
}

// Key modifiers
const char *ModifierKeyKeywords[Prefs::nModifierKeys] = { "Shift", "Ctrl", "Alt" };
Prefs::ModifierKey Prefs::modifierKey(const char *s, bool reporterror)
{
	Prefs::ModifierKey mk = (Prefs::ModifierKey) enumSearch("modifier key", Prefs::nModifierKeys, ModifierKeyKeywords, s);
	if ((mk == Prefs::nModifierKeys) && reporterror) enumPrintValid(Prefs::nModifierKeys,ModifierKeyKeywords);
	return mk;
}
const char *Prefs::modifierKey(Prefs::ModifierKey i)
{
	return ModifierKeyKeywords[i];
}

// Key actions
const char *KeyActionKeywords[Prefs::nKeyActions] = { "None", "Transform", "ZRotate" };
Prefs::KeyAction Prefs::keyAction(const char *s, bool reporterror)
{
	Prefs::KeyAction ka = (Prefs::KeyAction) enumSearch("key action", Prefs::nKeyActions, KeyActionKeywords, s);
	if ((ka == Prefs::nKeyActions) && reporterror) enumPrintValid(Prefs::nKeyActions,KeyActionKeywords);
	return ka;
}
const char *Prefs::keyAction(Prefs::KeyAction i)
{
	return KeyActionKeywords[i];
}

// Colours
const char *ObjectColourKeywords[Prefs::nObjectColours] = { "ring", "bg", "fixedatom", "globeaxes", "globe", "glyph", "specular", "text", "unitcellaxes", "unitcell", "vibrationarrow" };
const char *ObjectColourNames[Prefs::nObjectColours] = { "Aromatic Ring", "Background", "Fixed Atom", "Globe Axes", "Globe", "Glyph Default", "Specular", "Text", "Unit Cell Axes", "Unit Cell", "Vibration Arrow" };
Prefs::ObjectColour Prefs::objectColour(const char *s, bool reporterror)
{
	Prefs::ObjectColour pc = (Prefs::ObjectColour) enumSearch("colour", Prefs::nObjectColours, ObjectColourKeywords, s);
	if ((pc == Prefs::nObjectColours) && reporterror) enumPrintValid(Prefs::nObjectColours,ObjectColourKeywords);
	return pc;
}
const char *Prefs::objectColour(Prefs::ObjectColour i)
{
	return ObjectColourKeywords[i];
}
const char *Prefs::objectColourName(Prefs::ObjectColour i)
{
	return ObjectColourNames[i];
}

// Density calculation units
const char *DensityUnitKeywords[Prefs::nDensityUnits] = { "gpercm", "atomsperang" };
const char *Prefs::densityUnit(Prefs::DensityUnit i)
{
	return DensityUnitKeywords[i];
}
Prefs::DensityUnit Prefs::densityUnit(const char *s, bool reporterror)
{
	Prefs::DensityUnit du = (Prefs::DensityUnit) enumSearch("density unit", Prefs::nDensityUnits, DensityUnitKeywords, s);
	if ((du == Prefs::nDensityUnits) && reporterror) enumPrintValid(Prefs::nDensityUnits,DensityUnitKeywords);
	return du;
}

// Energy Units
const char *EnergyUnitFormatted[Prefs::nEnergyUnits] = { "J/mol", "kJ/mol", "cal/mol", "kcal/mol", "K", "eV/mol", "Ha/mol" };
const char *EnergyUnitKeywords[Prefs::nEnergyUnits] = { "j", "kj", "cal", "kcal", "K", "ev", "ha" };
const char *Prefs::energyUnit(Prefs::EnergyUnit i)
{
	return EnergyUnitKeywords[i];
}
Prefs::EnergyUnit Prefs::energyUnit(const char *s, bool reporterror)
{
	Prefs::EnergyUnit eu = (Prefs::EnergyUnit) enumSearch("energy unit", Prefs::nEnergyUnits, EnergyUnitKeywords, s);
	if ((eu == Prefs::nEnergyUnits) && reporterror) enumPrintValid(Prefs::nEnergyUnits,EnergyUnitKeywords);
	return eu;
}

// Guide Geometries
const char *GG_strings[Prefs::nGuideGeometries] = { "Square", "Hexagonal" };

// Constructor
Prefs::Prefs()
{
	// Rendering - Style
	renderStyle_ = Atom::StickStyle;
	colourScheme_ = Prefs::ElementScheme;
	atomStyleRadius_[Atom::StickStyle] = 0.1;      // Only used as a selection radius
	atomStyleRadius_[Atom::TubeStyle] = 0.15;
	atomStyleRadius_[Atom::SphereStyle] = 0.35;
	atomStyleRadius_[Atom::ScaledStyle] = 1.0;     // Used as a general scaling factor for all atoms
	bondStyleRadius_[Atom::StickStyle] = 0.1;	// Unused
	bondStyleRadius_[Atom::TubeStyle] = 0.15;
	bondStyleRadius_[Atom::SphereStyle] = 0.15;
	bondStyleRadius_[Atom::ScaledStyle] = 0.15;
	selectionScale_ = 1.5;
	perspective_ = TRUE;
	perspectiveFov_ = 20.0;

	// Rendering / Quality Options
	globeSize_ = 75;
	viewRotationGlobe_ = TRUE;
	spotlightActive_ = TRUE;
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
	depthCue_ = FALSE;
	lineAliasing_ = TRUE;
	polygonAliasing_ = FALSE;
	multiSampling_ = TRUE;
	backfaceCulling_ = FALSE;
	instanceType_ = PrimitiveInstance::ListInstance;
	shininess_ = 100;
	clipNear_ = 0.5;
	clipFar_ = 2000.0;
	depthNear_ = 1;
	depthFar_ = 200;
	primitiveQuality_ = 10;
	imagePrimitiveQuality_ = 50;
	reusePrimitiveQuality_ = FALSE;
	levelsOfDetail_ = 8;
	levelOfDetailWidth_ = 25.0;
	levelOfDetailStartZ_ = 25.0;
	transparencyCorrect_ = TRUE;
	transparencyNBins_ = 1000;
	transparencyBinStartZ_ = 0.0;
	transparencyBinWidth_ = 0.2;
	frameCurrentModel_ = TRUE;

	// Build
	showGuide_ = FALSE;
	bondTolerance_ = 1.15;
	drawDepth_ = -15.0;
	guideSpacing_ = 1.0;
	guideTicks_ = 5;
	guideExtent_ = 10;
	guideShape_ = Prefs::SquareGuide;
	hydrogenDistance_ = 1.08;

	// Input
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

	// Colours
	setColour(Prefs::AromaticRingColour, 0.4, 0.4, 0.7, 1.0);
	setColour(Prefs::BackgroundColour, 1.0, 1.0, 1.0, 1.0);
	setColour(Prefs::FixedAtomColour, 0.0, 0.0, 0.0, 1.0);
	setColour(Prefs::GlyphDefaultColour, 0.0, 0.0, 1.0, 0.7);
	setColour(Prefs::GlobeColour, 0.9, 0.9, 0.9, 1.0);
	setColour(Prefs::GlobeAxesColour, 0.5, 0.5, 0.5, 1.0);
	setColour(Prefs::SpecularColour, 0.9, 0.9, 0.9, 1.0);
	setColour(Prefs::TextColour, 0.0, 0.0, 0.0, 1.0);
	setColour(Prefs::UnitCellColour, 0.0, 0.0, 0.0, 1.0);
	setColour(Prefs::UnitCellAxesColour, 0.8, 0.8, 0.8, 1.0);
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
	modelUpdate_ = 5;
	energyUpdate_ = 5;
	maxRingSize_ = 6;
	maxRings_ = 100;
	maxCuboids_ = 100;
	replicateFold_ = TRUE;
	replicateTrim_ = TRUE;
	forceRhombohedral_ = FALSE;
	augmentAfterRebond_ = TRUE;
	warning1056_ = FALSE;
	loadIncludes_ = TRUE;
	loadPartitions_ = TRUE;
	loadFragments_ = TRUE;
	generateFragmentIcons_ = TRUE;
	commonElements_ = "H,C,N,O,Cl";
	maxUndoLevels_ = -1;
	loadQtSettings_ = TRUE;
	maxImproperDist_ = 5.0;
	loadFilters_ = TRUE;
	readPipe_ = FALSE;
	
	// File
	bondOnLoad_ = Choice::Default;
	foldOnLoad_ = Choice::Default;
	centreOnLoad_ = Choice::Default;
	packOnLoad_ = Choice::Default;
	cacheLimit_ = 512000;
	zMapType_ = ElementMap::AutoZMap;
	fixedZMapType_ = FALSE;
	coordsInBohr_ = FALSE;
	keepNames_ = FALSE;
	keepTypes_ = FALSE;
	keepView_ = FALSE;

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
	if (this == &prefs) setEnergyUnit(Prefs::KiloJoules);
	densityUnit_ = Prefs::GramsPerCm;

	// Energy
	electrostaticsMethod_ = Electrostatics::Coulomb;
	calculateIntra_ = TRUE;
	calculateVdw_ = TRUE;
	ewaldKMax_.set(5,5,5);
	ewaldAlpha_ = 0.5;
	ewaldPrecision_.set(5.0, -6);
	vdwCutoff_ = 50.0;
	elecCutoff_ = 50.0;
	vdwScale_ = 1.0;
	validEwaldAuto_ = FALSE;
	combinationRules_[Combine::ArithmeticRule] = "c = (a+b)*0.5";
	combinationRules_[Combine::GeometricRule] = "c = sqrt(a*b)";
	combinationRules_[Combine::CustomRule1] = "c = a+b";
	combinationRules_[Combine::CustomRule2] = "c = a+b";
	combinationRules_[Combine::CustomRule3] = "c = a+b";

	// General Program (including compatibility) options
	useNiceText_ = TRUE;
	distanceLabelFormat_ = "%0.3f ";
	angleLabelFormat_ = "%0.2f";
	labelSize_ = 10;
	manualSwapBuffers_ = FALSE;
	mouseMoveFilter_ = 10;
	useFrameBuffer_ = FALSE;
	renderDashedAromatics_ = TRUE;
	nModelsPerRow_ = 2;

	// External programs
#ifdef _WIN32
	tempDir_ = "C:\\";
#else
	tempDir_ = "/tmp";
#endif
	encoderExe_ = "/usr/bin/mencoder";
	encoderArguments_ = "mf://FILES -ovc x264 -fps FPS -v -o OUTPUT";
}

// Load user preferences file
bool Prefs::load()
{
	msg.enter("Prefs::load");
	Dnchar filename;
	ReturnValue rv;
	bool result, found;
	// Program preferences
	found = FALSE;
	filename.sprintf("%s%c%s%cprefs.dat", aten.homeDir(), PATHSEP, aten.atenDir(), PATHSEP);
	msg.print("Looking for program preferences file '%s'...\n", filename.get());
	if (fileExists(filename)) found = TRUE;
	else
	{
		// Try .txt extension instead
		filename.sprintf("%s%c%s%cprefs.txt", aten.homeDir(), PATHSEP, aten.atenDir(), PATHSEP);
		msg.print("Looking for program preferences file '%s'...\n", filename.get());
		if (fileExists(filename)) found = TRUE;
		
	}
	if (found)
	{
		Program prefscmds;
		result = prefscmds.generateFromFile(filename, "Program Preferences");
		if (result) result = prefscmds.execute(rv);
	}
	else msg.print("Program preferences file not found.\n");
	// User preferences
	found = FALSE;
	filename.sprintf("%s%c%s%cuser.dat", aten.homeDir(), PATHSEP, aten.atenDir(), PATHSEP);
	msg.print("Looking for user preferences file '%s'...\n", filename.get());
	if (fileExists(filename)) found = TRUE;
	else
	{
		// Try .txt extension instead
		filename.sprintf("%s%c%s%cuser.txt", aten.homeDir(), PATHSEP, aten.atenDir(), PATHSEP);
		msg.print("Looking for user preferences file '%s'...\n", filename.get());
		if (fileExists(filename)) found = TRUE;
	}
	if (found)
	{
		Program prefscmds;
		result = prefscmds.generateFromFile(filename, "User Preferences");
		if (result) result = prefscmds.execute(rv);
	}
	else msg.print("User preferences file not found.\n");
	msg.exit("Prefs::load");
	return TRUE;
}

// Save user preferences file
bool Prefs::save(const char *filename)
{
	msg.enter("Prefs::save");
	bool result = TRUE;
	Dnchar line;
	int n, i;
	LineParser prefsfile(filename, TRUE);
	if (prefsfile.isFileGoodForWriting())
	{
		// First - loop over all element data, comparing it to the stored default values
		prefsfile.writeLine("// Element Data\n");
		for (n=0; n<elements().nElements(); ++n)
		{
			// Ambient Colour
			for (i = 0; i<4; ++i) if (elements().defaultEl[n].colour[i] != elements().el[n].colour[i]) break;
			if (i != 4)
			{
				line.sprintf("aten.elements[%s].colour = { %f, %f, %f, %f };\n", elements().el[n].symbol, elements().el[n].colour[0], elements().el[n].colour[1], elements().el[n].colour[2], elements().el[n].colour[3]);
				prefsfile.writeLine(line);
			}
			// Atomic radius
			if (elements().defaultEl[n].atomicRadius != elements().el[n].atomicRadius)
			{
				line.sprintf("aten.elements[%s].radius = %f;\n", elements().el[n].symbol, elements().el[n].atomicRadius);
				prefsfile.writeLine(line);
			}
		}
		// Next - for each accessor in PreferencesVariable compare the results to our local Prefs copy
		prefsfile.writeLine("// Program Preferences\n");
		Prefs defaults;
		ReturnValue rv;
		Dnchar newvalue, defaultvalue;		
		for (i = 0; i < PreferencesVariable::nAccessors; ++i)
		{
			rv.set(VTypes::PreferencesData, this);
			if (!PreferencesVariable::retrieveAccessor(i, rv, FALSE)) continue;
			newvalue = rv.asString();
			rv.set(VTypes::PreferencesData, &defaults);
			if (!PreferencesVariable::retrieveAccessor(i, rv, FALSE)) continue;
			defaultvalue = rv.asString();
			// Compare the two strings - if different, write the prefs value to the file....
// 			printf("acc = %i [%s], default = '%s', new = '%s'\n", i, PreferencesVariable::accessorData[i].name, defaultvalue.get(), newvalue.get());
			if (strcmp(defaultvalue.get(), newvalue.get()) == 0) continue;
			if ((PreferencesVariable::accessorData[i].returnType == VTypes::StringData) && (PreferencesVariable::accessorData[i].arraySize == 0)) line.sprintf("aten.prefs.%s = \"%s\";\n", PreferencesVariable::accessorData[i].name, newvalue.get());
			else line.sprintf("aten.prefs.%s = %s;\n", PreferencesVariable::accessorData[i].name, newvalue.get());
			prefsfile.writeLine(line);
		}
	}
	else result = FALSE;
	prefsfile.closeFile();
	msg.exit("Prefs::save");
	return result;
}

/*
// Rendering
*/

// Return whether to frame current model in view
bool Prefs::frameCurrentModel()
{
	return frameCurrentModel_;
}

// Set whether to frame current model in view
void Prefs::setFrameCurrentModel(bool b)
{
	frameCurrentModel_ = b;
}

// Return whether to frame whole view
bool Prefs::frameWholeView()
{
	return frameWholeView_;
}

// Set whether to frame whole view
void Prefs::setFrameWholeView(bool b)
{
	frameWholeView_ = b;
}

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
void Prefs::setRenderStyle(Atom::DrawStyle ds)
{
	renderStyle_ = ds;
}

// Return the current drawing style of models
Atom::DrawStyle Prefs::renderStyle() const
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

// Set positive repeat cell value
void Prefs::setRepeatCellsPos(int i, int r)
{
	repeatCellsPos_.set(i,r);
}

// Get positive repeat cell value
int Prefs::repeatCellsPos(int i) const
{
	return repeatCellsPos_.get(i);
}

// Set negative repeat cell value
void Prefs::setRepeatCellsNeg(int i, int r)
{
	repeatCellsNeg_.set(i,r);
}

// Get negative repeat cell value
int Prefs::repeatCellsNeg(int i) const
{
	return repeatCellsNeg_.get(i);
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

// Set number of levels of detail for rendering primitives
void Prefs::setLevelsOfDetail(int n)
{
	levelsOfDetail_ = n;
}

// Return number of levels of detail for rendering primitives
int Prefs::levelsOfDetail()
{
	return levelsOfDetail_;
}

// Set level of detail starting z
void Prefs::setLevelOfDetailStartZ(double z)
{
	levelOfDetailStartZ_ = z;
}

// Return level of detail slice 'width'
double Prefs::levelOfDetailStartZ()
{
	return levelOfDetailStartZ_;
}

// Set level of detail slice 'width'
void Prefs::setLevelOfDetailWidth(double width)
{
	levelOfDetailWidth_ = width;
}

// Return level of detail slice 'width'
double Prefs::levelOfDetailWidth()
{
	return levelOfDetailWidth_;
}

// Return whether transparency correction is enabled
bool Prefs::transparencyCorrect()
{
	return transparencyCorrect_;
}

// Return whether transparency correction is enabled
void Prefs::setTransparencyCorrect(bool b)
{
	transparencyCorrect_ = b;
}

// Return number of bins to use in transparency sorting
int Prefs::transparencyNBins()
{
	return transparencyNBins_;
}

// Set number of bins to use in transparency sorting
void Prefs::setTransparencyNBins(int nbins)
{
	transparencyNBins_ = nbins;
}

// Return starting Z-depth of transparency bins
double Prefs::transparencyBinStartZ()
{
	return transparencyBinStartZ_;
}

// Set starting Z-depth of transparency bins
void Prefs::setTransparencyBinStartZ(double startz)
{
	transparencyBinStartZ_ = startz;
}

// Return width of individual transparency Z-bin
double Prefs::transparencyBinWidth()
{
	return transparencyBinWidth_;
}

// Set width of individual transparency Z-bin
void Prefs::setTransparencyBinWidth(double width)
{
	transparencyBinWidth_ = width;
}


// Return styled radius of specified atom
double Prefs::styleRadius(Atom *i) const
{
	Atom::DrawStyle dstyle;
	renderStyle_ == Atom::IndividualStyle ? dstyle = i->style() : dstyle = renderStyle_;
	return (dstyle == Atom::ScaledStyle) ? (elements().atomicRadius(i) * atomStyleRadius_[Atom::ScaledStyle]) : atomStyleRadius_[dstyle];
}

/*
// Rendering - Style
*/

// Sets the specified atom size to the given value
void Prefs::setAtomStyleRadius(Atom::DrawStyle ds, double f)
{
	atomStyleRadius_[ds] = f;
}

// Return the specified atom size
GLdouble Prefs::atomStyleRadius(Atom::DrawStyle ds) const
{
	return atomStyleRadius_[(int)ds];
}

// Return atom radii array
GLdouble *Prefs::atomStyleRadii()
{
	return atomStyleRadius_;
}

// Sets the tube size in DS_TUBE
void Prefs::setBondStyleRadius(Atom::DrawStyle ds, double f)
{
	bondStyleRadius_[ds] = f;
}

// Return bond radius for the specified style
GLdouble Prefs::bondStyleRadius(Atom::DrawStyle ds) const
{
	return bondStyleRadius_[ds];
}

// Return bond radii array
GLdouble *Prefs::bondStyleRadii()
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
double *Prefs::spotlightColour(Prefs::ColourComponent sc)
{
	return spotlightColour_[sc];
}

// Return spotlight colour component in provided array
void Prefs::copySpotlightColour(ColourComponent sc, GLfloat *col)
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
double *Prefs::spotlightPosition()
{
	return spotlightPosition_;
}

// Return spotlight position in provided array
void Prefs::copySpotlightPosition(GLfloat *col)
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

/*
// GL Options
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
	if ((n < 0) || (n > 127)) msg.print("The option 'shininess' must be an integer between 0 and 127.\n");
	else shininess_ = n;
}

// Return the current shininess of GL objects
GLint Prefs::shininess() const
{
	return shininess_;
}

// Set which primitive instance type to use
void Prefs::setInstanceType(PrimitiveInstance::InstanceType type)
{
	instanceType_ = type;
}

// Return which primitive instance type to use
PrimitiveInstance::InstanceType Prefs::instanceType()
{
	return instanceType_;
}

/*
// Colours
*/

// Return the specified colour
double *Prefs::colour(ObjectColour c)
{
	return colours_[c];
}

// Copy the specified colour
void Prefs::copyColour(ObjectColour c, GLfloat *target) const
{
	target[0] = (GLfloat) colours_[c][0];
	target[1] = (GLfloat) colours_[c][1];
	target[2] = (GLfloat) colours_[c][2];
	target[3] = (GLfloat) colours_[c][3];
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
// Edit Preferences
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
// Interaction Preferences
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
Dnchar *Prefs::mouseActionTexts()
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
Dnchar *Prefs::keyActionTexts()
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

/*
// File Preferences
*/

// Sets whether to calculate bonding on model load
void Prefs::setBondOnLoad(Choice s)
{
	bondOnLoad_ = s;
}

// Whether bonding should be recalculated on model load
Choice Prefs::bondOnLoad() const
{
	return bondOnLoad_;
}

// Sets whether to centre molecule on load
void Prefs::setCentreOnLoad(Choice s)
{
	centreOnLoad_ = s;
}

// Whether molecule should be centred on model load
Choice Prefs::centreOnLoad() const
{
	return centreOnLoad_;
}

// Sets whether to fold atomic positions after model load
void Prefs::setFoldOnLoad(Choice s)
{
	foldOnLoad_ = s;
}

// Whether atoms should be folded after model load
Choice Prefs::foldOnLoad() const
{
	return foldOnLoad_;
}

// Sets whether to apply symmetry operators (pack) on load
void Prefs::setPackOnLoad(Choice s)
{
	packOnLoad_ = s;
}

// Whether atoms should be packed (with symmetry operations) after model load
Choice Prefs::packOnLoad() const
{
	return packOnLoad_;
}

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
void Prefs::setZMapType(ElementMap::ZMapType i)
{
	if (fixedZMapType_) msg.print("Ignored change of ZMapping type to '%s' since it is currently fixed to '%s'.\n", ElementMap::zMapType(i), ElementMap::zMapType(zMapType_));
	else zMapType_ = i;
	msg.print(Messenger::Verbose, "ZMapping type is now %s\n", ElementMap::zMapType(zMapType_));
}


// Sets the style of element conversion to use, and the fixed status of the mapping type
void Prefs::setZMapType(ElementMap::ZMapType i, bool fixed)
{
	fixedZMapType_ = fixed;
	zMapType_ = i;
}

// Return the style of element conversion in use
ElementMap::ZMapType Prefs::zMapType() const
{
	return zMapType_;
}

// Sets whether to convert coords from Bohr to Angstrom on load
void Prefs::setCoordsInBohr(bool b)
{
	coordsInBohr_ = b;
}

// Whether coordinates should be converted from Bohr to Angstrom
bool Prefs::coordsInBohr() const
{
	return coordsInBohr_;
}

// Set whether to keep file type names on load
void Prefs::setKeepNames(bool b)
{
	keepNames_ = b;
}

// Return whether to keep file type names on load
bool Prefs::keepNames() const
{
	return keepNames_;
}

// Set whether to assign and fix type names on load
void Prefs::setKeepTypes(bool b)
{
	keepTypes_ = b;
}

// Return whether to assign and fix type names on load
bool Prefs::keepTypes() const
{
	return keepTypes_;
}

// Set whether to keep view on GUI start
void Prefs::setKeepView(bool b)
{
	keepView_ = b;
}

// Return whether to keep view on GUI start
bool Prefs::keepView() const
{
	return keepView_;
}

/*
// Units and Conversion
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
	// Loop over stored forcefields and convert energetic parameters
	for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next) ff->convertParameters();
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
// Method Preferences
*/

// Set the model update frequency
void Prefs::setModelUpdate(int n)
{
	modelUpdate_ = n;
}

// Return the model update frequency
int Prefs::modelUpdate() const
{
	return modelUpdate_;
}

// Return whether to update the energy, given the cycle number
bool Prefs::shouldUpdateModel(int n)
{
	if (n == 1) return FALSE;
	else return (n%modelUpdate_ == 0 ? TRUE : FALSE);
}

// Set the energy update frequency
void Prefs::setEnergyUpdate(int n)
{
	energyUpdate_ = n;
}

// Return the energy update frequency
int Prefs::energyUpdate() const
{
	return energyUpdate_;
}

// Return whether to update the energy, given the cycle number
bool Prefs::shouldUpdateEnergy(int n)
{
	if (n == 1) return FALSE;
	else return (n%energyUpdate_ == 0 ? TRUE : FALSE);
}

// Return the maximum ring size allowed
int Prefs::maxRingSize() const
{
	return maxRingSize_;
}

// Set the maximum ring size allowed
void Prefs::setMaxRingSize(int i)
{
	if (i < 3) msg.print("The option 'maxringsize' cannot be set to less than 3.\n");
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

// Set whether to fold atoms before replication
void Prefs::setReplicateFold(bool b)
{
	replicateFold_ = b;
}

// Return whether to fold atoms before replication
bool Prefs::replicateFold() const
{
	return replicateFold_;
}

// Set whether to trim atoms after replication
void Prefs::setReplicateTrim(bool b)
{
	replicateTrim_ = b;
}

// Return whether to trim atoms after replication
bool Prefs::replicateTrim() const
{
	return replicateTrim_;
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

// Whether to display warning for version 1.5 (r1056) newness
bool Prefs::warning1056() const
{
	return warning1056_;
}

// Set whether to display warning for version 1.5 (r1056) newness
void Prefs::setWarning1056(bool b)
{
	warning1056_ = b;
}

// Whether to load filters on startup
bool Prefs::loadFilters() const
{
	return loadFilters_;
}

// Set whether to load filters on startup
void Prefs::setLoadFilters(bool b)
{
	loadFilters_ = b;
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

// Set list of common elements in SelectElement dialog
void Prefs::setCommonElements(const char *s)
{
	commonElements_ = s;
}

// Return list of common elements to use in SelectElement dialog
const char *Prefs::commonElements() const
{
	return commonElements_.get();
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
	validEwaldAuto_ = FALSE;
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

// Sets the vdw radius scaling factor
void Prefs::setVdwScale(double d)
{
	vdwScale_ = d;
}

// Return the VDW radius scaling factor
double Prefs::vdwScale() const
{
	return vdwScale_;
}

// Set combination rule equation
void Prefs::setCombinationRule(Combine::CombinationRule cr, const char *s)
{
	combinationRules_[cr] = s;
}

// Return combination rule equation
const char *Prefs::combinationRule(Combine::CombinationRule cr) const
{
	return combinationRules_[cr].get();
}

// Return array of combination rule equations
Dnchar *Prefs::combinationRules()
{
	return combinationRules_;
}

/*
// Rendering (and compatibility) Options
*/

// Set C-style format for distance label values
void Prefs::setDistanceLabelFormat(const char *format)
{
	distanceLabelFormat_ = format;
}

// Return C-style format for distance label values
const char *Prefs::distanceLabelFormat()
{
	return distanceLabelFormat_.get();
}

// Set C-style format for angle label values
void Prefs::setAngleLabelFormat(const char *format)
{
	angleLabelFormat_ = format;
}

// Return C-style format for angle label values
const char *Prefs::angleLabelFormat()
{
	return angleLabelFormat_.get();
}

// Set the scale of labels in the model
void Prefs::setLabelSize(int size)
{
	labelSize_ = size;
}

// Return the current label scale
int Prefs::labelSize() const
{
	return labelSize_;
}

// Set whether to use nice text rendering
void Prefs::setUseNiceText(bool b)
{
	useNiceText_ = b;
}

// Return whether to use nice text rendering
bool Prefs::useNiceText() const
{
	return useNiceText_;
}

// Set manual swapbuffers
void Prefs::setManualSwapBuffers(bool on)
{
	manualSwapBuffers_ = on;
}

// Return whether manual buffer swapping is enabled
bool Prefs::manualSwapBuffers() const
{
	return manualSwapBuffers_;
}

// Return whether manual buffer swapping is enabled
bool Prefs::useFrameBuffer() const
{
	return useFrameBuffer_;
}

// Set manual swapbuffers
void Prefs::setUseFrameBuffer(bool on)
{
	useFrameBuffer_ = on;
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

/*
// External Programs
*/

// Set temp directory
void Prefs::setTempDir(const char *path)
{
	tempDir_ = path;
}

// Return the temp directory path
const char *Prefs::tempDir() const
{
	return tempDir_.get();
}

// Location of MOPAC executable
void Prefs::setMopacExe(const char *exe)
{
	mopacExe_ = exe;
}

// Return the location of the MOPAC executable
const char *Prefs::mopacExe() const
{
	return mopacExe_.get();
}

// Encoder command
void Prefs::setEncoderExe(const char *exe)
{
	encoderExe_ = exe;
}

// Return encoder command
const char *Prefs::encoderExe() const
{
	return encoderExe_.get();
}

// Encoder arguments
void Prefs::setEncoderArguments(const char *arguments)
{
	encoderArguments_ = arguments;
}

// Return encoder arguments
const char *Prefs::encoderArguments() const
{
	return encoderArguments_.get();
}

// Video encoder post-process command
void Prefs::setEncoderPostExe(const char *exe)
{
	encoderPostExe_ = exe;
}

// Return the video encoder post-process command
const char *Prefs::encoderPostExe() const
{
	return encoderPostExe_.get();
}

// Video encoder post-process arguments
void Prefs::setEncoderPostArguments(const char *arguments)
{
	encoderPostArguments_ = arguments;
}

// Return the video encoder post-process arguments
const char *Prefs::encoderPostArguments() const
{
	return encoderPostArguments_.get();
}
