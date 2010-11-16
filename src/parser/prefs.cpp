/*
	*** Prefs Variable
	*** src/parser/prefs.cpp
	Copyright T. Youngs 2007-2010

	This file is part of Aten.

	Prefs is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Prefs is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Prefs.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "parser/prefs.h"
#include "parser/stepnode.h"
#include "base/constants.h"
#include "classes/prefs.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "gui/gui.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
PreferencesVariable::PreferencesVariable()
{
	// Private variables
	returnType_ = VTypes::PreferencesData;
	readOnly_ = TRUE;
}

// Destructor
PreferencesVariable::~PreferencesVariable()
{
}

/*
// Accessors
*/

// Accessor data - name, type, arraysize, ro?
Accessor PreferencesVariable::accessorData[PreferencesVariable::nAccessors] = {
	{ "anglelabel",		VTypes::StringData,	0, FALSE },
	{ "atomdetail"	,	VTypes::DoubleData,	0, FALSE },
	{ "atomstyleradius",	VTypes::DoubleData,	Atom::nDrawStyles, FALSE },
	{ "backcull",		VTypes::IntegerData,	0, FALSE },
	{ "backgroundcolour",	VTypes::DoubleData,	4, FALSE },
	{ "bonddetail"	,	VTypes::DoubleData,	0, FALSE },
	{ "bondstyleradius",	VTypes::DoubleData,	Atom::nDrawStyles, FALSE },
	{ "bondtolerance",	VTypes::DoubleData,	0, FALSE },
	{ "cachelimit",		VTypes::IntegerData,	0, FALSE },
	{ "calculateelec",	VTypes::IntegerData,	0, FALSE },
	{ "calculateintra",	VTypes::IntegerData,	0, FALSE },
	{ "calculatevdw",	VTypes::IntegerData,	0, FALSE },
	{ "clipfar",		VTypes::DoubleData,	0, FALSE },
	{ "clipnear",		VTypes::DoubleData,	0, FALSE },
	{ "colourscheme",	VTypes::StringData,	0, FALSE },
	{ "combinationrule",	VTypes::StringData,	Combine::nCombinationRules, FALSE },
	{ "commonelements",	VTypes::StringData,	0, FALSE },
	{ "densityunit",	VTypes::StringData,	0, FALSE },
	{ "depthcue",		VTypes::IntegerData,	0, FALSE },
	{ "depthfar",		VTypes::IntegerData,	0, FALSE },
	{ "depthnear",		VTypes::IntegerData,	0, FALSE },
	{ "distancelabel",	VTypes::StringData,	0, FALSE },
	{ "eleccutoff",		VTypes::DoubleData,	0, FALSE },
	{ "elecmethod",		VTypes::StringData,	0, FALSE },
	{ "energyunit",		VTypes::StringData,	0, FALSE },
	{ "energyupdate",	VTypes::IntegerData,	0, FALSE },
	{ "ewaldalpha",		VTypes::DoubleData,	0, FALSE },
	{ "ewaldkmax",		VTypes::IntegerData,	3, FALSE },
	{ "ewaldprecision",	VTypes::DoubleData,	0, FALSE },
	{ "forcerhombohedral",	VTypes::IntegerData,	0, FALSE },
	{ "foregroundcolour",	VTypes::DoubleData,	4, FALSE },
	{ "globesize",		VTypes::IntegerData,	0, FALSE },
	{ "glyphcolour",	VTypes::DoubleData,	4, FALSE },
	{ "hdistance",		VTypes::DoubleData,	0, FALSE },
	{ "keyaction",		VTypes::StringData,	Prefs::nModifierKeys, FALSE },
	{ "labelsize",		VTypes::IntegerData,	0, FALSE },
	{ "linealiasing",	VTypes::IntegerData,	0, FALSE },
	{ "manualswapbuffers",	VTypes::IntegerData,	0, FALSE },
	{ "maxcuboids",		VTypes::IntegerData,	0, FALSE },
	{ "maxrings",		VTypes::IntegerData,	0, FALSE },
	{ "maxringsize",	VTypes::IntegerData,	0, FALSE },
	{ "maxundo",		VTypes::IntegerData,	0, FALSE },
	{ "modelupdate",	VTypes::IntegerData,	0, FALSE },
	{ "mopacexe",		VTypes::StringData,	0, FALSE },
	{ "mouseaction",	VTypes::StringData,	Prefs::nMouseButtons, FALSE },
	{ "multisampling",	VTypes::IntegerData,	0, FALSE },
	{ "noqtsettings",	VTypes::IntegerData,	0, FALSE },
	{ "offscreenobjects",	VTypes::IntegerData,	0, FALSE },
	{ "perspective"	,	VTypes::IntegerData,	0, FALSE },
	{ "perspectivefov",	VTypes::DoubleData,	0, FALSE },
	{ "polygonaliasing",	VTypes::IntegerData,	0, FALSE },
	{ "renderstyle",	VTypes::StringData,	0, FALSE },
	{ "replicatefold",	VTypes::IntegerData,	0, FALSE },
	{ "replicatetrim",	VTypes::IntegerData,	0, FALSE },
	{ "screenobjects",	VTypes::IntegerData,	0, FALSE },
	{ "selectionscale",	VTypes::DoubleData,	0, FALSE },
	{ "shininess",		VTypes::IntegerData,	0, FALSE },
	{ "specularcolour",	VTypes::DoubleData,	4, FALSE },
	{ "spotlight",		VTypes::IntegerData,	0, FALSE },
	{ "spotlightambient",	VTypes::DoubleData,	4, FALSE },
	{ "spotlightdiffuse",	VTypes::DoubleData,	4, FALSE },
	{ "spotlightposition",	VTypes::DoubleData,	4, FALSE },
	{ "spotlightspecular",	VTypes::DoubleData,	4, FALSE },
	{ "tempdir",		VTypes::StringData,	0, FALSE },
	{ "useframebuffer",	VTypes::IntegerData,	0, FALSE },
	{ "usenicetext",	VTypes::IntegerData,	0, FALSE },
	{ "vdwcutoff",		VTypes::DoubleData,	0, FALSE },
	{ "vdwscale",		VTypes::DoubleData,	0, FALSE },
	{ "warn1056",		VTypes::IntegerData,	0, FALSE },
	{ "zmap",		VTypes::StringData,	0, FALSE },
	{ "zoomthrottle",	VTypes::DoubleData,	0, FALSE }
};

// Function data
FunctionAccessor PreferencesVariable::functionData[PreferencesVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *PreferencesVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return PreferencesVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *PreferencesVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("PreferencesVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'prefs&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("PreferencesVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'prefs&' function '%s'.\n", s);
			msg.exit("PreferencesVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::PreferencesData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'prefs&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
		{
			msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
			result = NULL;
		}
		else result = new StepNode(i, VTypes::PreferencesData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("PreferencesVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool PreferencesVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PreferencesVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Prefs type.\n", i);
		msg.exit("PreferencesVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("PreferencesVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("PreferencesVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	bool result;
	Prefs *ptr = (Prefs*) rv.asPointer(VTypes::PreferencesData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::PreferencesData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (PreferencesVariable::AngleLabel):
			rv.set( ptr->angleLabel() );
			break;
		case (PreferencesVariable::AtomDetail):
			rv.set( (int) ptr->atomDetail() );
			break;
		case (PreferencesVariable::AtomStyleRadius):
			if (hasArrayIndex) rv.set(ptr->atomStyleRadius( (Atom::DrawStyle) (arrayIndex-1)) );
			else rv.setArray( VTypes::DoubleData, &ptr->atomStyleRadius_, Atom::nDrawStyles);
			break;
		case (PreferencesVariable::BackCull):
			rv.set( ptr->backfaceCulling() );
			break;
		case (PreferencesVariable::BackgroundColour):
			if (hasArrayIndex) rv.set( ptr->colour(Prefs::BackgroundColour)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(Prefs::BackgroundColour), 4);
			break;
		case (PreferencesVariable::BondDetail):
			rv.set( (int) ptr->bondDetail() );
			break;
		case (PreferencesVariable::BondStyleRadius):
			if (hasArrayIndex) rv.set(ptr->bondStyleRadius( (Atom::DrawStyle) (arrayIndex-1)) );
			else rv.setArray( VTypes::DoubleData, &ptr->bondStyleRadius_, Atom::nDrawStyles);
			break;
		case (PreferencesVariable::BondTolerance):
			rv.set(ptr->bondTolerance());
			break;
		case (PreferencesVariable::CacheLimit):
			rv.set(ptr->cacheLimit());
			break;
		case (PreferencesVariable::CalculateElec):
			rv.set(ptr->calculateElec());
			break;
		case (PreferencesVariable::CalculateIntra):
			rv.set(ptr->calculateIntra());
			break;
		case (PreferencesVariable::CalculateVdw):
			rv.set(ptr->calculateVdw());
			break;
		case (PreferencesVariable::ClipFar):
			rv.set(ptr->clipFar());
			break;
		case (PreferencesVariable::ClipNear):
			rv.set(ptr->clipNear());
			break;
		case (PreferencesVariable::ColourScheme):
			rv.set(Prefs::colouringScheme(ptr->colourScheme()));
			break;
		case (PreferencesVariable::CombinationRule):
			if (hasArrayIndex) rv.set( ptr->combinationRule( (Combine::CombinationRule) (arrayIndex-1)) );
			else rv.setArray( VTypes::StringData, &ptr->combinationRules_, Combine::nCombinationRules);
			break;
		case (PreferencesVariable::CommonElements):
			rv.set(ptr->commonElements());
			break;
		case (PreferencesVariable::DensityUnit):
			rv.set(Prefs::densityUnit(ptr->densityUnit()));
			break;
		case (PreferencesVariable::DepthCue):
			rv.set( ptr->depthCue() );
			break;
		case (PreferencesVariable::DepthFar):
			rv.set( (int) ptr->depthFar());
			break;
		case (PreferencesVariable::DepthNear):
			rv.set( (int) ptr->depthNear());
			break;
		case (PreferencesVariable::DistanceLabel):
			rv.set( ptr->distanceLabel() );
			break;
		case (PreferencesVariable::ElecCutoff):
			rv.set( ptr->elecCutoff() );
			break;
		case (PreferencesVariable::ElecMethod):
			rv.set(Electrostatics::elecMethod(ptr->electrostaticsMethod()));
			break;
		case (PreferencesVariable::EnergyUnit):
			rv.set(Prefs::energyUnit(ptr->energyUnit()));
			break;
		case (PreferencesVariable::EnergyUpdate):
			rv.set( ptr->energyUpdate() );
			break;
		case (PreferencesVariable::EwaldAlpha):
			rv.set( ptr->ewaldAlpha() );
			break;
		case (PreferencesVariable::EwaldKMax):
			if (hasArrayIndex) rv.set( ptr->ewaldKMax()[arrayIndex-1] );
			else rv.setArray(ptr->ewaldKMax());
			break;
		case (PreferencesVariable::EwaldPrecision):
			rv.set( ptr->ewaldPrecision().value() );
			break;
		case (PreferencesVariable::ForceRhombohedral):
			rv.set( ptr->forceRhombohedral() );
			break;
		case (PreferencesVariable::ForegroundColour):
			if (hasArrayIndex) rv.set( ptr->colour(Prefs::ForegroundColour)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(Prefs::ForegroundColour), 4);
			break;
		case (PreferencesVariable::GlobeSize):
			rv.set(ptr->globeSize() );
			break;
		case (PreferencesVariable::GlyphColour):
			if (hasArrayIndex) rv.set( ptr->colour(Prefs::GlyphColour)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(Prefs::GlyphColour), 4);
			break;
		case (PreferencesVariable::HDistance):
			rv.set( ptr->hydrogenDistance() );
			break;
		case (PreferencesVariable::KeyAction):
			if (hasArrayIndex) rv.set(Prefs::keyAction( ptr->keyAction((Prefs::ModifierKey) (arrayIndex-1))) );
			else rv.setArray( VTypes::StringData, &ptr->keyActionTexts_, Prefs::nModifierKeys);
			break;
		case (PreferencesVariable::LabelSize):
			rv.set( ptr->labelSize() );
			break;
		case (PreferencesVariable::LineAliasing):
			rv.set( ptr->lineAliasing() );
			break;
		case (PreferencesVariable::ManualSwapBuffers):
			rv.set( ptr->manualSwapBuffers() );
			break;
		case (PreferencesVariable::MaxCuboids):
			rv.set( ptr->maxCuboids() );
			break;
		case (PreferencesVariable::MaxRings):
			rv.set( ptr->maxRings() );
			break;
		case (PreferencesVariable::MaxRingSize):
			rv.set( ptr->maxRingSize() );
			break;
		case (PreferencesVariable::MaxUndo):
			rv.set( ptr->maxUndoLevels() );
			break;
		case (PreferencesVariable::ModelUpdate):
			rv.set( ptr->modelUpdate() );
			break;
		case (PreferencesVariable::MopacExe):
			rv.set( ptr->mopacExe() );
			break;
		case (PreferencesVariable::MouseAction):
			if (hasArrayIndex) rv.set(Prefs::mouseAction( ptr->mouseAction((Prefs::MouseButton) (arrayIndex-1))) );
			else rv.setArray( VTypes::StringData, &ptr->mouseActionTexts_, Prefs::nMouseButtons);
			break;
		case (PreferencesVariable::MultiSampling):
			rv.set( ptr->multiSampling() );
			break;
		case (PreferencesVariable::NoQtSettings):
			rv.set( ptr->loadQtSettings() );
			break;
		case (PreferencesVariable::OffScreenObjects):
			rv.set( ptr->offScreenObjects() );
			break;
		case (PreferencesVariable::Perspective):
			rv.set( ptr->hasPerspective() );
			break;
		case (PreferencesVariable::PerspectiveFov):
			rv.set( ptr->perspectiveFov() );
			break;
		case (PreferencesVariable::PolygonAliasing):
			rv.set( ptr->polygonAliasing() );
			break;
		case (PreferencesVariable::RenderStyle):
			rv.set( Atom::drawStyle(ptr->renderStyle()) );
			break;
		case (PreferencesVariable::ReplicateFold):
			rv.set( ptr->replicateFold() );
			break;
		case (PreferencesVariable::ReplicateTrim):
			rv.set( ptr->replicateTrim() );
			break;
		case (PreferencesVariable::ScreenObjects):
			rv.set( ptr->screenObjects() );
			break;
		case (PreferencesVariable::SelectionScale):
			rv.set( ptr->selectionScale() );
			break;
		case (PreferencesVariable::Shininess):
			rv.set( (int) ptr->shininess() );
			break;
		case (PreferencesVariable::SpecularColour):
			if (hasArrayIndex) rv.set( ptr->colour(Prefs::SpecularColour)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(Prefs::SpecularColour), 4);
			break;
		case (PreferencesVariable::Spotlight):
			rv.set( ptr->spotlightActive() );
			break;
		case (PreferencesVariable::SpotlightAmbient):
			if (hasArrayIndex) rv.set( ptr->spotlightColour(Prefs::AmbientComponent)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->spotlightColour(Prefs::AmbientComponent), 4);
			break;
		case (PreferencesVariable::SpotlightDiffuse):
			if (hasArrayIndex) rv.set( ptr->spotlightColour(Prefs::DiffuseComponent)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->spotlightColour(Prefs::DiffuseComponent), 4);
			break;
		case (PreferencesVariable::SpotlightPosition):
			if (hasArrayIndex) rv.set( ptr->spotlightPosition()[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->spotlightPosition(), 4);
			break;
		case (PreferencesVariable::SpotlightSpecular):
			if (hasArrayIndex) rv.set( ptr->spotlightColour(Prefs::SpecularComponent)[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->spotlightColour(Prefs::SpecularComponent), 4);
			break;
		case (PreferencesVariable::TempDir):
			rv.set( ptr->tempDir() );
			break;
		case (PreferencesVariable::UseFrameBuffer):
			rv.set( ptr->useFrameBuffer() );
			break;
		case (PreferencesVariable::UseNiceText):
			rv.set( ptr->useNiceText() );
			break;
		case (PreferencesVariable::VdwCutoff):
			rv.set( ptr->vdwCutoff() );
			break;
		case (PreferencesVariable::VdwScale):
			rv.set( ptr->vdwScale() );
			break;
		case (PreferencesVariable::Warn1056):
			rv.set( ptr->warning1056() );
			break;
		case (PreferencesVariable::ZMapping):
			rv.set( ElementMap::zMapType( ptr->zMapType()) );
			break;
		case (PreferencesVariable::ZoomThrottle):
			rv.set( ptr->zoomThrottle() );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PreferencesVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("PreferencesVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool PreferencesVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PreferencesVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Prefs type.\n", i);
		msg.exit("PreferencesVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = TRUE;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				msg.print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newvalue.arraySize() > 0)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newvalue.arraySize() > accessorData[i].arraySize)
			{
				msg.print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newvalue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
			else if ((newvalue.type() != VTypes::VectorData) && (newvalue.arraySize() != 3))
			{
				msg.print("Error: Only an array of size 3 can be assigned to a vector (member '%s').\n", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		msg.exit("PreferencesVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Prefs *ptr= (Prefs*) sourcerv.asPointer(VTypes::PreferencesData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::PreferencesData));
		result = FALSE;
	}
	int n;
	Prefs::ColouringScheme cs;
	Prefs::DensityUnit du;
	Prefs::EnergyUnit eu;
	Electrostatics::ElecMethod em;
//	Prefs::ModifierKey mk;
	Prefs::KeyAction ka;
//	Prefs::MouseButton mb;
	Prefs::MouseAction ma;
	Atom::DrawStyle ds;
	ElementMap::ZMapType zm;
	if (result) switch (acc)
	{
		case (PreferencesVariable::AngleLabel):
			ptr->setAngleLabel( newvalue.asString(result) );
			break;
		case (PreferencesVariable::AtomDetail):
			ptr->setAtomDetail( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::AtomStyleRadius):
			if (newvalue.arraySize() == Atom::nDrawStyles) for (n=0; n<Atom::nDrawStyles; ++n) ptr->setAtomStyleRadius( (Atom::DrawStyle) n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setAtomStyleRadius( (Atom::DrawStyle) (arrayIndex-1), newvalue.asDouble(result));
			else for (n=0; n<Atom::nDrawStyles; ++n) ptr->setAtomStyleRadius( (Atom::DrawStyle) n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::BackCull):
			ptr->setBackfaceCulling(newvalue.asBool());
			break;
		case (PreferencesVariable::BackgroundColour):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setColour(Prefs::BackgroundColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::BackgroundColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::BackgroundColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::BondDetail):
			ptr->setBondDetail( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::BondStyleRadius):
			if (newvalue.arraySize() == Atom::nDrawStyles) for (n=0; n<Atom::nDrawStyles; ++n) ptr->setBondStyleRadius( (Atom::DrawStyle) n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setBondStyleRadius( (Atom::DrawStyle) (arrayIndex-1), newvalue.asDouble(result));
			else for (n=0; n<Atom::nDrawStyles; ++n) ptr->setBondStyleRadius( (Atom::DrawStyle) n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::BondTolerance):
			ptr->setBondTolerance( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::CacheLimit):
			ptr->setCacheLimit( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::CalculateElec):
			ptr->setCalculateElec( newvalue.asBool() );
			break;
		case (PreferencesVariable::CalculateIntra):
			ptr->setCalculateIntra( newvalue.asBool() );
			break;
		case (PreferencesVariable::CalculateVdw):
			ptr->setCalculateVdw( newvalue.asBool() );
			break;
		case (PreferencesVariable::ClipFar):
			ptr->setClipFar( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::ClipNear):
			ptr->setClipNear( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::ColourScheme):
			cs = Prefs::colouringScheme( newvalue.asString(result), TRUE );
			if (cs != Prefs::nColouringSchemes) ptr->setColourScheme(cs);
			else result = FALSE;
			break;
		case (PreferencesVariable::CombinationRule):
			if (newvalue.arraySize() == Combine::nCombinationRules) for (n=0; n<Combine::nCombinationRules; ++n)
			{
				ptr->setCombinationRule( (Combine::CombinationRule) n, newvalue.asString(n, result));
			}
			else if (hasArrayIndex)
			{
				ptr->setCombinationRule( (Combine::CombinationRule) (arrayIndex-1), newvalue.asString(result));
			}
			else for (n=0; n<Combine::nCombinationRules; ++n) ptr->setCombinationRule((Combine::CombinationRule) n, newvalue.asString(result));
			// Regenerate equations to check
			if (!Combine::regenerateEquations()) result = FALSE;
			break;
		case (PreferencesVariable::CommonElements):
			ptr->setCommonElements( newvalue.asString(result) );
			break;
		case (PreferencesVariable::DensityUnit):
			du = Prefs::densityUnit( newvalue.asString(result), TRUE );
			if (du != Prefs::nDensityUnits) ptr->setDensityUnit(du);
			else result = FALSE;
			break;
		case (PreferencesVariable::DepthCue):
			ptr->setDepthCue( newvalue.asBool() );
			break;
		case (PreferencesVariable::DepthFar):
			ptr->setDepthFar( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::DepthNear):
			ptr->setDepthNear( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::DistanceLabel):
			ptr->setDistanceLabel( newvalue.asString(result) );
			break;
		case (PreferencesVariable::ElecCutoff):
			ptr->setElecCutoff( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::ElecMethod):
			em = Electrostatics::elecMethod( newvalue.asString(result), TRUE );
			if (em != Electrostatics::nElectrostatics) ptr->setElectrostaticsMethod(em);
			else result = FALSE;
			break;
		case (PreferencesVariable::EnergyUnit):
			eu = Prefs::energyUnit( newvalue.asString(result), TRUE );
			if (eu != Prefs::nEnergyUnits) ptr->setEnergyUnit(eu);
			else result = FALSE;
			break;
		case (PreferencesVariable::EnergyUpdate):
			ptr->setEnergyUpdate( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::EwaldAlpha):
			ptr->setEwaldAlpha( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::EwaldKMax):
			if (newvalue.arraySize() == 3) for (n=0; n<3; ++n) ptr->setEwaldKMax(n, newvalue.asInteger(n, result));
			else if (hasArrayIndex) ptr->setEwaldKMax(arrayIndex-1, newvalue.asInteger(result));
			else for (n=0; n<3; ++n) ptr->setEwaldKMax(n, newvalue.asInteger(result));
			break;
		case (PreferencesVariable::EwaldPrecision):
			ptr->ewaldPrecision().set( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::ForceRhombohedral):
			ptr->setForceRhombohedral( newvalue.asBool() );
			break;
		case (PreferencesVariable::ForegroundColour):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setColour(Prefs::ForegroundColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::ForegroundColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::ForegroundColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::GlobeSize):
			ptr->setGlobeSize( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::GlyphColour):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setColour(Prefs::GlyphColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::GlyphColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::GlyphColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::HDistance):
			ptr->setHydrogenDistance( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::KeyAction):
			if (newvalue.arraySize() == Prefs::nModifierKeys) for (n=0; n<Prefs::nModifierKeys; ++n)
			{
				ka = Prefs::keyAction( newvalue.asString(n, result) );
				if ((ka != Prefs::nKeyActions) && result) ptr->setKeyAction( (Prefs::ModifierKey) n, ka);
				else { result = FALSE; break; }
			}
			else if (hasArrayIndex)
			{
				ka = Prefs::keyAction( newvalue.asString(result) );
				if ((ka != Prefs::nKeyActions) && result) ptr->setKeyAction( (Prefs::ModifierKey) (arrayIndex-1), ka);
				else result = FALSE;
			}
			else
			{
				ka = Prefs::keyAction( newvalue.asString(result) );
				if ((ka != Prefs::nKeyActions) && result) for (n=0; n<Prefs::nKeyActions; ++n) ptr->setKeyAction( (Prefs::ModifierKey) n, ka);
				else { result = FALSE; break; }
			}
			break;
		case (PreferencesVariable::LabelSize):
			ptr->setLabelSize( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::LineAliasing):
			ptr->setLineAliasing( newvalue.asBool() );
			break;
		case (PreferencesVariable::ManualSwapBuffers):
			ptr->setManualSwapBuffers( newvalue.asBool() );
			break;
		case (PreferencesVariable::MaxCuboids):
			ptr->setMaxCuboids( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::MaxRings):
			ptr->setMaxRings( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::MaxRingSize):
			ptr->setMaxRingSize( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::MaxUndo):
			ptr->setMaxUndoLevels( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::ModelUpdate):
			ptr->setModelUpdate( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::MopacExe):
			ptr->setMopacExe( newvalue.asString(result) );
			break;
		case (PreferencesVariable::MouseAction):
			if (newvalue.arraySize() == Prefs::nModifierKeys) for (n=0; n<Prefs::nMouseActions; ++n)
			{
				ma = Prefs::mouseAction( newvalue.asString(n, result) );
				if ((ma != Prefs::nMouseActions) && result) ptr->setMouseAction( (Prefs::MouseButton) n, ma);
				else { result = FALSE; break; }
			}
			else if (hasArrayIndex)
			{
				ma = Prefs::mouseAction( newvalue.asString(result) );
				if ((ma != Prefs::nMouseActions) && result) ptr->setMouseAction( (Prefs::MouseButton) (arrayIndex-1), ma);
				else result = FALSE;
			}
			else
			{
				ma = Prefs::mouseAction( newvalue.asString(result) );
				if ((ma != Prefs::nMouseActions) && result) for (n=0; n<Prefs::nMouseActions; ++n) ptr->setMouseAction( (Prefs::MouseButton) n, ma);
				else { result = FALSE; break; }
			}
			break;
		case (PreferencesVariable::MultiSampling):
			ptr->setMultiSampling( newvalue.asBool() );
			break;
		case (PreferencesVariable::NoQtSettings):
			ptr->setLoadQtSettings( newvalue.asBool() );
			break;
		case (PreferencesVariable::OffScreenObjects):
			ptr->setOffScreenObjects( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::Perspective):
			ptr->setPerspective( newvalue.asBool() );
			break;
		case (PreferencesVariable::PerspectiveFov):
			ptr->setPerspectiveFov( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::PolygonAliasing):
			ptr->setPolygonAliasing( newvalue.asBool() );
			break;
		case (PreferencesVariable::RenderStyle):
			ds = Atom::drawStyle( newvalue.asString(result), TRUE );
			if ((ds != Atom::nDrawStyles) && result) ptr->setRenderStyle(ds);
			else result = FALSE;
			break;
		case (PreferencesVariable::ReplicateFold):
			ptr->setReplicateFold( newvalue.asBool() );
			break;
		case (PreferencesVariable::ReplicateTrim):
			ptr->setReplicateTrim( newvalue.asBool() );
			break;
		case (PreferencesVariable::ScreenObjects):
			ptr->setScreenObjects( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::SelectionScale):
			ptr->setSelectionScale( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::Shininess):
			ptr->setShininess( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::SpecularColour):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setColour(Prefs::SpecularColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::SpecularColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::SpecularColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::Spotlight):
			ptr->setSpotlightActive( newvalue.asBool() );
			break;
		case (PreferencesVariable::SpotlightAmbient):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setSpotlightColour(Prefs::AmbientComponent, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setSpotlightColour(Prefs::AmbientComponent, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setSpotlightColour(Prefs::AmbientComponent, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::SpotlightDiffuse):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setSpotlightColour(Prefs::DiffuseComponent, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setSpotlightColour(Prefs::DiffuseComponent, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setSpotlightColour(Prefs::DiffuseComponent, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::SpotlightPosition):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setSpotlightPosition(n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setSpotlightPosition(arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setSpotlightPosition(n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::SpotlightSpecular):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setSpotlightColour(Prefs::SpecularComponent, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setSpotlightColour(Prefs::SpecularComponent, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setSpotlightColour(Prefs::SpecularComponent, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::TempDir):
			ptr->setTempDir( newvalue.asString(result) );
			break;
		case (PreferencesVariable::UseFrameBuffer):
			ptr->setUseFrameBuffer( newvalue.asBool() );
			break;
		case (PreferencesVariable::UseNiceText):
			ptr->setUseNiceText( newvalue.asBool() );
			break;
		case (PreferencesVariable::VdwCutoff):
			ptr->setVdwCutoff( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::VdwScale):
			ptr->setVdwScale( newvalue.asDouble(result) );
			break;
		case (PreferencesVariable::Warn1056):
			ptr->setWarning1056( newvalue.asBool() );
			break;
		case (PreferencesVariable::ZMapping):
			zm = ElementMap::zMapType( newvalue.asString(result), TRUE );
			if (zm != ElementMap::nZMapTypes) ptr->setZMapType(zm);
			else result = FALSE;
			break;
		case (PreferencesVariable::ZoomThrottle):
			ptr->setZoomThrottle( newvalue.asDouble(result) );
			break;
		default:
			printf("PreferencesVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	// Update model and main view
	if (result)
	{
		if (aten.current.rs != NULL) aten.current.rs->changeLog.add(Log::Visual);
		gui.mainView.postRedisplay();
	}
	msg.exit("PreferencesVariable::setAccessor");
	return result;
}

// Perform desired function
bool PreferencesVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("PreferencesVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Preferences type.\n", i);
		msg.exit("PreferencesVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Prefs *ptr= (Prefs*) rv.asPointer(VTypes::PreferencesData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in PreferencesVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("PreferencesVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void PreferencesVariable::printAccessors()
{
	if (PreferencesVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<PreferencesVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((PreferencesVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<PreferencesVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}
