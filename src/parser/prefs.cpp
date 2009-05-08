/*
	*** Prefs Variable
	*** src/parser/prefs.cpp
	Copyright T. Youngs 2007-2009

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
// Set / Get
*/

// Set value of variable
bool PreferencesVariable::set(ReturnValue &rv)
{
	msg.print("A constant value (in this case the Prefs) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void PreferencesVariable::reset()
{
	// No action
}

// Return value of node
bool PreferencesVariable::execute(ReturnValue &rv)
{
	rv.set(VTypes::PreferencesData, &prefs);
	return TRUE;
}

// Print node contents
void PreferencesVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s&%li (Prefs) (constant value)\n", tab, &prefs);
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data - name, type, arraysize, ro?
Accessor PreferencesVariable::accessorData[PreferencesVariable::nAccessors] = {
	{ "anglelabel",		VTypes::StringData,	0, FALSE },
	{ "atomdetail"	,	VTypes::DoubleData,	0, FALSE },
	{ "atomstyleradius",	VTypes::DoubleData,	Atom::nDrawStyles, FALSE },
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
	{ "commonelements",	VTypes::StringData,	0, FALSE },
	{ "densityunit",	VTypes::StringData,	0, FALSE },
	{ "distancelabel",	VTypes::StringData,	0, FALSE },
	{ "eleccutoff",		VTypes::DoubleData,	0, FALSE },
	{ "elecmethod",		VTypes::StringData,	0, FALSE },
	{ "energyunit",		VTypes::StringData,	0, FALSE },
	{ "energyupdate",	VTypes::IntegerData,	0, FALSE },
	{ "depthcue",		VTypes::IntegerData,	0, FALSE },
	{ "depthfar",		VTypes::IntegerData,	0, FALSE },
	{ "depthnear",		VTypes::IntegerData,	0, FALSE },
	{ "foregroundcolour",	VTypes::DoubleData,	4, FALSE },
	{ "globesize",		VTypes::IntegerData,	0, FALSE },
	{ "hdistance",		VTypes::DoubleData,	0, FALSE },
	{ "keyaction",		VTypes::StringData,	Prefs::nModifierKeys, FALSE },
	{ "labelsize",		VTypes::IntegerData,	0, FALSE },
	{ "linealiasing",	VTypes::IntegerData,	0, FALSE },
	{ "manualswapbuffers",	VTypes::IntegerData,	0, FALSE },
	{ "maxringsize",	VTypes::IntegerData,	0, FALSE },
	{ "maxundo",		VTypes::IntegerData,	0, FALSE },
	{ "modelupdate",	VTypes::IntegerData,	0, FALSE },
	{ "mouseaction",	VTypes::StringData,	Prefs::nMouseButtons, FALSE },
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
	{ "usenicetext",	VTypes::IntegerData,	0, FALSE },
	{ "vdwcutoff",		VTypes::DoubleData,	0, FALSE },
	{ "vdwscale",		VTypes::DoubleData,	0, FALSE },
	{ "zoomthrottle",	VTypes::DoubleData,	0, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *PreferencesVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return PreferencesVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *PreferencesVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("PreferencesVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'aten&' has no member named '%s'.\n", s);
		msg.exit("PreferencesVariable::accessorSearch");
		return NULL;
	}
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	// Were we given an array index when we didn't want one?
	if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
	{
		msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
		result = NULL;
	}
	else result = new StepNode(i, VTypes::PreferencesData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize != 0);
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
		printf("Internal Error: Accessor id %i is out of range for Prefs type.\n");
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
			msg.exit("ElementVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	Model *m;
	bool result = TRUE;
	if (result) switch (acc)
	{
		case (PreferencesVariable::AngleLabel):
			rv.set( prefs.angleLabel() );
			break;
		case (PreferencesVariable::AtomDetail):
			rv.set( prefs.atomDetail() );
			break;
		case (PreferencesVariable::AtomStyleRadius):
			if (hasArrayIndex) rv.set( VTypes::DoubleData, &prefs.atomStyleRadius_, Atom::nDrawStyles);
			else rv.set(prefs.atomStyleRadius( (Atom::DrawStyle) (arrayIndex-1)) );
			break;
		case (PreferencesVariable::BackgroundColour):
			if (hasArrayIndex) rv.set( VTypes::DoubleData, prefs.colour(Prefs::BackgroundColour), 4);
			else rv.set( prefs.colour(Prefs::BackgroundColour)[arrayIndex-1] );
			break;
		case (PreferencesVariable::BondDetail):
			rv.set( prefs.bondDetail() );
			break;
		case (PreferencesVariable::BondStyleRadius):
			if (hasArrayIndex) rv.set( VTypes::DoubleData, &prefs.bondStyleRadius_, Atom::nDrawStyles);
			else rv.set(prefs.bondStyleRadius( (Atom::DrawStyle) (arrayIndex-1)) );
			break;
		case (PreferencesVariable::BondTolerance):
			rv.set(prefs.bondTolerance());
			break;
		case (PreferencesVariable::CacheLimit):
			rv.set(prefs.cacheLimit());
			break;
		case (PreferencesVariable::CalculateElec):
			rv.set(prefs.calculateElec());
			break;
		case (PreferencesVariable::CalculateIntra):
			rv.set(prefs.calculateIntra());
			break;
		case (PreferencesVariable::CalculateVdw):
			rv.set(prefs.calculateVdw());
			break;
		case (PreferencesVariable::ClipFar):
			rv.set(prefs.clipFar());
			break;
		case (PreferencesVariable::ClipNear):
			rv.set(prefs.clipNear());
			break;
		case (PreferencesVariable::ColourScheme):
			rv.set(Prefs::colouringScheme(prefs.colourScheme()));
			break;
		case (PreferencesVariable::CommonElements):
			rv.set(prefs.commonElements());
			break;
		case (PreferencesVariable::DensityUnit):
			rv.set(Prefs::densityUnit(prefs.densityUnit()));
			break;
		case (PreferencesVariable::DepthCue):
			rv.set( prefs.depthCue() );
			break;
		case (PreferencesVariable::DepthFar):
			rv.set(prefs.depthFar());
			break;
		case (PreferencesVariable::DepthNear):
			rv.set(prefs.depthNear());
			break;
		case (PreferencesVariable::DistanceLabel):
			rv.set( prefs.distanceLabel() );
			break;
		case (PreferencesVariable::ElecCutoff):
			rv.set( prefs.elecCutoff() );
			break;
		case (PreferencesVariable::ElecMethod):
			rv.set(Electrostatics::elecMethod(prefs.electrostaticsMethod()));
			break;
		case (PreferencesVariable::EnergyUnit):
			rv.set(Prefs::energyUnit(prefs.energyUnit()));
			break;
		case (PreferencesVariable::EnergyUpdate):
			rv.set( prefs.energyUpdate() );
			break;
		case (PreferencesVariable::ForegroundColour):
			if (hasArrayIndex) rv.set( VTypes::DoubleData, prefs.colour(Prefs::ForegroundColour), 4);
			else rv.set( prefs.colour(Prefs::ForegroundColour)[arrayIndex-1] );
			break;
		case (PreferencesVariable::GlobeSize):
			rv.set(prefs.globeSize() );
			break;
		case (PreferencesVariable::HDistance):
			rv.set( prefs.hydrogenDistance() );
			break;
		case (PreferencesVariable::KeyAction):
			if (hasArrayIndex) rv.set(Prefs::keyAction( prefs.keyAction((Prefs::ModifierKey) (arrayIndex-1))) );
			else rv.set( VTypes::StringData, &prefs.keyActionTexts_, Prefs::nModifierKeys);
			break;
		case (PreferencesVariable::LabelSize):
			rv.set( prefs.labelSize() );
			break;
		case (PreferencesVariable::LineAliasing):
			rv.set( prefs.lineAliasing() );
			break;
		case (PreferencesVariable::ManualSwapBuffers):
			rv.set( prefs.manualSwapBuffers() );
			break;
		case (PreferencesVariable::MaxRingSize):
			rv.set( prefs.maxRingSize() );
			break;
		case (PreferencesVariable::MaxUndo):
			rv.set( prefs.maxUndoLevels() );
			break;
		case (PreferencesVariable::ModelUpdate):
			rv.set( prefs.modelUpdate() );
			break;
		case (PreferencesVariable::MouseAction):
			if (hasArrayIndex) rv.set(Prefs::mouseAction( prefs.mouseAction((Prefs::MouseButton) (arrayIndex-1))) );
			else rv.set( VTypes::StringData, &prefs.mouseActionTexts_, Prefs::nMouseButtons);
			break;
		case (PreferencesVariable::OffScreenObjects):
			rv.set( prefs.offScreenObjects() );
			break;
		case (PreferencesVariable::Perspective):
			rv.set( prefs.hasPerspective() );
			break;
		case (PreferencesVariable::PerspectiveFov):
			rv.set( prefs.perspectiveFov() );
			break;
		case (PreferencesVariable::PolygonAliasing):
			rv.set( prefs.polygonAliasing() );
			break;
		case (PreferencesVariable::RenderStyle):
			rv.set( Atom::drawStyle(prefs.renderStyle()) );
			break;
		case (PreferencesVariable::ReplicateFold):
			rv.set( prefs.replicateFold() );
			break;
		case (PreferencesVariable::ReplicateTrim):
			rv.set( prefs.replicateTrim() );
			break;
		case (PreferencesVariable::ScreenObjects):
			rv.set( prefs.screenObjects() );
			break;
		case (PreferencesVariable::SelectionScale):
			rv.set( prefs.selectionScale() );
			break;
		case (PreferencesVariable::Shininess):
			rv.set( prefs.shininess() );
			break;
		case (PreferencesVariable::SpecularColour):
			if (hasArrayIndex) rv.set( VTypes::DoubleData, prefs.colour(Prefs::SpecularColour), 4);
			else rv.set( prefs.colour(Prefs::SpecularColour)[arrayIndex-1] );
			break;
		case (PreferencesVariable::Spotlight):
			rv.set( prefs.spotlightActive() );
			break;
		case (PreferencesVariable::UseNiceText):
			rv.set( prefs.useNiceText() );
			break;
		case (PreferencesVariable::VdwCutoff):
			rv.set( prefs.vdwCutoff() );
			break;
		case (PreferencesVariable::VdwScale):
			rv.set( prefs.vdwScale() );
			break;
		case (PreferencesVariable::ZoomThrottle):
			rv.set( prefs.zoomThrottle() );
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
		printf("Internal Error: Accessor id %i is out of range for Prefs type.\n");
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
			if ((newvalue.arraySize() > 0) && (newvalue.arraySize() != accessorData[i].arraySize))
			{
				msg.print("Error: The array being assigned to member '%s' is not of the same size (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
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
	int n;
	Prefs::ColouringScheme cs;
	Prefs::DensityUnit du;
	Prefs::EnergyUnit eu;
	Electrostatics::ElecMethod em;
	Prefs::ModifierKey mk;
	Prefs::KeyAction ka;
	Prefs::MouseButton mb;
	Prefs::MouseAction ma;
	Atom::DrawStyle ds;
	switch (acc)
	{
		case (PreferencesVariable::AngleLabel):
			prefs.setAngleLabel( newvalue.asString(result) );
			break;
		case (PreferencesVariable::AtomDetail):
			prefs.setAtomDetail( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::AtomStyleRadius):
			if (newvalue.arraySize() == Atom::nDrawStyles) for (n=0; n<Atom::nDrawStyles; ++n) ptr->setAtomStyleRadius( (Atom::DrawStyle) n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setAtomStyleRadius( (Atom::DrawStyle) (arrayIndex-1), newvalue.asDouble(result));
			else for (n=0; n<Atom::nDrawStyles; ++n) ptr->setAtomStyleRadius( (Atom::DrawStyle) n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::BackgroundColour):
			if (newvalue.arraySize() == 4) for (n=0; n<4; ++n) ptr->setColour(Prefs::BackgroundColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::BackgroundColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::BackgroundColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::BondDetail):
			prefs.setBondDetail( newvalue.asInteger(result) );
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
			cs = Prefs::colouringScheme( newvalue.asString(result) );
			if (cs != Prefs::nColouringSchemes) ptr->setColourScheme(cs);
			else result = FALSE;
			break;
		case (PreferencesVariable::CommonElements):
			ptr->setCommonElements( newvalue.asString(result) );
			break;
		case (PreferencesVariable::DensityUnit):
			du = Prefs::densityUnit( newvalue.asString(result) );
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
			em = Electrostatics::elecMethod( newvalue.asString(result) );
			if (em != Electrostatics::nElectrostatics) ptr->setElectrostaticsMethod(em);
			else result = FALSE;
			break;
		case (PreferencesVariable::EnergyUnit):
			eu = Prefs::energyUnit( newvalue.asString(result) );
			if (eu != Prefs::nEnergyUnits) ptr->setEnergyUnit(eu);
			else result = FALSE;
			break;
		case (PreferencesVariable::EnergyUpdate):
			ptr->setEnergyUpdate( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::ForegroundColour):
			if (newvalue.arraySize() == 4) for (n=0; n<4; ++n) ptr->setColour(Prefs::ForegroundColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::ForegroundColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::ForegroundColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::GlobeSize):
			ptr->setGlobeSize( newvalue.asInteger(result) );
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
				ka = Prefs::keyAction( newvalue.asString(n, result) );
				if ((ka != Prefs::nKeyActions) && result) ptr->setKeyAction( (Prefs::ModifierKey) (arrayIndex-1), ka);
				else result = FALSE;
			}
			else
			{
				ka = Prefs::keyAction( newvalue.asString(n, result) );
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
		case (PreferencesVariable::MaxRingSize):
			ptr->setMaxRingSize( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::MaxUndo):
			ptr->setMaxUndoLevels( newvalue.asInteger(result) );
			break;
		case (PreferencesVariable::ModelUpdate):
			ptr->setModelUpdate( newvalue.asInteger(result) );
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
				ma = Prefs::mouseAction( newvalue.asString(n, result) );
				if ((ma != Prefs::nMouseActions) && result) ptr->setMouseAction( (Prefs::MouseButton) (arrayIndex-1), ma);
				else result = FALSE;
			}
			else
			{
				ma = Prefs::mouseAction( newvalue.asString(n, result) );
				if ((ma != Prefs::nMouseActions) && result) for (n=0; n<Prefs::nMouseActions; ++n) ptr->setMouseAction( (Prefs::MouseButton) n, ma);
				else { result = FALSE; break; }
			}
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
			ds = Atom::drawStyle( newvalue.asString(result) );
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
			if (newvalue.arraySize() == 4) for (n=0; n<4; ++n) ptr->setColour(Prefs::SpecularColour, n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(Prefs::SpecularColour, arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(Prefs::SpecularColour, n, newvalue.asDouble(result));
			break;
		case (PreferencesVariable::Spotlight):
			ptr->setSpotlightActive( newvalue.asBool() );
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
