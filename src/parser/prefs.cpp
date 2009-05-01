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
	{ "energyunit",	VTypes::StringData,	FALSE, FALSE }
/*

	// Repeat units in positive xyz directions
	Vec3<int> repeatCellsPos_;
	// Repeat units in negative xyz directions
	Vec3<int> repeatCellsNeg_;

	{ "colourscheme",	VTypes::StringData,	FALSE, FALSE },
	{ "energyunit",		VTypes::StringData,	FALSE, FALSE },
	{ "globesize",		VTypes::IntegerData,	FALSE, FALSE },
	{ "imageobjects",	VTypes::IntegerData,	FALSE, FALSE },
	{ "renderstyle",	VTypes::StringData,	FALSE, FALSE },
	{ "screenobjects",	VTypes::IntegerData,	FALSE, FALSE },

	private:
	// Atom sizes / radii
	GLdouble atomStyleRadius_[Atom::nDrawStyles];
	// Bond radii
	GLdouble bondStyleRadius_[Atom::nDrawStyles];
	// Size scaling for atom selection transparency
	GLdouble selectionScale_;
	// Detail of atom quadric (slices/stacks)
	GLint atomDetail_;
	// Detail of bond quadric (slices/stacks)
	GLint bondDetail_;
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

	private:
	// Postfix (units) label for distances
	Dnchar distanceLabel_;
	// Postfix (units) label for angles
	Dnchar angleLabel_;
	// Pointsize for labels
	int labelSize_;
	// Use QGlWidget::renderText (FALSE) or QPainter::drawText (TRUE) for labels etc.
	bool useNiceText_;
	// Flag to manually perform swapBuffers
	bool manualSwapBuffers_;


	private:
	// Bitvector for GL options
	int glOptions_;
	// Shininess of 3D objects
	GLint shininess_;
	// Fog start and finish depths
	GLint fogNear_, fogFar_;
	// Near and far clipping planes for glPerspective() and glFrustum();
	GLdouble clipNear_, clipFar_;


	private:
	// RGB colour values
	GLfloat colours_[Prefs::nPenColours][4];

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
	ElementMap::ZMapType zMapType_;
	// Whether to retain file atom type names on load (in a new forcefield)
	bool keepNames_;
	// Whether to retain view when GUI starts (i.e. don't reset it)
	bool keepView_;


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
	// Hydrogen add distance
	double hydrogenDistance_;


	private:
	// User-definable mouse button actions
	MouseAction mouseAction_[Prefs::nMouseButtons];
	// User-definable key modifier actions
	KeyAction keyAction_[Prefs::nModifierKeys];
	// Zoom 'throttle'
	double zoomThrottle_;

	private:
	// Main modelview update and energy output frequencies
	int modelUpdate_, energyUpdate_;
	// Maximum ring size in ring search algorithm
	int maxRingSize_;
	// Whether to fold atoms before replication
	bool replicateFold_;
	// Whether to trim atoms after replication
	bool replicateTrim_;


	private:
	DensityUnit densityUnit_;
	// Internal energy units to use for forcefield storage, energy calculation etc.
	EnergyUnit energyUnit_;
	// Conversion factors for energy units
	double energyConversions_[Prefs::nEnergyUnits];
	// Factor to convert from atomic units to internal units
	double elecConvert_;

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


	private:
	// Maximum number of undo levels (-1 for unlimited)
	int maxUndoLevels_;

	private:
	// List of common drawing elements to put in SelectElement dialog
	Dnchar commonElements_;

	*/


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
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::PreferencesData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
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
	// Variables used in retrieval
	Model *m;
	bool result = TRUE;
	if (result) switch (acc)
	{
		case (PreferencesVariable::EnergyUnit):
			rv.set(Prefs::energyUnit(prefs.energyUnit()));
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
	// Check for correct lack/presence of array index given
	if (accessorData[i].arraySize == 0)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("PreferencesVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue (as a sanity check - we don't actually use the pointer)
	bool result = TRUE;
	Prefs *ptr= (Prefs*) sourcerv.asPointer(VTypes::PreferencesData, result);
	switch (acc)
	{
		case (PreferencesVariable::EnergyUnit):
			// Call the
			if (!CommandNode::run(Command::EnergyUnits, "s", newvalue.asString())) result = FALSE;
			sourcerv.set(Prefs::energyUnit(prefs.energyUnit()));
			break;
		default:
			printf("PreferencesVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("PreferencesVariable::setAccessor");
	return result;
}
