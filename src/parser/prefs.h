/*
	*** Prefs Variable
	*** src/parser/prefs.h
	Copyright T. Youngs 2007-2016

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

#ifndef ATEN_PREFSVARIABLE_H
#define ATEN_PREFSVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class TreeNode;

// Prefs Master Variable
class PreferencesVariable : public Variable
{
	public:
	// Constructor / Destructor
	PreferencesVariable();
	~PreferencesVariable();

	/*
	 * Access Data
	 */
	public:
	// Accessor list
	enum Accessors { AllowDialogs, AngleLabelFormat, AromaticRingColour, AtomStyleRadius, BackCull, BackgroundColour, BondStyleRadius, BondTolerance, CalculateIntra, CalculateVdw, ChargeLabelFormat, ClipFar, ClipNear, ColourScales, CorrectTransparentGrids, DashedAromatics, DefaultDrawStyle, DensityUnit, DepthCue, DepthFar, DepthNear, DistanceLabelFormat, DynamicPanels, ElecCutoff, ElecMethod, EnergyUnit, EwaldAlpha, EwaldKMax, EwaldPrecision, FontFileName, ForegroundColour, GlobeSize, GlyphDefaultColour, HBonds, HBondDotRadius, HDistance, ImageQuality, KeyAction, LabelSize, LabelDepthScaling, LineAliasing, MaxCuboids, MaxRings, MaxRingSize, MaxUndo, MessagesFontSize, MopacExe, MouseAction, MouseMoveFilter, MultiSampling, NoQtSettings, PartitionGrid, Perspective, PerspectiveFov, PolygonAliasing, Quality, ReuseQuality, SelectionScale, Shininess, SpecularColour, Spotlight, SpotlightAmbient, SpotlightDiffuse, SpotlightPosition, SpotlightSpecular, StickNormalWidth, StickSelectedWidth, TempDir, UseWidgetForegroundBackground, VdwCutoff, VibrationArrowColour, ViewerFontFileName, ViewLock, ViewRotationGlobe, ZoomThrottle, nAccessors };
	// Function list
	enum Functions { DummyFunction, nFunctions };
	// Search variable access list for provided accessor
	StepNode* findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
	// Static function to search accessors
	static StepNode* accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
	// Retrieve desired value
	static bool retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex = -1);
	// Set desired value
	static bool setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex = -1);
	// Perform desired function
	static bool performFunction(int i, ReturnValue& rv, TreeNode* node);
	// Accessor data
	static Accessor accessorData[nAccessors];
	// Function Accessor data
	static FunctionAccessor functionData[nFunctions];
};

ATEN_END_NAMESPACE

#endif
