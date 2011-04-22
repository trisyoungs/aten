/*
	*** Prefs Variable
	*** src/parser/prefs.h
	Copyright T. Youngs 2007-2011

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

// Forward Declarations
class TreeNode;

// Prefs Master Variable
class PreferencesVariable : public Variable
{
	public:
	// Constructor / Destructor
	PreferencesVariable();
	~PreferencesVariable();

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { AngleLabelFormat, AromaticRingColour, AtomStyleRadius, BackCull, BackgroundColour, BondStyleRadius, BondTolerance, CacheLimit, CalculateIntra, CalculateVdw, ClipFar, ClipNear, ColourScales, ColourScheme, CombinationRule, CommonElements, DashedAromatics, DensityUnit, DepthCue, DepthFar, DepthNear, DistanceLabelFormat, ElecCutoff, ElecMethod, EncoderArgs, EncoderExe, EncoderPostArgs, EncoderPostExe, EnergyUnit, EnergyUpdate, EwaldAlpha, EwaldKMax, EwaldPrecision, ForceRhombohedral, FrameCurrentModel, FrameWholeView, GlobeAxesColour, GlobeColour, GlobeSize, GlyphDefaultColour, HBonds, HBondDotRadius, HDistance, ImageQuality, KeyAction, LabelSize, LevelOfDetailStartZ, LevelOfDetailWidth, LevelsOfDetail, LineAliasing, ManualSwapBuffers, MaxCuboids, MaxRings, MaxRingSize, MaxUndo, ModelUpdate, MopacExe, MouseAction, MouseMoveFilter, MultiSampling, NoQtSettings, Perspective, PerspectiveFov, PolygonAliasing, Quality, RenderStyle, ReplicateFold, ReplicateTrim, ReuseQuality, SelectionScale, Shininess, SpecularColour, Spotlight, SpotlightAmbient, SpotlightDiffuse, SpotlightPosition, SpotlightSpecular, TempDir, TextColour, TransparencyBinStartZ, TransparencyBinWidth, TransparencyCorrect, TransparencyNBins, UnitCellAxesColour, UnitCellColour, UseFrameBuffer, UseNiceText, VdwCutoff, VdwScale, VibrationArrowColour, ViewRotationGlobe, Warn1056, ZMapping, ZoomThrottle, nAccessors };
	// Function list
	enum Functions { DummyFunction, nFunctions };
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
	// Static function to search accessors
	static StepNode *accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
	// Retrieve desired value
	static bool retrieveAccessor(int i, ReturnValue &rv, bool hasarrayindex, int arrayIndex = -1);
	// Set desired value
	static bool setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasarrayindex, int arrayIndex = -1);
	// Perform desired function
	static bool performFunction(int i, ReturnValue &rv, TreeNode *node);
	// Print valid accessors/functions
	static void printAccessors();
	// Accessor data
	static Accessor accessorData[nAccessors];
	// Function Accessor data
	static FunctionAccessor functionData[nFunctions];
};

#endif
