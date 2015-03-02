/*
	*** User Actions
	*** src/gui/useractions.cpp
	Copyright T. Youngs 2007-2015

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

#include "gui/useractions.h"
#include "math/constants.h"

ATEN_BEGIN_NAMESPACE

// User action texts (name, nomod, shift, ctrl, alt)
UserAction UserActions[] = {
	{ "", "", "", "", "" },
	{ "Select", 	"Click or click-drag to select",
		"toggle (left click) or add area to selection (left click-drag) or translate (middle click-drag) or rotate in local frame (right click-drag)",
		"remove from selection",
		"" },
	{ "Select Molecule", "Click atom to select bound fragment",
		"add to current selection",
		"remove from selection",
		"" },
	{ "Select Element", "Click to select all atoms of the same element",
		"add to current selection",
		"remove from selection",
		"" },
	{ "Select Radial", "",
		"",
		"",
		"" },
	{ "Measure Distance", "Click two atoms i-j to measure distance",
		"",
		"",
		"" },
	{ "Measure Angle", "Click three atoms i-j-k to measure angle",
		"",
		"",
		"" },
	{ "Measure Torsion", "Click four atoms i-j-k-l to measure torsion",
		"",
		"",
		"" },
	{ "Draw Atoms", "Click in empty space to create an atom of the current drawing element",
		"",
		"",
		"" },
	{ "Draw Chains", "Click-drag to draw bound atoms",
		"",
		"",
		"" },
	{ "Draw Fragments", "Click or click-drag to draw free fragments, or attach to existing atoms (click-drag to rotate)",
		"to replace attached atom instead of creating new bond",
		"cycle over bonds on attachment point (existing atom)",
		"cycle between anchor points on fragment" },
	{ "Transmute", "Click atoms to transmute into current drawing element",
		"to transmute all atoms of the same element as the clicked atom",
		"",
		"" },
	{ "Delete Atoms/Bonds", "Click individual atoms to delete them",
		"to delete all bonds attached to atom",
		"",
		"" },
	{ "Probe Atom", "Click individual atoms to view atom information",
		"",
		"",
		"" },
	{ "Create Single Bond", "Click two atoms i-j to join them with a single bond",
		"",
		"",
		"" },
	{ "Create Double Bond", "Click two atoms i-j to join them with a double bond",
		"",
		"",
		"" },
	{ "Create Triple Bond", "Click two atoms i-j to join them with a triple bond",
		"",
		"",
		"" },
	{ "Delete Bond", "Click two atoms to remove any bond between them",
		"",
		"",
		"" },
	{ "Add Hydrogens", "Click single atoms to automatically add hydrogen atoms",
		"",
		"",
		"" },
	{ "Grow Atom", "Click single atoms to grow additional atoms in selected geometry",
		"grow without bonding",
		"",
		"" },
	{ "Rotate (XY)", "Click-drag to rotate entire model",
		"modify coordinates of current atom selection",
		"",
		"" },
	{ "Rotate (Z)", "Click-drag to rotate entire model",
		"modify coordinates of current atom selection",
		"",
		"" },
	{ "Translate", "Click-drag to move camera",
		"modify coordinates of current atom selection",
		"",
		"" },
	{ "Zoom Camera", "Click-drag to zoom camera",
		"",
		"",
		"" },
	{ "Rotate (XY)", "Click-drag to rotate entire model",
		"modify coordinates of current atom selection",
		"",
		"" },
	{ "Rotate (Z)", "Click-drag to rotate entire model",
		"modify coordinates of current atom selection",
		"",
		"" },
	{ "Translate", "Click-drag to move camera",
		"modify coordinates of current atom selection",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the vector shift",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which will define the rotation axis for the transformation",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the X-axis of the transformation matrix",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the Y-axis of the transformation matrix",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the Z-axis of the transformation matrix",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the X-axis of the reference matrix for the conversion",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the Y-axis of the reference matrix for the conversion",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the Z-axis of the reference matrix for the conversion",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the X-axis of the target matrix for the conversion",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the Y-axis of the target matrix for the conversion",
		"",
		"",
		"" },
	{ "Picking Mode", "Pick two atoms which define the Z-axis of the target matrix for the conversion",
		"",
		"",
		"" }
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Action group checks
bool UserAction::isBuildWidgetAction(UserAction::Action ua)
{
	switch (ua)
	{
		case (DrawAtomAction):
		case (DrawChainAction):
		case (DrawFragmentAction):
		case (DrawTransmuteAction):
		case (DrawDeleteAction):
		case (DrawBondSingleAction):
		case (DrawBondDoubleAction):
		case (DrawBondTripleAction):
		case (DrawDeleteBondAction):
		case (DrawAddHydrogenAction):
		case (DrawGrowAtomAction):
			return TRUE;
		default:
			return FALSE;
	}
}

bool UserAction::isGeometryWidgetAction(UserAction::Action ua)
{
	switch (ua)
	{
		case (MeasureDistanceAction):
		case (MeasureAngleAction):
		case (MeasureTorsionAction):
			return TRUE;
		default:
			return FALSE;
	}
}

bool UserAction::isPositionWidgetAction(UserAction::Action ua)
{
	switch (ua)
	{
		case (ShiftPickVectorAction):
			return TRUE;
		default:
			return FALSE;
	}
}

bool UserAction::isTransformWidgetAction(UserAction::Action ua)
{
	switch (ua)
	{
		case (RotatePickAxisAction):
		case (TransformPickAAction):
		case (TransformPickBAction):
		case (TransformPickCAction):
		case (ConvertSourcePickAAction):
		case (ConvertSourcePickBAction):
		case (ConvertSourcePickCAction):
		case (ConvertTargetPickAAction):
		case (ConvertTargetPickBAction):
		case (ConvertTargetPickCAction):
			return TRUE;
		default:
			return FALSE;
	}
}

