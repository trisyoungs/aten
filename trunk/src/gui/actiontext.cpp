/*
	*** User action text
	*** src/gui/actiontext.cpp
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

#include "gui/canvas.h"

// User action texts
UserActionText UserActionTexts[] = {
	{ "", "", "", "", "" },
	{ "Select", 	"Click atoms to select. Click-drag to select by box",
		"Click atoms to toggle selection state, click-drag to add atoms to current selection",
		"", "" },
	{ "Select Molecule", "Click atom to select all other atoms reachable through bonds",
		"Click atom to select all other atoms reachable through bonds and add to current selection",
		"", "" }
	};
/*	// Action texts
	const char *name;
	const char *unModified;
	const char *shiftModified;
	const char *ctrlModified;
	const char *altModified;
*/
// NoAction, SelectAction, SelectMoleculeAction, SelectElementAction, SelectRadialAction, MeasureDistanceAction, MeasureAngleAction, MeasureTorsionAction, EditDrawAction, EditChainAction, EditTransmuteAction, EditDeleteAction, EditProbeAction, EditBondSingleAction, EditBondDoubleAction, EditBondTripleAction, EditDeleteBondAction, EditAddHydrogenAction, RotateXYAction, RotateZAction, TranslateAction, ZoomAction, TransformRotateXYAction, TransformRotateZAction, TransformTranslateAction, ManualPickAction

