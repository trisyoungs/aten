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

// User action texts (name, nomod, shift, ctrl, alt)
UserActionText UserActionTexts[] = {
	{ "", "", "", "", "" },
	{ "Select", 	"Click atoms to select, or click-drag to select by box",
		"toggle selection state (click), add atoms to current selection (click-drag)",
		"", "" },
	{ "Select Molecule", "Click atom to select bound fragment",
		"to add bound fragment to current selection",
		"", "" },
	{ "Select Element", "Click atom to select all atoms of the same element",
		"Click atom to add all atoms of the same element to the current selection",
		"", "" },
	{ "Select Radial", "", "", "", "" },
	{ "Measure Distance", "", "", "", "" },
	{ "Measure Angle", "", "", "", "" },
	{ "Measure Torsion", "", "", "", "" },
	{ "Draw Atoms", "", "", "", "" },
	{ "Draw Chains", "", "", "", "" },
	{ "Transmute", "", "", "", "" },
	{ "Delete Atoms", "", "", "", "" },
	{ "Probe Atom", "", "", "", "" },
	{ "Create Single Bond", "", "", "", "" },
	{ "Create Double Bond", "", "", "", "" },
	{ "Create Triple Bond", "", "", "", "" },
	{ "Delete Bond", "", "", "", "" },
	{ "Add Hydrogens", "", "", "", "" },
	{ "Rotate Camera (XY)", "", "", "", "" },
	{ "Rotate Camera (Z)", "", "", "", "" },
	{ "Translate Camera", "", "", "", "" },
	{ "Zoom Camera", "", "", "", "" },
	{ "Rotate Selection (XY)", "", "", "", "" },
	{ "Rotate Selection (Z)", "", "", "", "" },
	{ "Translate Selection", "", "", "", "" },
	{ "Picking", "", "", "", "" }
	};

