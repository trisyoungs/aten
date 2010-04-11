/*
	*** User action text
	*** src/gui/actiontext.cpp
	Copyright T. Youngs 2007-2010

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

#include "render/canvas.h"

// User action texts (name, nomod, shift, ctrl, alt)
UserActionText UserActionTexts[] = {
	{ "", "", "", "", "" },
	{ "Select", 	"Click or click-drag to select",
		"toggle (click) or to add to selection (click-drag)",
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
	{ "Draw Fragments", "Click to draw free fragments, or attach to existing atoms (click-drag to rotate)",
		"to replace attached hydrogen instead of creating new bond",
		"",
		"cycle between anchor points on fragment" },
	{ "Transmute", "Click atoms to transmute into current drawing element",
		"",
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
	{ "Picking Mode", "",
		"",
		"",
		"" }
	};

