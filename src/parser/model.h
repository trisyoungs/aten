/*
	*** Model Variable and Array
	*** src/parser/model.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_MODELVARIABLE_H
#define ATEN_MODELVARIABLE_H

#include "parser/pvariable.h"
#include "parser/accessor.h"

// Forward Declarations
class Model;

// Model Variable
class ModelVariable : public PointerVariable
{
	public:
	// Constructor / Destructor
	ModelVariable(Model *ptr = NULL, bool constant = FALSE);
	~ModelVariable();

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { Angles, Atoms, Bonds, Celldata, ComponentDensity, ComponentPartition, ComponentPolicy, ComponentPopulation, Distances, Eigenvectors, Energy, FFAngles, FFBonds, FFMass, FFTorsions, FFTypes, FField, Filename, Frame, Frames, Glyphs, Grids, Id, Mass, Name, NAngles, NAtoms, NBasisCartesians, NBasisShells, NBonds, NDistances, NEigenvectors, NFFAngles, NFFBonds, NFFTorsions, NFFTypes, NFrames, NGlyphs, NGrids, NPatterns, NSelected, NTorsions, NUnknown, NVibrations, Patterns, PropagateStyle, Selection, Torsions, Vibrations, ZMatrix, nAccessors };
	// Function list
	enum Functions { AddHydrogen, AngleEnergy, Augment, BondEnergy, Charge, ClearBonds, ClearCharges, ClearSelectedBonds, Copy, Cut, Delete, DeSelect, ElectrostaticEnergy, Expand, Finalise, InterEnergy, IntraEnergy, MoveToEnd, MoveToStart, NewAtom, NewAtomFrac, NewBasisShell, NewBond, NewBondId, NewEigenvector, NewGlyph, NewGrid, NewVibration, Paste, ReBond, ReBondPatterns, ReBondSelection, Redo, ReOrder, SaveBitmap, Select, SelectAll, SelectionAddHydrogen, SelectNone, SelectTree, SetupComponent, ShiftDown, ShiftUp, ShowAll, ToAngstroms, TorsionEnergy, Transmute, Undo, VdwEnergy, nFunctions };

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

// Model Array Variable
class ModelArrayVariable : public PointerArrayVariable
{
	public:
	// Constructor / Destructor
	ModelArrayVariable(TreeNode *sizeexpr, bool constant = FALSE);

	/*
	// Inherited Virtuals
	*/
	public:
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
};

#endif
