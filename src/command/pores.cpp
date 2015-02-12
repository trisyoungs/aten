/*
	*** Pores Commands
	*** src/command/pores.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "gui/pores.h"

// Create a partitioning scheme from the current model
bool Command::function_CreateScheme(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Set command variables
	Vec3<int> gridSize(50,50,50);
	if (c->hasArg(3)) gridSize.set(c->argi(1), c->argi(2), c->argi(3));
	double minSizePcnt = 0.05;
	if (c->hasArg(4)) minSizePcnt = c->argd(4) / 100.0;
	int atomExtent = 2;
	if (c->hasArg(5)) atomExtent = c->argi(5);
	bool copyToBuilder = FALSE;
	if (c->hasArg(6)) copyToBuilder = c->argb(6);

	// Create temporary partitioning scheme structure
	PartitioningScheme &scheme = PoresWidget::partitioningScheme();
	scheme.initialiseAbsolute(c->argc(0), "Scheme generated from model");
	Vec3<double> cellDelta(1.0/gridSize.x, 1.0/gridSize.y, 1.0/gridSize.z);
	scheme.setGridSize(gridSize);
	Grid &schemeGrid = scheme.grid();
	double volumeElement = obj.rs()->cell()->volume() / gridSize.dp(gridSize);
	schemeGrid.setAxes(cellDelta);
	double ***data = schemeGrid.data3d();
	int minimumSize = gridSize.x*gridSize.y*gridSize.z*minSizePcnt;

	// Set all grid data to -1.0 to start with (i.e. all space available)
	int x, y, z, a, b, d, x2, y2, z2;
	for (x=0; x<gridSize.x; ++x)
	{
		for (y=0; y<gridSize.y; ++y)
		{
			for (z=0; z<gridSize.z; ++z) data[x][y][z] = -1.0;
		}
	}
	
	// Now, zero individual cells which model atoms sit in
	UnitCell *cell = obj.rs()->cell();
	Vec3<double> r;
	// Determine rough atom size (in grid cells....
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next)
	{
		// Work in fractional coordinates
		// Atom centre...
		r = cell->realToFrac(i->r());
		UnitCell::foldFrac(r);
		x = int(r.x/cellDelta.x);
		y = int(r.y/cellDelta.y);
		z = int(r.z/cellDelta.z);
		schemeGrid.setData(x,y,z,0.0);
		
		// Check around atom
		for (a=-atomExtent; a<=atomExtent; ++a)
		{
			for (b=-atomExtent; b<=atomExtent; ++b)
			{
				for (d=-atomExtent; d<=atomExtent; ++d)
				{
					r.set(a, b, d);
					if (r.magnitude() > atomExtent*1.1) continue;
					x2 = x + a;
					y2 = y + b;
					z2 = z + d;
					if (x2 < 0) x2 += gridSize.x;
					else if (x2 >= gridSize.x) x2 -= gridSize.x;
					if (y2 < 0) y2 += gridSize.y;
					else if (y2 >= gridSize.y) y2 -= gridSize.y;
					if (z2 < 0) z2 += gridSize.z;
					else if (z2 >= gridSize.z) z2 -= gridSize.z;
					data[x2][y2][z2] = 0.0;
				}
			}
		}
	}

	// The Grid data now contains 1.0 in each position which is free from any atoms
	// Partition this data up by finding an element which is -1.0 and selecting its encompassing region
	int nPartitions = 0, partitionSize;
	for (x=0; x<gridSize.x; ++x)
	{
		for (y=0; y<gridSize.y; ++y)
		{
			for (z=0; z<gridSize.z; ++z)
			{
				if (data[x][y][z] > -0.5) continue;
				
				// Found a cell containing -1.0 - select its encompassing region
				partitionSize = schemeGrid.modifyRegion(x, y, z, -1.5, -0.5, nPartitions+1.0, TRUE);
				
				// How big is this new region? Bigger than the minimum size requested?
				if (partitionSize < minimumSize)
				{
					// Not big enough, so run modifyRegion again and zero it
					schemeGrid.modifyRegion(x, y, z, nPartitions+0.5, nPartitions+1.5, 0.0, TRUE);
					msg.print("Found a partition of %i cells, which is below the threshold size of %i and will be ignored...\n", partitionSize, minimumSize);
				}
				else
				{
					++nPartitions;
					msg.print("Found a partition containing %i grid cells (volume = %f cubic Angstroms).\n", partitionSize, partitionSize * volumeElement);
				}
			}
		}
	}
	msg.print("Found %i partitions in the model.\n", nPartitions);
	rv.set(nPartitions);
	
	// Generate partitions from the new grid data
	scheme.createPartitionsFromGrid();

	// Copy to Disorder builder, if required...
	if (copyToBuilder)
	{
		aten.addPartitioningScheme(scheme);
		msg.print("Copied scheme to disorder builder.\n");
	}
	
	return TRUE;
}

// Drill pores in current model
bool Command::function_DrillPores(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	// Grab some parameters as variables to make readability easier
	double sizeParam = c->argd(1);
	int nA = c->argi(2), nB = c->argi(3), face = c->hasArg(4) ? c->argi(4) - 1 : 2;
	Vec3<double> v;
	if (c->hasArg(5)) v = c->arg3d(5);
	else v.set(0.0,0.0,1.0);
	
	// Determine origin face vectors, and determine first pore centre coordinates
	if ((face < 0) || (face > 2))
	{
		msg.print("Error: Origin face must be specified as 1 (YZ plane), 2 (XZ plane) or 3 (XY plane).\n");
		return FALSE;
	}
	Vec3<double> faceA = obj.rs()->cell()->axes().columnAsVec3((face+1)%3);
	Vec3<double> faceB = obj.rs()->cell()->axes().columnAsVec3((face+2)%3);
	Vec3<double> deltaA = faceA / nA, deltaB = faceB / nB;
	Vec3<double> origin = (deltaA + deltaB) * 0.5;
	
	obj.rs()->beginUndoState("Drill pores");
	for (int a = 0; a < nA; ++a)
	{
		for (int b = 0; b < nB; ++b)
		{
			obj.rs()->selectLine(v, origin + deltaA*a + deltaB*b, sizeParam);
		}
	}
	obj.rs()->selectionDelete();
	obj.rs()->endUndoState();

	return TRUE;
}

// Select pores atoms
bool Command::function_SelectPores(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Grab some parameters as variables to make readability easier
	double sizeParam = c->argd(1);
	int nA = c->argi(2), nB = c->argi(3), face = c->hasArg(4) ? c->argi(4) - 1 : 2;
	Vec3<double> v;
	if (c->hasArg(5)) v = c->arg3d(5);
	else v.set(0.0,0.0,1.0);
	
	// Determine origin face vectors, and determine first pore centre coordinates
	if ((face < 0) || (face > 2))
	{
		msg.print("Error: Origin face must be specified as 1 (YZ plane), 2 (XZ plane) or 3 (XY plane).\n");
		return FALSE;
	}
	Vec3<double> faceA = obj.rs()->cell()->axes().columnAsVec3((face+1)%3);
	Vec3<double> faceB = obj.rs()->cell()->axes().columnAsVec3((face+2)%3);
	Vec3<double> deltaA = faceA / nA, deltaB = faceB / nB;
	Vec3<double> origin = (deltaA + deltaB) * 0.5;
	
	obj.rs()->beginUndoState("Select pore atoms");
	obj.rs()->selectNone();
	for (int a = 0; a < nA; ++a)
	{
		for (int b = 0; b < nB; ++b)
		{
			obj.rs()->selectLine(v, origin + deltaA*a + deltaB*b, sizeParam);
		}
	}
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected());

	return TRUE;
}

// Terminate atoms with 'H' or 'OH'
bool Command::function_Terminate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	// Loop over atoms in current selection
	obj.rs()->selectNone(TRUE);
	Atom* i, *j;
	obj.rs()->beginUndoState("Terminate atoms");
	for (Refitem<Atom,int> *ri = obj.rs()->selection(); ri != NULL; ri = ri->next)
	{
		i = ri->item;
		switch (i->element())
		{
			// Oxygen
			case (8):
				if (i->nBonds() == 0)
				{
					msg.print(" ... Warning: Found unbound oxygen in selection ...\n");
					obj.rs()->selectAtom(i, TRUE);
				}
				else if (i->nBonds() == 1) obj.rs()->growAtom(i, 1, 1.0, Atom::TetrahedralGeometry);
				break;
			// Silicon
			case (14):
				if (i->nBonds() == 0)
				{
					msg.print(" ... Warning: Found unbound silicon in selection ...\n");
					obj.rs()->selectAtom(i, TRUE);
				}
				else for (int n=i->nBonds(); n<4; ++n)
				{
					// Must grow an oxygen onto the silicon, and then add a hydrogen to the oxygen
					j = obj.rs()->growAtom(i, 8, 1.5, Atom::TetrahedralGeometry);
					if (j != NULL) obj.rs()->growAtom(j, 1, 1.0, Atom::TetrahedralGeometry);
				}
				break;
			default:
				msg.print(" ... Skipping atom %i (%s) in termination ...\n", i->id()+1, Elements().symbol(i));
				break;
		}
	}
	
	// Select any unbound atoms for inspection by the user
	if (obj.rs()->nMarked() != 0)
	{
		msg.print("Viable unbound atoms were found in the provided selection, and remain selected in the model.\n");
		obj.rs()->selectNone();
		obj.rs()->selectMarkedAtoms();
	}
	obj.rs()->endUndoState();
	
	return TRUE;
}
