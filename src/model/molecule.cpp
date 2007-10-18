/*
	*** Model molecule functions
	*** src/model/molecule.cpp

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

#include "model/model.h"
#include "classes/pattern.h"
#include "base/elements.h"

// Position molecule at specified coordinates
void model::position_molecule(pattern *p, int mol, const vec3<double> &v)
{
	dbg_begin(DM_CALLS,"model::position_molecule");
	static vec3<double> newpos, cog;
	static int pnatoms, offset, n;
	atom **modelatoms = get_staticatoms();
	pnatoms = p->get_natoms();
	offset = p->get_startatom() + pnatoms * mol;
	msg(DM_VERBOSE,"model::position_molecule : Moving %i atoms starting at %i (config can hold %i atoms)\n", pnatoms, offset, atoms.size());
	if (offset < atoms.size())
	{
		cog = p->calculate_cog(this,mol);
		for (n=offset; n<offset+pnatoms; n++)
		{
			// Get local coordinates of atom - mim with and then subtract centre of geometry
			//newpos = modelatoms[n];
			newpos = cell.mim(modelatoms[n]->r,cog);
			newpos -= cog;
			// Re-position
			newpos += v;
			// Store new positions
			modelatoms[n]->r = newpos;
		}
	}
	else printf("model::position_molecule : Requested a molecule past end of config contents. (%s %i)\n",p->get_name(),mol); 
	dbg_end(DM_CALLS,"model::position_molecule");
}

// Translate molecule along vector
void model::translate_molecule(pattern *p, int mol, const vec3<double> &v)
{
	// Vector 'v' should be normalised before passing
	dbg_begin(DM_CALLS,"model::translate_molecule");
	static int pnatoms, offset, n;
	atom **modelatoms = get_staticatoms();
	pnatoms = p->get_natoms();
	offset = p->get_startatom() + pnatoms * mol;
	msg(DM_VERBOSE,"model::translate_molecule : Moving %i atoms starting at %i (%i atoms currently in model)\n", pnatoms, offset, atoms.size());
	if (offset < atoms.size()) for (n=offset; n<offset+pnatoms; n++) modelatoms[n]->r += v;
	else printf("model::translate_molecule : Requested a molecule past end of model contents. (%s %i)\n", p->get_name(), mol); 
	dbg_end(DM_CALLS,"model::translate_molecule");
}

void model::rotate_molecule(pattern *p, int mol, double rotx, double roty)
{
	// Rotate the coordinates of the atoms in pattern p, molecule mol, about their centre of geometry.
	// rotx and roty are the rotations about the x and y axes respectively, in degrees
	dbg_begin(DM_CALLS,"model::rotate_molecule");
	static double cosx, cosy, sinx, siny;
	static mat3<double> rotmat;
	static vec3<double> delta, newpos, cog;
	static int pnatoms, offset, n;
	atom **modelatoms = get_staticatoms();
	rotx /= DEGRAD;
	roty /= DEGRAD;
	cosx = cos(rotx);
	cosy = cos(roty);
	sinx = sin(rotx);
	siny = sin(roty);
	rotmat.set(0,cosy,0.0,siny);
	rotmat.set(1,(-sinx)*(-siny),cosx,(-sinx)*cosy);
	rotmat.set(2,cosx*(-siny),sinx,cosx*cosy);
	pnatoms = p->get_natoms();
	offset = p->get_startatom() + pnatoms * mol;
	// Calculate COG before we start
	cog = p->calculate_cog(this,mol);
	//printf("rotate_molecule : Moving %i atoms starting at %i (%i atoms currently in config)\n",pnatoms,offset,natoms);
	if (offset < atoms.size())
		for (n=offset; n<offset+pnatoms; n++)
		{
			// Get local coordinates of atom, i.e. subtract centre of geometry
			newpos = rotmat * (modelatoms[n]->r - cog);
			newpos += cog;
			// Store the new position
			modelatoms[n]->r = newpos;
		}
	else printf("model::rotate_molecule : Requested a molecule past end of model contents. (%s %i)\n", p->get_name(), mol); 
	dbg_end(DM_CALLS,"model::rotate_molecule");
}

// Set the hidden flag on atoms of the specified molecule
void model::hide_molecule(pattern *p, int mol, bool visible)
{
	dbg_begin(DM_CALLS,"model::hide_molecule");
	static int pnatoms, offset, n;
	atom **modelatoms = get_staticatoms();
	pnatoms = p->get_natoms();
	offset = p->get_startatom() + pnatoms * mol;
	if (offset < atoms.size()) for (n=offset; n<offset+pnatoms; n++) modelatoms[n]->set_hidden(visible);
	else printf("model::hide_molecule : Requested a molecule past end of model contents. (%s %i)\n", p->get_name(), mol); 
	dbg_end(DM_CALLS,"model::hide_molecule");
}
