/*
	*** Model transformation functions
	*** src/model/transform.cpp
	Copyright T. Youngs 2007

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
#include "classes/atom.h"
#include "base/master.h"
#include "base/prefs.h"

// Variables
vec3<double> cog, localcog;
double unitrad;

// Prepare model for atom transform
void model::prepare_transform()
{
	// Called at the beginning of a transform operation, prior to mouse movement
	// Take the first selected atom to be the initial reference point to calculate
	// the centre of geometry. Then, mim every other selected atom so that the
	// atomic positions are all minimum image to the reference (so transforms
	// work properly in periodic systems). We re-fold the positions on mouse-up.
	int nadded;
	// Return if no cog could be defined (i.e. no atoms selected)
	dbg_begin(DM_CALLS,"model::prepare_transform");
	if (nselected < 1)
	{
		dbg_end(DM_CALLS,"model::prepare_transform");
		return;
	}
	cog.zero();
	// Reference point for mim will be the updating cog
	for (atom *i = atoms.first(); i != NULL; i = i->next) if (i->is_selected()) cog += i->get_world_coords();
	cog /= nselected;
	// Calculate a unit radius for the centre of geometry
	localcog = cog;
	vec4<double> pvec = world_to_screen(localcog);
	unitrad = pvec.w;
	dbg_end(DM_CALLS,"model::prepare_transform");
}

// Finalize Model Manipulation
void model::finalize_transform()
{
	// Called after mouse-up.
	// Atom positions may have moved outside the boundaries of the box, so
	// need to re-fold. ANYTHING ELSE?
	fold_all_atoms();
	log_change(LOG_COORDS);
	project_all();
}

// Free Rotation of Selection
void model::rotate_selection_world(double dx, double dy)
{
	// Rotate the selection about the calculated centre of geometry.
	// We are passed the 2D-movement of the mouse, which we use to generate a rotation matrix.
	// We then apply this to the stored *world* coordinates of 
	// the selected atoms, which we then unproject to get the new model coordinates.
	dbg_begin(DM_CALLS,"model::rotate_selection_world");
	static double rotx, roty, theta, cosx, cosy, sinx, siny;
	static vec3<double> origin, newr;
	static mat3<double> rotmat;
	rotx = dy/10.0;
	roty = dx/10.0;
	cosx = cos(rotx);
	cosy = cos(roty);
	sinx = sin(rotx);
	siny = sin(roty);
	rotmat.set(0,cosy,0.0,siny);
	rotmat.set(1,-sinx*-siny,cosx,-sin(rotx)*cos(roty));
	rotmat.set(2,cosx*-siny,sinx,cosx*cosy);
	origin = cell.get_origin();
	// Now, make the rotation 
	for (atom* i = atoms.first(); i != NULL; i = i->next)
	{
		if (!i->is_selected()) continue;
		// Rotate this atom's position about the geometric centre of all selected atoms.
		newr = i->get_world_coords() - localcog;
		newr = (rotmat * newr) + localcog;
		i->r = (view_inverse * newr) - origin;
	}
	log_change(LOG_VISUAL);
	project_selection();
	dbg_end(DM_CALLS,"model::rotate_selection_world");
}

// Rotate about defined vector
void model::rotate_selection_vector(vec3<double> origin, vec3<double> vector, double step)
{
	dbg_begin(DM_CALLS,"model::rotate_selection_vector");
	static mat3<double> r, u, ut, gr, Igr;
	vec3<double> tempv;
	int n,m,o;
	atom *i = get_first_selected();
	if (i == NULL)
	{
		msg(DM_NONE,"No atoms selected!\n");
		dbg_end(DM_CALLS,"model::rotate_selection_vector");
		return;
	}
	// Generate target coordinate system, defined from xaxis == v and orthogonal vectors from first atom
	vector.normalise();
	u.rows[0] = vector;
	tempv = i->r - origin;
	tempv.normalise();
	u.rows[1] = tempv - vector * tempv.dp(vector);
	u.rows[1].normalise();
	u.rows[2] = vector * u.rows[1];
	u.rows[2].normalise();
	ut = u.transpose();

	// Create rotation matrix
	step /= DEGRAD;
	r.set(0,1.0,0.0,0.0);
	r.set(1,0.0,cos(step),sin(step));
	r.set(2,0.0,-sin(step),cos(step));

	// Create grand rotation matrix
	gr = ut * r * u;
	Igr.set_identity();
	Igr = Igr - gr;

	// Loop over atoms
	while (i != NULL)
	{
		tempv = gr * i->r;
		tempv += Igr * origin;
		i->r = tempv;
		i = i->get_next_selected();
	}
	log_change(LOG_STRUCTURE);
	dbg_end(DM_CALLS,"model::rotate_selection_vector");
}

// Rotation of selection about screen Z-axis
void model::manip_rotate_zaxis(double dz)
{
	// Rotate about the perceived z-axis by changing the up vector of the camera.
	dbg_begin(DM_CALLS,"model::manip_rotate_zaxis");
	//GLdouble newx, newy;
	//dx = (dx / DEGRAD ) * 2.0f;
	//master.activemodel->adjust_camera(0.0,0.0,0.0,dx);
	//master.activemodel->mmat_transform_all();
	dbg_end(DM_CALLS,"model::manip_rotate_zaxis");
}

// Translate Selection in world coordinates
void model::translate_selection_world(const vec3<double> &v)
{
	// Translate the selected atoms in the local XY plane
	dbg_begin(DM_CALLS,"model::translate_selection_world");
	static vec3<double> origin, newr;
	// No need to account for orientation / rotation of view, since we do the transformation in world coordinates.
	// So, take the local coordinates of each selected atom and add our position delta to it.
	// We then unproject this new local coordinate to get the new model (world) coordinate.
	// Grab unit cell origin
	origin = cell.get_origin();
	for (atom *i = get_first_selected(); i != NULL; i = i->get_next_selected())
	{
		newr = i->get_world_coords();
		newr.add(v.x,v.y,v.z);
		newr = (view_inverse * newr) - origin;
		i->r = newr;
	}
	log_change(LOG_VISUAL);
	project_selection();
	dbg_end(DM_CALLS,"model::translate_selection_world");
}

// Move selected atoms in local space
void model::translate_selection_local(const vec3<double> &tvec)
{
	// Translate the model's current selection by the vector supplied.
	dbg_begin(DM_CALLS,"model::translate_selection_local");
	for (atom *i = get_first_selected(); i != NULL; i = i->get_next_selected()) i->r += tvec;
	log_change(LOG_VISUAL);
	dbg_end(DM_CALLS,"model::translate_selection_local");
}

// Mirror selection in local coordinates
void model::mirror_selection_local(int axis)
{
	dbg_begin(DM_CALLS,"model::mirror_selection_local");
	// Get selection's local COG
	vec3<double> cog = selection_get_cog();
	vec3<double> mimd;
	for (atom *i = get_first_selected(); i != NULL; i = i->get_next_selected())
	{
		// Get coordinates relative to COG
		mimd = cell.mimd(i->r, cog);
		// Flip specified coordinate
		mimd.set(axis, -mimd.get(axis));
		// Store new coordinate
		i->r = mimd + cog;
	}
	log_change(LOG_VISUAL);
	project_selection();
	dbg_end(DM_CALLS,"model::mirror_selection_local");
}
