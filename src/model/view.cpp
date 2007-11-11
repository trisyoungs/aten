/*
	*** Model view functions
	*** src/model/view.cpp
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
#include "gui/gui.h"

// Adjust Camera
void model::adjust_camera(double dx, double dy, double dz, double angle)
{
	// Adjust the models camera variables
	dbg_begin(DM_CALLS,"model::adjust_camera");
	double sincam, coscam;
	camr.x += dx;
	camr.y += dy;
	camr.z += dz;
	camrot = camrot + angle;
	if (camrot > 2.0*PI) camrot -= 2.0*PI;
	coscam = cos(camrot);
	sincam = sin(camrot);
	// Now create the new matrix
	camera.rows[0].set(coscam,-sincam,0.0,camr.x*coscam-camr.y*sincam);
	camera.rows[1].set(sincam,coscam,0.0,camr.x*sincam-camr.y*coscam);
	camera.rows[2].set(0.0,0.0,1.0,camr.z);
	camera.rows[3].set(0.0,0.0,0.0,1.0);
	calculate_viewmatrix();
	// Log camera change
	log_change(LOG_CAMERA);
	dbg_end(DM_CALLS,"model::adjust_camera");
}

// Adjust orthographic size
void model::adjust_ortho_size(double delta)
{
	dbg_begin(DM_CALLS,"model::adjust_ortho_size");
	#ifdef HAS_GUI
		ortho_size += delta;
		if (ortho_size < 1.0) ortho_size = 1.0;
		if (ortho_size > 50.0) ortho_size = 50.0;
		calculate_viewmatrix();
		gui.mainview.do_projection();
		// Log camera change
		log_change(LOG_CAMERA);
	#endif
	dbg_end(DM_CALLS,"model::adjust_ortho_size");
}

// Reset Camera
void model::reset_camera(const vec3<double> &newr)
{
	// Adjust the models camera variables
	dbg_begin(DM_CALLS,"model::reset_camera");
	#ifdef HAS_GUI
		camr = newr;
		camrot = 0.0;
		// Now create the new matrix
		camera.rows[0].set(1.0,0.0,0.0,camr.x);
		camera.rows[1].set(0.0,1.0,0.0,camr.y);
		camera.rows[2].set(0.0,0.0,1.0,camr.z);
		camera.rows[3].set(0.0,0.0,0.0,1.0);
		// Recalculate viewing matrix
		calculate_viewmatrix();
		// Log camera change
		log_change(LOG_CAMERA);
	#endif
	dbg_end(DM_CALLS,"model::reset_camera");
}

// Reset View
void model::reset_view()
{
	// Reset the modelview matrix and the camera
	dbg_begin(DM_CALLS,"model::reset_view");
	#ifdef HAS_GUI
		static vec3<double> newcam, newscreen;
		atom *i, target;
		double z, largest = 0.0;
		rotation.set_identity();
		ortho_size = 20.0;
		// Fit model to screen
		// Crude approach - find largest coordinate and zoom out so that {0,0,largest} is visible on screen
		for (i = atoms.first(); i != NULL; i = i->next)
		{
			z = i->r.absmax();
			if (z > largest) largest = z;
		}
		target.r = cell.get_origin();
		target.r.add(0.0,0.0,cell.get_lengths().z+largest);
		newcam.set(0.0,0.0,0.0);
		reset_camera(newcam);
		// Now, adjust camera matrix so that this atom is on-screen.
		// Need to do a check for the viability of the canvas first...
		if (gui.mainview.is_valid())
			do
			{
				// Project our local atom and grab the z screen coordinate
				calculate_viewmatrix();
				project_atom(&target);
				z = target.get_world_coords().z;
				adjust_camera(0.0,0.0,-1.0,0.0);
			} while (z > -5.0);
		// Recalculate viewing matrix
		calculate_viewmatrix();
		// Log camera change
		log_change(LOG_CAMERA);
	#endif
	dbg_end(DM_CALLS,"model::reset_view");
}

// Rotate free
void model::rotate(double dx, double dy)
{
	// Rotate the whole system by the amounts specified.
	dbg_begin(DM_CALLS,"model::rotate");
	static double rotx, roty, theta, sinx, cosx, siny, cosy;
	static mat4<double> newrotmat, oldrotmat;
	camrot > PI ? theta = camrot-2.0*PI : theta = camrot;
	// Account for the orientation of the current camera up vector.
	rotx = (dx*sin(theta) + dy*cos(theta) ) / DEGRAD;
	roty = (dx*cos(theta) - dy*sin(theta) ) / DEGRAD;
	// Calculate cos/sin terms for needless speedup!
	cosx = cos(rotx);
	cosy = cos(roty);
	sinx = sin(rotx);
	siny = sin(roty);
	newrotmat.rows[0].set(cosy,0.0,siny,0.0);
	newrotmat.rows[1].set((-sinx)*(-siny),cosx,(-sinx)*cosy,0.0);
	newrotmat.rows[2].set(cosx*(-siny),sinx,cosx*cosy,0.0);
	newrotmat.rows[3].set(0.0,0.0,0.0,1.0);
	oldrotmat = rotation;
	// Now, multiply our matrices together...
	rotation = newrotmat * oldrotmat;
	// Recalculate view matrix
	calculate_viewmatrix();
	// Log camera change
	log_change(LOG_CAMERA);
	dbg_end(DM_CALLS,"model::rotate");
}

// Rotate Z-axis
void model::zrotate(double dx)
{
	// Rotate about the perceived z-axis by changing the up vector of the camera.
	dbg_begin(DM_CALLS,"model::zrotate");
	static vec3<double> zero;
	dx = (dx / DEGRAD ) * 2.0;
	adjust_camera(zero,dx);
	dbg_end(DM_CALLS,"model::zrotate");
}

// Calculate View Matrix
void model::calculate_viewmatrix()
{
	// Calculate full viewing matrix
	view = camera * rotation;
	// Calculate inverse
	view_inverse = view;
	view_inverse.invert();
}

// Project the coordinates of all atoms in the model
void model::project_all()
{
	// Transform the model coordinates of all atoms into world GL and 2D screen coordinates
	dbg_begin(DM_CALLS,"model::project_all");
	#ifdef HAS_GUI
		if (projection_point != (logs[LOG_COORDS] + logs[LOG_CAMERA]))
		{
			if (gui.mainview.is_valid()) for (atom *i = atoms.first(); i != NULL; i = i->next) project_atom(i);
			projection_point = logs[LOG_COORDS] + logs[LOG_CAMERA];
		}
	#endif
	dbg_end(DM_CALLS,"model::project_all");
}

// Project the coordinates of all selected atoms in the model
void model::project_selection()
{
	dbg_begin(DM_CALLS,"model::project_selection");
	#ifdef HAS_GUI
		if (gui.mainview.is_valid()) for (atom *i = atoms.first(); i != NULL; i = i->next) if (i->is_selected()) project_atom(i);
	#endif
	dbg_end(DM_CALLS,"model::project_selection");
}

// Project the coordinates of a single atom in the model
void model::project_atom(atom *i)
{
	// Transform the model coordinates of specified atom into world GL and 2D screen coordinates
	dbg_begin(DM_MORECALLS,"model::project_atom");
	#ifdef HAS_GUI
		if (!gui.mainview.is_valid())
		{
			dbg_end(DM_MORECALLS,"model::project_atom");
			return;
		}
		static vec4<double> modelr, screenr, worldr;
		static double srx, sry, srz;
		static GLint *vmat;
		// Projection formula is : worldr = P x M x modelr 
		modelr.set(i->r, 1.0);
		// We also need to add on the cell origin (which basically means subtract a half-cell in 3D)
		modelr += cell.get_origin();
		// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
		worldr = view * modelr;
		i->set_world_coords(worldr);
		// Calculate 2D screen coordinates - Multiply world coordinates by P
		screenr = gui.mainview.PMAT * worldr;
		screenr.x /= screenr.w;
		screenr.y /= screenr.w;
		srz = screenr.z / screenr.w;
		vmat = gui.mainview.VMAT;
		srx = vmat[0] + vmat[2]*(screenr.x+1)/2.0;
		sry = vmat[1] + vmat[3]*(screenr.y+1)/2.0;
		i->set_screen_coords(srx,sry,srz);
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		worldr.x += prefs.screenradius(i);
		screenr = gui.mainview.PMAT * worldr;
		screenr.x /= screenr.w;
		screenr.y /= screenr.w;
		i->set_screen_radius(fabs( (vmat[0] + vmat[2]*(screenr.x+1)/2.0) - srx));
	#endif
	dbg_end(DM_MORECALLS,"model::project_atom");
}

vec4<double> &model::world_to_screen(const vec3<double> &v)
{
	// Project the supplied world coordinates into screen coordinates.
	// The returned vec4's 'w' component is the unit 'radius' at that point.
	dbg_begin(DM_CALLS,"model::world_to_screen");
	static vec4<double> modelr, screenr, worldr, result;
	static double x1,x2,radius;
	static GLint *vmat;
	#ifdef HAS_GUI
		screenr.zero();
		if (!gui.mainview.is_valid())
		{
			dbg_end(DM_CALLS,"model::world_to_screen");
			return screenr;
		}
		// Projection formula is : worldr = P x M x modelr
		// Get the 3D coordinates of the atom - Multiply by modelview matrix 'view'
		modelr.set(v.x, v.y, v.z, 1.0);
		worldr = view * modelr;
		// Calculate 2D 'radius' of the atom - Multiply worldr[x+delta] coordinates by P
		screenr = gui.mainview.PMAT * worldr;
		screenr.x /= screenr.w;
		screenr.y /= screenr.w;
		result = screenr;
		vmat = gui.mainview.VMAT;
		x1 = vmat[0] + vmat[2]*(screenr.x+1)/2.0;
		worldr.x += 1.0;
		screenr = gui.mainview.PMAT * worldr;
		screenr.x /= screenr.w;
		x2 = vmat[0] + vmat[2]*(screenr.x+1)/2.0;
		radius = fabs(x2 - x1);
		// Store info and return
		result.w = radius * 2.0;
	#endif
	dbg_end(DM_CALLS,"model::world_to_screen");
	return result;
}

vec3<double> model::guide_to_model(const vec3<double> &sr)
{
	// Convert the screen coordinates passed to a position on the drawing guide, and then into model coordinates
	dbg_begin(DM_CALLS,"model::guide_to_model");
	static vec4<double> guidepoint;
	static vec3<double> newpoint;
	double radius, depth;
	#ifdef HAS_GUI

		depth = prefs.get_draw_depth();
		// First, project a point at the guide z-position into screen coordinates to get the guide 'yardstick'
		newpoint.set(0.0,0.0,depth);
		guidepoint = world_to_screen(newpoint);
		radius = guidepoint.w;
		// Now, calculate the position of the clicked point on the guide
		newpoint.x = sr.x - (gui.mainview.get_width() / 2.0 );
		newpoint.y = (gui.mainview.get_height() - sr.y) - (gui.mainview.get_height() / 2.0 );
		newpoint /= radius;
		newpoint.z = depth + camr.z;
		// Convert this world coordinate into model coordinates by multiplying by the inverse of the PM matrix.
		newpoint *= view_inverse;
		// Also need to account for periodic systems (which are translated so the cell midpoint is centred in the screen) by subtracting the cell.origin coordinates.
		newpoint -= cell.get_origin();
	#endif
	dbg_end(DM_CALLS,"model::guide_to_model");
	return newpoint;
}


