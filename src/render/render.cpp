/*
	*** Master rendering routines
	*** src/render/render.cpp
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

#include "base/elements.h"
#include "base/master.h"
#include "gui/canvas.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// Render model
void canvas_master::render_scene(model *source)
{
	dbg_begin(DM_CALLS,"canvas_master::render_scene");
	static double rotmat[16], cammat[16];
	static model *trajparent;
	static double camrot;

	// Begin the GL commands
	if (!begin_gl())
	{
		dbg_end(DM_CALLS,"canvas_master::render");
		return;
	}

	// Check the supplied model against the previous one rendered to see if we must outdate the display list
	if ((source != displaymodel) || (source == NULL)) render_point = -1;

	// Store the source model pointer and grab the trajectoryparent pointer (if there is one)
	displaymodel = source;
	trajparent = source->get_trajparent();

	if (displaymodel == NULL)
	{
		// Select projection matrix and load the identity matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		// Set up a 2D canvas
		gluOrtho2D(0.0,w,0.0,h);
		// Draw on our default message
		glMatrixMode(GL_MODELVIEW);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		glRasterPos2i(1,(int)h-13);
		textbitmap(1.0,h-10.0,"No model to display.");
		dbg_end(DM_CALLS,"canvas_master::render");
		return;
	}

	// Clear colour and depth buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Grab rotation & camera matrices, and camera rotation for the model. If we're displaying a trajectory frame, grab the parent's matrix instead.
	if (trajparent == NULL)
	{
		displaymodel->get_rotation_matrix(rotmat);
		displaymodel->get_camera_matrix(cammat);
		camrot = displaymodel->get_camrot();
	}
	else
	{
		trajparent->get_rotation_matrix(rotmat);
		trajparent->get_camera_matrix(cammat);
		camrot = trajparent->get_camrot();
	}

	// Draw on the rotation globe
	if (prefs.should_render(VO_GLOBE)) render_rotation_globe(rotmat, camrot);

	// Reset projection matrix and set perspective view
	double top, bottom;
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	if (prefs.render_perspective)
	{
		bottom = tan(prefs.render_fov / DEGRAD) * prefs.gl_clip_near;
		top = -bottom;
		glFrustum(aspect*top,aspect*bottom,top,bottom,prefs.gl_clip_near,prefs.gl_clip_far);
	}
	else
	{
		bottom = displaymodel->get_ortho_size();
		top = -bottom;
		glOrtho(aspect*top,aspect*bottom,top,bottom,-prefs.gl_clip_far,prefs.gl_clip_far);
	}

	// Reset GLs modelview matrix and apply camera matrix from model
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glMultMatrixd(cammat);

	// Draw guide if visible
	if (prefs.build_show_guide)
	{
		glTranslatef(0.0f,0.0f,-prefs.build_draw_depth);
		glCallList(list[GLOB_GUIDE]);
		glTranslatef(0.0f,0.0f,prefs.build_draw_depth);
	}

	// Apply model's rotation matrix (which we grabbed earlier)
	glMultMatrixd(rotmat);

	// Set the initial state of lighting in the model
	prefs.render_style == DS_STICK ? glDisable(GL_LIGHTING) : glEnable(GL_LIGHTING);

	// Draw the main model parts
	// If render_point matches the model's total change point (from get_point()) then just re-render the stored display list. If not, create the display list.
	glPushMatrix();
	  if (render_point == displaymodel->get_log(LOG_TOTAL)) glCallList(list[GLOB_MODEL]);
	  else
	  {
		msg(DM_VERBOSE,"Recreating display list for model '%s'...", displaymodel->get_name());
		glDeleteLists(list[GLOB_MODEL],1);
		glNewList(list[GLOB_MODEL],GL_COMPILE_AND_EXECUTE);
		  // Draw the model cell (this also translates our drawing position to the -half cell point.
		  render_model_cell();
		  // Draw the model's atoms, bonds, and selection
		  if (prefs.should_render(VO_ATOMS)) render_model_atoms();
		  // Render any other objects associated with the model
		  render_model_glyphs();
		  // Render force arrows
		  if (prefs.should_render(VO_FORCEARROWS)) render_model_forcearrows();
		glEndList();
		render_point = displaymodel->get_log(LOG_TOTAL);
		msg(DM_VERBOSE," Done. (New point = %i)\n",render_point);
	  }
	  // Render surfaces
	  if (prefs.should_render(VO_SURFACES)) render_surfaces();
	  // Render MC regions
	  if ((displaymodel->get_celltype() != CT_NONE) && prefs.should_render(VO_REGIONS)) render_regions();
	  glColor3iv(prefs.colours[COL_PEN]);
	  render_extra_3d();
	glPopMatrix();

	// Draw replicated cells (using display list)
	if (prefs.should_render(VO_CELLREPEAT))
	{
		static mat3<double> cellmat;
		static vec3<double> cx, cy, cz;
		cellmat = displaymodel->get_cellaxes();
		cx = cellmat.rows[0];
		cy = cellmat.rows[1];
		cz = cellmat.rows[2];
		for (int i=-prefs.get_repcellneg(0); i<=prefs.get_repcellpos(0); i++)
		{
			glPushMatrix();
			glTranslated(i*cx.x,i*cx.y,i*cx.z);
			for (int j=-prefs.get_repcellneg(1); j<=prefs.get_repcellpos(1); j++)
			{
				glPushMatrix();
				glTranslated(j*cy.x,j*cy.y,j*cy.z);
				for (int k=-prefs.get_repcellneg(2); k<=prefs.get_repcellpos(2); k++)
				{
					if ((i == 0) && (j == 0) && (k == 0)) continue;
					glPushMatrix();
					glTranslated(k*cz.x,k*cz.y,k*cz.z);
					glCallList(list[GLOB_MODEL]);
					glPopMatrix();
				}
				glPopMatrix();
			}
			glPopMatrix();
		}
	}

	// Render measurements / labels, also in 3D
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glMaterialiv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, prefs.colours[COL_PEN]);
	glDisable(GL_LIGHTING);

	if (prefs.should_render(VO_LABELS)) render_model_labels();
	if (prefs.should_render(VO_MEASUREMENTS)) render_model_measurements();

	render_extra_2d();
	glDisable(GL_COLOR_MATERIAL);

	glFlush();
	swap_buffers();
	end_gl();
	dbg_end(DM_CALLS,"canvas_master::render");
}

