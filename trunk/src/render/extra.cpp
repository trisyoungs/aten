/*
	*** Extra rendering
	*** src/render/extra.cpp
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

#include "model/model.h"
#include "base/master.h"
#include "base/elements.h"
#include "methods/mc.h"
#include "gui/canvas.h"

// Render other 3D objects
void canvas::render_extra_3d()
{
	dbg_begin(DM_CALLS,"canvas::render_extra_3d");
	// Draw on 3D embellishments for active modes
	static double radius;
	static vec3<double> r, mouse;
	static vec3<double> tempv;
	static atom *i;
	// Draw on the selection highlights (for atoms in canvas.subsel)
	gl_subsel_3d();
	// Other modes
	switch (activemode)
	{
		// Draw on the bounding sphere of a radial selection
		case (UA_PICKRADIAL):
			i = atom_hover;
			if (i == NULL) break;
			// Work out the radius of the sphere
			tempv = r_mousedown - r_mouseup;
			radius = tempv.x * tempv.y;
			radius /= i->get_screen_radius();
			// Convert the pixel radius into model coordinate radius. We will have the selection 'hotspot'
			// radius of the atom from its screen projection, which itself depends on the drawing style...
			radius *= prefs.screenradius(i);
			r = i->worldr();
			glPushMatrix();
			  glTranslatef(r.x,r.y,r.z);
			  glScalef(radius,radius,radius);
			  glCallList(list[GLOB_SELSPHEREATOM]);
			glPopMatrix();
			break;
		// Draw on bond and new atom for chain drawing
		case (UA_DRAWCHAIN):
			if (atom_hover == NULL) break;
			r = atom_hover->r();
			// We need to project a point from the mouse position onto the canvas plane, unless the mouse is over an existing atom in which case we snap to its position instead
			i = displaymodel->atom_on_screen(r_mouselast.x, r_mouselast.y);
			if (i == NULL) mouse = displaymodel->guide_to_model(r_mouselast);
			else mouse = i->r();
			mouse -= r;
			glPushMatrix();
			  glTranslated(r.x,r.y,r.z);
			  // Determine how we'll draw the new bond / atom
			  if (prefs.render_style == DS_STICK)
			  {
				// Simple - draw line from atom_hover to mouse position
				glBegin(GL_LINES);
				  glVertex3d(0.0,0.0,0.0);
				  glVertex3d(mouse.x,mouse.y,mouse.z);
				glEnd();
			  }
			  else
			  {
				gl_cylinder(mouse,mouse.magnitude(),1);
				glTranslated(mouse.x, mouse.y, mouse.z);
				switch (prefs.render_style)
				{
					case (DS_TUBE):
						glCallList(list[GLOB_WIRETUBEATOM]);
						break;
					case (DS_SPHERE):
						glCallList(list[GLOB_WIRESPHEREATOM]);
						break;
					case (DS_SCALED):
						glCallList(list[GLOB_WIRESPHEREATOM]);
						break;
				}
			  }
			glPopMatrix();
			break;
	}
	dbg_end(DM_CALLS,"canvas::render_extra_3d");
}

// Render 2D objects
void canvas::render_extra_2d()
{
	dbg_begin(DM_CALLS,"canvas::render_extra_2d");
	// Draw on any 2D objects, e.g. selection boxes, labels etc.
	static int n, i;
	static double dx, dy, halfw;
	// First set up a 2D drawing area...
	glMatrixMode(GL_PROJECTION);		// Swap to projection matrix...
	glLoadIdentity();			// ...clear it...
	gluOrtho2D(0.0,w,0.0,h);	// ...and setup a 2D canvas.
	// Now draw
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	// We add various 2D features depending on the current interaction mode.
	// Some we draw when the mode is active, some whtn it is just selected
	// Those for active modes (when button is down)...
	switch (activemode)
	{
		case (UA_NONE):
			break;
		// Only selection mode where we draw a selection box
		case (UA_PICKSELECT):
		case (UA_GEOMSELECT):
		case (UA_POSSELECT):
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(1,0x5555);
			gl_rectangle(r_mousedown.x,h-r_mousedown.y,r_mouselast.x,h-r_mouselast.y);
			glDisable(GL_LINE_STIPPLE);
			break;
		// Draw line from last atom in selection list (if any) to the current mouse pos
		case (UA_GEOMDIST):
		case (UA_GEOMANGLE):
		case (UA_GEOMTORSION):
			break;
	}
	// ...and those for selected modes (whether the button is down or not).
	switch (selectedmode)
	{
		// Draw on distance ruler for drawing modes
		case (UA_DRAWATOM):
		case (UA_DRAWCHAIN):
			// Get angstrom length
			dx = 1.0 / drawpixelwidth;
			halfw = w / 2.0;
			i = int( halfw / dx);
			if (i < 2) break;
			glBegin(GL_LINES);
			  for (n = -i; n <= i; n++)
			  {
				glVertex2d(halfw + n*dx, 10);
				glVertex2d(halfw + n*dx, 20);
				glVertex2d(halfw + (n+0.5)*dx, 10);
				glVertex2d(halfw + (n+0.5)*dx, 15);
			  }
			  glVertex2d(halfw - i*dx, 11);
			  glVertex2d(halfw + i*dx, 11);
			glEnd();
			for (n = -i; n < 0; n++) textbitmap(halfw + n*dx - 8, 1, itoa(n));
			for (n = 0; n <= i; n++) textbitmap(halfw + n*dx - 3, 1, itoa(n));
			break;
	}
	// If the mouse is hovering over an atom, draw a circle around it...
	if (atom_hover != NULL)
	{
		vec3<double> hoverpos = ((atom*) atom_hover)->screenr();
		gl_circle(hoverpos.x,hoverpos.y,((atom*) atom_hover)->get_screen_radius());
	}
	// Add text
	//textbitmap(1.0,h-12.0,displaymodel->get_name());
	// Draw on colour scale if necessary
	if (prefs.get_colour_scheme() != AC_ELEMENT)
	{
		float midy = h / 2;
		//glBegin(
	}
	dbg_end(DM_CALLS,"canvas::render_extra_2d");
}

// Render disordered insertion regions
void canvas::render_regions()
{
	dbg_begin(DM_CALLS,"canvas::render_regions");
	static vec3<double> centre, size;
	static GLint colour[4];
	int i = 0;
	// Enable alpha component and make sure lighting is on
	glEnable(GL_BLEND);
	glEnable(GL_LIGHTING);
	for (component *c = master.mc.components.first(); c != NULL; c = c->next)
	{
		elements.ambient(i, colour);
		colour[3] = (GLint) (0.4 * INT_MAX);
		glMaterialiv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,colour);
		glPushMatrix();
		  centre = c->area.get_centre();
		  size = c->area.get_size();
		  switch (c->area.get_shape())
		  {
			case (RS_CELL):
				break;
			case (RS_CUBOID):
				glTranslated(centre.x,centre.y,centre.z);
				glScaled(size.x,size.y,size.z);
				glCallList(list[GLOB_UNITCUBE]);
				break;
			case (RS_SPHEROID):
				glTranslated(centre.x,centre.y,centre.z);
				glScaled(size.x,size.y,size.z);
				glCallList(list[GLOB_UNITATOM]);
				break;
			default:
				printf("render_model_regions :: Region type not done.\n");
				break;
		  }
		glPopMatrix();
		i ++;
	}
	// Turn off blending (if not antialiasing)
	if (!prefs.get_gl_option(GO_LINEALIASING) && !prefs.get_gl_option(GO_POLYALIASING)) glDisable(GL_BLEND);
	glDisable(GL_LIGHTING);
	dbg_end(DM_CALLS,"canvas::render_regions");
}
