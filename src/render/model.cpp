/*
	*** Model rendering
	*** src/render/model.cpp
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

#include "classes/forcefield.h"
#include "base/elements.h"
#include "base/master.h"
#include "methods/mc.h"
#include "gui/canvas.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// Render model atoms and bonds
void canvas_master::render_model_atoms()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_atoms");
	static draw_style style_i, renderstyle;
	static GLint ambient[4], diffuse[4];
	static short int cindex;
	static atom_colours scheme;
	static double radius, rij;
	static vec3<double> ri, rj, rk, ijk;
	static atom *i, *j;
	static refitem<bond> *bref;
	static unitcell *cell;
	// Reproject atoms if necessary
	displaymodel->project_all();

	renderstyle = prefs.render_style;
	scheme = prefs.get_colour_scheme();
	i = displaymodel->get_atoms();
	cell = &displaymodel->cell;
	
	// Set polygon fill mode and specular reflection
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glMaterialiv(GL_FRONT, GL_SPECULAR, prefs.get_colour(COL_SPECREFLECT));
	glMateriali(GL_FRONT, GL_SHININESS, prefs.gl_shininess);

	while (i != NULL)
	{
		// If the atom is hidden then move on to the next
		if (i->is_hidden()) { i = i->next; continue; }
		// Check if its a drawing object and not an element
		if (i->get_element() > 118) { i = i->next; continue; }
		// Push the current matrix, translate to the atoms coordinates and set the drawing colour
		glPushMatrix();
		  // Define atom colours
		  if (scheme == AC_ELEMENT)
		  {
			cindex = i->get_element();
			elements.ambient(cindex, ambient);
			elements.diffuse(cindex, diffuse);
		  }
		  else
		  {
			printf("Colour scale selection for atoms is not yet done.\n");
			cindex = 0;
			prefs.get_scale_colour(cindex, ambient);
			prefs.get_scale_colour(cindex, diffuse);
		  }
		  glMaterialiv(GL_FRONT, GL_AMBIENT, ambient);
		  glMaterialiv(GL_FRONT, GL_DIFFUSE, diffuse);
		  // Get position
		  ri = i->r;
		  glTranslated(ri.x,ri.y,ri.z);
		  // Grab atom style and toggle lighting state if DS_INDIVIDUAL is the main drawing style
		  if (renderstyle == DS_INDIVIDUAL)
		  {
			style_i = i->get_style();
		  	style_i == DS_STICK ? glDisable(GL_LIGHTING) : glEnable(GL_LIGHTING);
		  }
		  else style_i = renderstyle;
		  /*
		  // Draw the atom.
		  // If the atom's style is DS_STICK, then we only draw if it is unbound.
		  */
		  if (style_i == DS_STICK)
		  {
			glColor3iv(ambient);
			i->is_selected() ? glLineWidth(3.0) : glLineWidth(1.0);
			if (i->get_nbonds() == 0) glCallList(list[GLOB_STICKATOM]); 
		  }
		  else
		  {
			if (style_i == DS_SCALED)
			{
				// Get the sphere radius and push the matrix again
				radius = prefs.screenradius(i);
				glPushMatrix();
				  glScaled(radius,radius,radius);
				  glCallList(list[GLOB_UNITATOM]); 
				glPopMatrix();
			}
			else style_i == DS_SPHERE ? glCallList(list[GLOB_SPHEREATOM]) : glCallList(list[GLOB_TUBEATOM]);
		  }
		  /*
		  // Draw the bonds.
		  // Render half bonds at each atom.
		  */
		  bref = i->get_bonds();
		  while (bref != NULL)
		  {
			j = bref->item->get_partner(i);
			if (j->is_hidden()) { bref = bref->next; continue; }
			// We are centred on atom i, so get the vector to atom j. Its more useful to have the half-length of the bond, so scale by 0.5 too.
			rj = cell->mimd(j, ri);
			rij = rj.magnitude() * 0.5;
			rj *= 0.5;
			// Now determine what sort of bond we're going to draw
			if (style_i != DS_STICK)
			{
				// Draw cylinder bonds.
				// Change sign of z-coordinate
				switch (bref->item->type)
				{
					case (BT_SINGLE):	// Single bond
						gl_cylinder(rj,rij,i->is_selected());
						break;
					case (BT_DOUBLE):	// Double bond
						ijk = i->find_bond_plane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						glTranslated(ijk.x,ijk.y,ijk.z);
						gl_cylinder(rj,rij,i->is_selected());
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						gl_cylinder(rj,rij,i->is_selected());
						glTranslated(ijk.x,ijk.y,ijk.z);
						break;
					case (BT_TRIPLE):	// Triple bond
						ijk = i->find_bond_plane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						gl_cylinder(rj,rij,i->is_selected());
						glTranslated(ijk.x,ijk.y,ijk.z);
						gl_cylinder(rj,rij,i->is_selected());
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						gl_cylinder(rj,rij,i->is_selected());
						glTranslated(ijk.x,ijk.y,ijk.z);
						break;
				}
			}
			else
			{
				// Draw stick bond(s)
				glBegin(GL_LINES);
				  switch (bref->item->type)
				  {
					case (BT_SINGLE):	// Single bond
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						break;
					case (BT_DOUBLE):	// Double bond
						// Must define a plane in which the bond will lay
						ijk = i->find_bond_plane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						glTranslated(ijk.x,ijk.y,ijk.z);
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						glTranslated(ijk.x,ijk.y,ijk.z);
						break;
					case (BT_TRIPLE):	// Triple bond
						// Draw the components arbitrarily oriented
						rk = rj;
						rk.x = rk.y;
						rk.y = rk.z;
						rk.z = rj.x;
						rk.normalise();
						rk *= 0.1;
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						glTranslated(rk.x,rk.y,rk.z);
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						glTranslated(-2.0*rk.x,-2.0*rk.y,-2.0*rk.z);
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						glTranslated(rk.x,rk.y,rk.z);
						break;
				  }
				glEnd();
			}
			bref = bref->next;
		  }
		glPopMatrix();
		i = i->next;
	}
	// End the GL_LINES command
	//glEnd();
	// Second pass to render selected sphere atoms (transparency)
	// Enable alpha component (if we weren't aliasing anyway)
	if (!prefs.get_gl_option(GO_LINEALIASING) && !prefs.get_gl_option(GO_POLYALIASING)) glEnable(GL_BLEND);
	glEnable(GL_LIGHTING);		// Make sure lighting is on
	for (i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		// Grab atom style and toggle lighting state if DS_INDIVIDUAL is the main drawing style
		renderstyle == DS_INDIVIDUAL ? style_i = i->get_style() : style_i = renderstyle;
		// Skip stick, hidden or unselected atoms...
		if (style_i == DS_STICK) continue;
		if (!i->is_selected()) continue;
		if (i->is_hidden()) continue;
		// Define atom colours
		if (scheme == AC_ELEMENT)
		{
			cindex = i->get_element();
			elements.ambient(cindex, ambient);
			elements.diffuse(cindex, diffuse);
		}
		else
		{
			printf("Colour scale selection for atoms is not yet done.\n");
			cindex = 0;
			prefs.get_scale_colour(cindex, ambient);
			prefs.get_scale_colour(cindex, diffuse);
		}
		ambient[3] = ambient[3] / 2;
		diffuse[3] = diffuse[3] / 2;
		glMaterialiv(GL_FRONT, GL_AMBIENT, ambient);
		glMaterialiv(GL_FRONT, GL_DIFFUSE, diffuse);
		glPushMatrix();
		  ri = i->r;
		  glTranslated(ri.x,ri.y,ri.z);
		  // Draw on the transparent highlight
		  if (style_i == DS_SCALED)
		  {
			radius = prefs.screenradius(i);
			glPushMatrix();
			  glScalef(radius,radius,radius);
			  glCallList(list[GLOB_SELUNITATOM]);
			glPopMatrix();
		  }
		  else style_i == DS_SPHERE ? glCallList(list[GLOB_SELSPHEREATOM]) : glCallList(list[GLOB_SELTUBEATOM]);
		glPopMatrix();
	}
	// Turn off blending (if not antialiasing)
	if (!prefs.get_gl_option(GO_LINEALIASING) && !prefs.get_gl_option(GO_POLYALIASING)) glDisable(GL_BLEND);
	// Reset line width to 1.0
	glLineWidth(1.0);
	dbg_end(DM_CALLS,"canvas_master::render_model_atoms");
}

// Render model objects
void canvas_master::render_model_objects()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_objects");
	// Render other elemental objects in the model
	int el;
	for (atom *i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		el = i->get_element();
		if (el <= 118) continue;
		glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, elements.ambient(i->get_element()));
		switch (el)
		{
			case (119): 	// Ellipsoid - coords = coords, velocities = lookat, forces = scaling
				gl_ellipsoid(i->r,i->v,i->f);
				break;
		}
	}
	dbg_end(DM_CALLS,"canvas_master::render_model_objects");
}

// Render atom labels
void canvas_master::render_model_labels()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_labels");
	// Annotate the model with 2D labels
	static char text[64];
	static atom *i;
	static int labels;
	static ffatom *ffa;
	static vec3<double> cellorigin;
	// If we have a unit cell we must account for the origin translation
	cellorigin = displaymodel->cell.get_origin();
	for (atom *i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		// Check if atom has labels
		if (!i->has_labels()) continue;
		labels = i->get_labels();
		ffa = i->get_fftype();
		// Blank label string
		text[0] = '\0';
		// Now add on all parts of the label that are required
		if (labels&AL_ID)
		{
			strcat(text,itoa(i->get_id()+1));
			strcat(text," ");
		}
		if (labels&AL_ELEMENT)
		{
			strcat(text,elements.symbol(i));
			strcat(text," ");
		}
		if (labels&AL_FFTYPE)
		{
			strcat(text,"[");
			strcat(text,ffa == NULL ? "None" : ffa->get_name());
			strcat(text,"] ");
		}
		if (labels&AL_FFEQUIV)
		{ 
			strcat(text,"[=");
			strcat(text,(ffa == NULL ? "None" : ffa->get_equiv()));
			strcat(text,"] ");
		}
		if (labels&AL_CHARGE)
		{
			strcat(text,"(");
			strcat(text,ftoa(i->get_charge()));
			strcat(text," e)");
		}
		textbitmap(cellorigin + i->r,text);
	}
	dbg_end(DM_CALLS,"canvas_master::render_model_labels");
}

// Render measurements
void canvas_master::render_model_measurements()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_measurements");
	static vec3<double> ri, rj, rk, rl, labpos, cellorigin;
	static char text[256];
	static atom **atoms;
	// Grab cell origin to get correct positioning
	cellorigin = displaymodel->cell.get_origin();
	glPushMatrix();
	  glTranslated(cellorigin.x, cellorigin.y, cellorigin.z);
	  // Go through list of measurements
	  for (measurement *m = displaymodel->get_measurements(); m != NULL; m = m->next)
	  {
		atoms = m->get_atoms();
		glBegin(GL_LINE_STRIP);
		  switch (m->get_type())
		  {
			case (GT_DISTANCE):
				ri = atoms[0]->r;
				rj = atoms[1]->r;
				labpos = (ri + rj) * 0.5;
				glVertex3d(ri.x, ri.y, ri.z);
				glVertex3d(rj.x, rj.y, rj.z);
				sprintf(text,"%f A",m->get_value());
				break;
			case (GT_ANGLE):
				ri = atoms[0]->r;
				rj = atoms[1]->r;
				rk = atoms[2]->r;
				labpos = rj;
				glVertex3d(ri.x, ri.y, ri.z);
				glVertex3d(rj.x, rj.y, rj.z);
				glVertex3d(rk.x, rk.y, rk.z);
				sprintf(text,"%f Deg",m->get_value());
				break;
			case (GT_TORSION):
				ri = atoms[0]->r;
				rj = atoms[1]->r;
				rk = atoms[2]->r;
				rl = atoms[3]->r;
				glVertex3d(ri.x, ri.y, ri.z);
				glVertex3d(rj.x, rj.y, rj.z);
				glVertex3d(rk.x, rk.y, rk.z);
				glVertex3d(rl.x, rl.y, rl.z);
				labpos = (rj + rk) * 0.5;
				sprintf(text,"%f Deg",m->get_value());
				break;
		  }
		glEnd();
		// Draw on label
		textbitmap(labpos, text);
	  }
	glPopMatrix();
	dbg_end(DM_CALLS,"canvas_master::render_model_measurements");
}

// Render other 3D objects
void canvas_master::render_model_3d()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_3d");
	// Draw an 3D embellishments for active modes
	double radius;
	static vec3<double> worldr;
	static vec3<double> tempv;
	draw_style dstyle;
	atom *i;
	// Draw on the selection highlights (for atoms in canvas.subsel)
	gl_subsel_3d();
	// Other modes
	switch (activemode)
	{
		case (UA_PICKRADIAL):
			// Draw on the bounding sphere of the selection	
			i = atom_hover;
			if (i == NULL) break;
			// Work out the radius of the sphere
			tempv = r_mousedown - r_mouseup;
			radius = tempv.x * tempv.y;
			radius /= i->get_screen_radius();
			// Convert the pixel radius into model coordinate radius. We will have the selection 'hotspot'
			// radius of the atom from its screen projection, which itself depends on the drawing style...
			radius *= prefs.screenradius(i);
			worldr = i->get_world_coords();
			glPushMatrix();
		  	  glTranslatef(worldr.x,worldr.y,worldr.z);
			  glScalef(radius,radius,radius);
			  glCallList(list[GLOB_SELSPHEREATOM]);
			glPopMatrix();
			break;
		case (UA_GEOMSELECT):
		case (UA_GEOMDIST):
		case (UA_GEOMANGLE):
		case (UA_GEOMTORSION):
			break;
	}
	dbg_end(DM_CALLS,"canvas_master::render_model_3d");
}

// Render 2D objects
void canvas_master::render_model_2d()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_2d");
	// Draw on any 2D objects, e.g. selection boxes, labels etc.
	int dx,dy,n;
	// First set up a 2D drawing area...
	glMatrixMode(GL_PROJECTION);		// Swap to projection matrix...
	glLoadIdentity();			// ...clear it...
	gluOrtho2D(0.0,w,0.0,h);	// ...and setup a 2D canvas.
	// Now draw
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	// We add various 2D features depending on the current interaction mode.
	switch (activemode)
	{
		case (UA_NONE): break;
		case (UA_PICKSELECT):
		case (UA_GEOMSELECT):
		case (UA_POSSELECT):
			// Only selection mode where we draw a selection box
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(1,0x5555);
			gl_rectangle(r_mousedown.x,h-r_mousedown.y,r_mouselast.x,h-r_mouselast.y);
			glDisable(GL_LINE_STIPPLE);
			break;
		case (UA_GEOMDIST):
		case (UA_GEOMANGLE):
		case (UA_GEOMTORSION):
			// Draw line from last atom in selection list (if any) to the current mouse pos
			break;
	}
	// If the mouse is hovering over an atom, draw a circle around it...
	if (atom_hover != NULL)
	{
		vec3<double> hoverpos = ((atom*) atom_hover)->get_screen_coords();
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
	dbg_end(DM_CALLS,"canvas_master::render_model_2d");
}

// Render model regions
void canvas_master::render_model_regions()
{
	// Draw on insertion regions
	static vec3<double> centre, size;
	static GLint colour[4];
	int i = 0;
	// Enable alpha component and make sure lighting is on
	glEnable(GL_BLEND);
	glEnable(GL_LIGHTING);
	for (component *c = mc.components.first(); c != NULL; c = c->next)
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
				glutSolidCube(1.0);
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
}

// Render force arrows
void canvas_master::render_model_forcearrows()
{
	dbg_begin(DM_CALLS,"canvas_master::render_model_forcearrows");
	static vec3<double> f;
	for (atom *i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		// Grab force vector from atom
		f = i->f;
		// Scale forces to more reasonable values
		f /= 30.0;
		gl_arrow(i->r,f);
	}
	dbg_end(DM_CALLS,"canvas_master::render_model_forcearrows");
}

// Render model cell
void canvas_master::render_model_cell()
{
	// Draw the unit cell of the model
	glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, prefs.colours[COL_PEN]);
	glColor3iv(prefs.colours[COL_PEN]);
	glLineWidth(1.0f);
	static vec3<double> origin;
	if (displaymodel->cell.get_type() != CT_NONE)
	{
		// All cell types are transformations of a unit cube.
		// So, multiply modelview matrix by cell axes matrix and draw a unit cube
		mat4<double> mat = displaymodel->cell.get_axes_as_mat4();
		double glmat[16];
		mat.get_column_major(glmat);
		glPushMatrix();
		  glMultMatrixd(glmat);
		  if (prefs.should_render(VO_CELL)) glCallList(list[GLOB_WIREUNITCUBE]);
		  vec3<double> l = displaymodel->cell.get_lengths();
		  glTranslated(-0.5,-0.5,-0.5);
		  glScaled(1.0/l.x,1.0/l.y,1.0/l.z);
		  if (prefs.should_render(VO_CELLAXES)) glCallList(list[GLOB_CELLAXES]);
		glPopMatrix();
		// Here, translate the initial drawing position to be 0,0,0 in cell coordinates
		origin = displaymodel->cell.get_origin();
		glTranslated(origin.x,origin.y,origin.z);
	}
}
