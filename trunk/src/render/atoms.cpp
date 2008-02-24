/*
	*** Atom/bond rendering
	*** src/render/atoms.cpp
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
#include "base/elements.h"
#include "gui/canvas.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// Render model atoms and bonds
void canvas::render_model_atoms()
{
	dbg_begin(DM_CALLS,"canvas::render_model_atoms");
	static draw_style style_i, renderstyle;
	static GLint ambient[4], diffuse[4];
	static short int cindex;
	static atom_colours scheme;
	static double radius, rij;
	static vec3<double> ri, rj, rk, ijk;
	static atom *i, *j;
	static refitem<bond,int> *bref;
	static unitcell *cell;
	// Reproject atoms if necessary
	displaymodel->project_all();

	renderstyle = prefs.render_style;
	scheme = prefs.get_colour_scheme();
	i = displaymodel->get_atoms();
	cell = displaymodel->get_cell();
	
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
		  ri = i->r();
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
						gl_cylinder(rj,rij,i->is_selected()*2);
						break;
					case (BT_DOUBLE):	// Double bond
						ijk = i->find_bond_plane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						glTranslated(ijk.x,ijk.y,ijk.z);
						gl_cylinder(rj,rij,i->is_selected()*2);
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						gl_cylinder(rj,rij,i->is_selected()*2);
						glTranslated(ijk.x,ijk.y,ijk.z);
						break;
					case (BT_TRIPLE):	// Triple bond
						ijk = i->find_bond_plane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						gl_cylinder(rj,rij,i->is_selected()*2);
						glTranslated(ijk.x,ijk.y,ijk.z);
						gl_cylinder(rj,rij,i->is_selected()*2);
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						gl_cylinder(rj,rij,i->is_selected()*2);
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
		glMaterialiv(GL_FRONT_AND_BACK, GL_AMBIENT, ambient);
		glMaterialiv(GL_FRONT_AND_BACK, GL_DIFFUSE, diffuse);
		glPushMatrix();
		  ri = i->r();
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
	dbg_end(DM_CALLS,"canvas::render_model_atoms");
}
