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

// Render model atoms and bonds
void Canvas::renderModelAtoms()
{
	dbgBegin(Debug::Calls,"Canvas::renderModelAtoms");
	static Atom::DrawStyle style_i, renderstyle;
	static GLfloat ambient[4], diffuse[4];
	static Prefs::ColourScheme scheme;
	static double radius, rij, cval;
	static Vec3<double> ri, rj, rk, ijk;
	static Atom *i, *j;
	static Refitem<Bond,int> *bref;
	static Cell *cell;
	// Reproject atoms if necessary
	displayModel_->projectAll();

	renderstyle = prefs.renderStyle();
	scheme = prefs.colourScheme();
	cell = displayModel_->cell();
	
	// Set polygon fill mode and specular reflection
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, prefs.penColour(Prefs::SpecularColour));
	glMateriali(GL_FRONT, GL_SHININESS, prefs.shininess());

	for (i = displayModel_->atoms(); i != NULL; i = i->next)
	{
		// If the atom is hidden then move on to the next
		if (i->isHidden()) continue;
		// Push the current matrix, translate to the atoms coordinates and set the drawing colour
		glPushMatrix();
		  // Define atom colour
		  switch (scheme)
		  {
			case (Prefs::ElementScheme):
				elements.copyAmbientColour(i->element(), ambient);
				elements.copyDiffuseColour(i->element(), diffuse);
				break;
			case (Prefs::ChargeScheme):
				prefs.colourScale[0].colour(i->charge(), ambient);
				prefs.colourScale[0].colour(i->charge(), diffuse);
				break;
			case (Prefs::VelocityScheme):
				cval = i->v().magnitude();
				prefs.colourScale[1].colour(cval, ambient);
				prefs.colourScale[1].colour(cval, diffuse);
				break;
			case (Prefs::ForceScheme):
				cval = i->f().magnitude();
				prefs.colourScale[2].colour(cval, ambient);
				prefs.colourScale[2].colour(cval, diffuse);
				break;
		  }
		  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, ambient);
		  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, diffuse);
		  // Get position
		  ri = i->r();
		  glTranslated(ri.x,ri.y,ri.z);
		  // Grab atom style and toggle lighting state if Atom::IndividualStyle is the main drawing style
		  if (renderstyle == Atom::IndividualStyle)
		  {
			style_i = i->style();
			style_i == Atom::StickStyle ? glDisable(GL_LIGHTING) : glEnable(GL_LIGHTING);
		  }
		  else style_i = renderstyle;
		  // Get atom radius
		  radius = prefs.atomSize(style_i);
		  if (style_i == Atom::ScaledStyle) radius *= elements.atomicRadius(i);
		  /*
		  // Draw the atom.
		  // If the atom's style is Atom::StickStyle, then we only draw if it is unbound.
		  */
		  if (style_i == Atom::StickStyle)
		  {
			glColor3fv(ambient);
			i->isSelected() ? glLineWidth(3.0) : glLineWidth(1.0);
			if (i->nBonds() == 0) glCallList(list_[GLOB_STICKATOM]); 
		  }
		  else
		  {
			if (style_i == Atom::ScaledStyle)
			{
				// Get the sphere radius and push the matrix again
				//radius = prefs.screenRadius(i);
				glPushMatrix();
				  glScaled(radius,radius,radius);
				  glCallList(list_[GLOB_UNITATOM]); 
				glPopMatrix();
			}
			else style_i == Atom::SphereStyle ? glCallList(list_[GLOB_SPHEREATOM]) : glCallList(list_[GLOB_TUBEATOM]);
		  }
		  /*
		  // Draw the bonds.
		  // Render half bonds at each atom.
		  */
		  for (bref = i->bonds(); bref != NULL; bref = bref->next)
		  {
			j = bref->item->partner(i);
			if (j->isHidden()) continue;
			// We are centred on atom i, so get the vector to atom j. Its more useful to have the half-length of the bond, so scale by 0.5 too.
			rj = cell->mimd(j, ri);
			rij = rj.magnitude() * 0.5;
			rj *= 0.5;
			// Now determine what sort of bond we're going to draw
			if (style_i != Atom::StickStyle)
			{
				// Draw cylinder bonds.
				switch (bref->item->order())
				{
					case (Bond::Single):	// Single bond
						glCylinder(rj,rij,0);
						break;
					case (Bond::Double):	// Double bond
						ijk = i->findBondPlane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						glTranslated(ijk.x,ijk.y,ijk.z);
						glCylinder(rj,rij,0);
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						glCylinder(rj,rij,0);
						glTranslated(ijk.x,ijk.y,ijk.z);
						break;
					case (Bond::Triple):	// Triple bond
						ijk = i->findBondPlane(j,bref->item,rj);
						ijk *= 0.1;
						// Can now draw the bond. Displace each part of the bond +rk or -rk.
						glCylinder(rj,rij,0);
						glTranslated(ijk.x,ijk.y,ijk.z);
						glCylinder(rj,rij,0);
						glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
						glCylinder(rj,rij,0);
						glTranslated(ijk.x,ijk.y,ijk.z);
						break;
				}
			}
			else
			{
				// Draw stick bond(s)
				glBegin(GL_LINES);
				  switch (bref->item->order())
				  {
					case (Bond::Single):	// Single bond
						glVertex3d(0.0,0.0,0.0);
						glVertex3d(rj.x,rj.y,rj.z);
						break;
					case (Bond::Double):	// Double bond
						// Must define a plane in which the bond will lay
						ijk = i->findBondPlane(j,bref->item,rj);
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
					case (Bond::Triple):	// Triple bond
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
		  }
		glPopMatrix();
	}
	// Second pass to render selected sphere atoms (transparency)
	// Enable alpha component (if we weren't aliasing anyway)
	if (!prefs.hasGlOption(Prefs::LineAliasOption) && !prefs.hasGlOption(Prefs::PolyAliasOption)) glEnable(GL_BLEND);
	// Make sure lighting is on
	glEnable(GL_LIGHTING);
	for (i = displayModel_->atoms(); i != NULL; i = i->next)
	{
		// Grab atom style and toggle lighting state if Atom::IndividualStyle is the main drawing style
		renderstyle == Atom::IndividualStyle ? style_i = i->style() : style_i = renderstyle;
		// Skip stick, hidden or unselected atoms...
		if (style_i == Atom::StickStyle) continue;
		if (!i->isSelected()) continue;
		if (i->isHidden()) continue;
		// Define atom colours
		switch (scheme)
		{
			case (Prefs::ElementScheme):
				elements.copyAmbientColour(i->element(), ambient);
				elements.copyDiffuseColour(i->element(), diffuse);
				break;
			case (Prefs::ChargeScheme):
				prefs.colourScale[0].colour(i->charge(), ambient);
				prefs.colourScale[0].colour(i->charge(), diffuse);
				break;
			case (Prefs::VelocityScheme):
				prefs.colourScale[1].colour(i->charge(), ambient);
				prefs.colourScale[1].colour(i->charge(), diffuse);
				break;
			case (Prefs::ForceScheme):
				prefs.colourScale[2].colour(i->charge(), ambient);
				prefs.colourScale[2].colour(i->charge(), diffuse);
				break;
		}
		ambient[3] = ambient[3] / 2.0f;
		diffuse[3] = diffuse[3] / 2.0f;
		glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, ambient);
		glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, diffuse);
		glPushMatrix();
		  ri = i->r();
		  glTranslated(ri.x,ri.y,ri.z);
		  // Draw on the transparent atom and its bonds
		  if (style_i == Atom::ScaledStyle)
		  {
			radius = prefs.screenRadius(i);
			glPushMatrix();
			  glScalef(radius,radius,radius);
			  glCallList(list_[GLOB_SELUNITATOM]);
			glPopMatrix();
		  }
		  else style_i == Atom::SphereStyle ? glCallList(list_[GLOB_SELSPHEREATOM]) : glCallList(list_[GLOB_SELTUBEATOM]);
		  for (bref = i->bonds(); bref != NULL; bref = bref->next)
		  {
			j = bref->item->partner(i);
			if (j->isHidden()) continue;
			// We are centred on atom i, so get the vector to atom j. Its more useful to have the half-length of the bond, so scale by 0.5 too.
			rj = cell->mimd(j, ri);
			rij = rj.magnitude() * 0.5;
			rj *= 0.5;
			// Draw cylinder bonds.
			switch (bref->item->order())
			{
				case (Bond::Single):	// Single bond
					glCylinder(rj,rij,1);
					break;
				case (Bond::Double):	// Double bond
					ijk = i->findBondPlane(j,bref->item,rj);
					ijk *= 0.1;
					// Can now draw the bond. Displace each part of the bond +rk or -rk.
					glTranslated(ijk.x,ijk.y,ijk.z);
					glCylinder(rj,rij,1);
					glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
					glCylinder(rj,rij,1);
					glTranslated(ijk.x,ijk.y,ijk.z);
					break;
				case (Bond::Triple):	// Triple bond
					ijk = i->findBondPlane(j,bref->item,rj);
					ijk *= 0.1;
					// Can now draw the bond. Displace each part of the bond +rk or -rk.
					glCylinder(rj,rij,1);
					glTranslated(ijk.x,ijk.y,ijk.z);
					glCylinder(rj,rij,1);
					glTranslated(-2.0*ijk.x,-2.0*ijk.y,-2.0*ijk.z);
					glCylinder(rj,rij,1);
					glTranslated(ijk.x,ijk.y,ijk.z);
					break;
			}
		  }
		glPopMatrix();
	}
	// Turn off blending (if not antialiasing)
	if (!prefs.hasGlOption(Prefs::LineAliasOption) && !prefs.hasGlOption(Prefs::PolyAliasOption)) glDisable(GL_BLEND);
	// Reset line width to 1.0
	glLineWidth(1.0);
	dbgEnd(Debug::Calls,"Canvas::renderModelAtoms");
}
