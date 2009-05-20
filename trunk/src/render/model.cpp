/*
	*** Model parts rendering
	*** src/render/model.cpp
	Copyright T. Youngs 2007-2009

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

#include "gui/canvas.h"
#include "model/model.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/sysfunc.h"

// Render atom labels
void Canvas::renderModelLabels()
{
	msg.enter("Canvas::renderModelLabels");
	// Annotate the model with 2D labels
	static char text[64];
	static Atom *i;
	static int labels;
	static ForcefieldAtom *ffa;
	static Vec3<double> cellCentre;
	// If we have a unit cell we must account for the origin translation
	cellCentre = displayModel_->cell()->centre();
	for (Atom *i = displayModel_->atoms(); i != NULL; i = i->next)
	{
		// Check if atom has labels and is visible
		if ((!i->hasLabels()) || (i->isHidden())) continue;
		labels = i->labels();
		ffa = i->type();
		// Blank label string
		text[0] = '\0';
		// Now add on all parts of the label that are required
		if (labels&Atom::IdLabel)
		{
			strcat(text,itoa(i->id()+1));
			strcat(text," ");
		}
		if (labels&Atom::ElementLabel)
		{
			strcat(text, elements().symbol(i));
			strcat(text," ");
		}
		if (labels&Atom::TypeLabel)
		{
			strcat(text,"[");
			if (ffa == NULL) strcat(text, "None");
			else
			{
				strcat(text,itoa(ffa->typeId()));
				strcat(text," ");
				strcat(text,ffa->name());
			}
			strcat(text,"] ");
		}
		if (labels&Atom::EquivLabel)
		{ 
			strcat(text,"[=");
			strcat(text,(ffa == NULL ? "None" : ffa->equivalent()));
			strcat(text,"] ");
		}
		if (labels&Atom::ChargeLabel)
		{
			strcat(text,"(");
			strcat(text,ftoa(i->charge()));
			strcat(text," e)");
		}
		//glText(i->r() - cellCentre, text);
		// Add text object to list
		displayModel_->projectAtom(i);
		if (i->rScreen().z < 1.0)
		{
			TextObject *to = new TextObject((int)i->rScreen().x, int(height_ - i->rScreen().y), FALSE, text);
			textObjects_.own(to);
		}
	}
	msg.exit("Canvas::renderModelLabels");
}

// Render measurements
void Canvas::renderModelMeasurements()
{
	msg.enter("Canvas::renderModelMeasurements");
	static Vec3<double> ri, rj, rk, rl, labpos, cellCentre, rji, rjk;
	static Vec3<double> pos1, pos2;
	double gamma, t;
	int i;
	bool rightalign, skip;
	static char text[256];
	static Atom **atoms;
	// Grab cell origin to get correct positioning
	cellCentre = -displayModel_->cell()->centre();
	glPushMatrix();
	  glTranslated(cellCentre.x, cellCentre.y, cellCentre.z);
	  // Go through list of measurements
	  for (Measurement *m = displayModel_->measurements(); m != NULL; m = m->next)
	  {
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		skip = FALSE;
		for (i=0; i<Measurement::nMeasurementAtoms(m->type()); i++)
		{
			if (atoms[i]->isHidden())
			{
				skip = TRUE;
				break;
			}
		}
		if (skip) continue;
		switch (m->type())
		{
			case (Measurement::Distance):
				ri = atoms[0]->r();
				rj = atoms[1]->r();
				labpos = (ri + rj) * 0.5;
				glBegin(GL_LINE_STRIP);
				  glVertex3d(ri.x, ri.y, ri.z);
				  glVertex3d(rj.x, rj.y, rj.z);
				glEnd();
				rightalign = FALSE;
				sprintf(text,"%f %s", m->value(), prefs.distanceLabel());
				break;
			case (Measurement::Angle):
				ri = atoms[0]->r();
				rj = atoms[1]->r();
				rk = atoms[2]->r();
				glBegin(GL_LINE_STRIP);
				  glVertex3d(ri.x, ri.y, ri.z);
				  glVertex3d(rj.x, rj.y, rj.z);
				  glVertex3d(rk.x, rk.y, rk.z);
				glEnd();
				// Angle marker oblongata
// 				rji = ri - rj;
// 				rjk = rk - rj;
				labpos = (rji + rjk) * 0.2 + rj;
// 				rji = rji * 0.2 + rj;
// 				rjk = rjk * 0.2 + rj;
// 				glBegin(GL_LINE_STRIP);
// 				  glVertex3d(rji.x, rji.y, rji.z);
// 				  glVertex3d(labpos.x, labpos.y, labpos.z);
// 				  glVertex3d(rjk.x, rjk.y, rjk.z);
// 				glEnd();
				// Curved angle marker
				rji = (ri - rj);
				rjk = (rk - rj);
				rji.normalise();
				rjk.normalise();
				gamma = acos(rji.dp(rjk));
				// Draw segments
				t = 0.0;
				glBegin(GL_LINES);
				  for (int n=0; n<11; n++)
				  {
					pos1 = rji * (sin((1.0-t)*gamma) / sin(gamma)) + rjk * (sin(t*gamma) / sin(gamma));
					pos1 *= 0.2;
					pos1 += rj;
					glVertex3d(pos1.x, pos1.y, pos1.z);
					t += 0.1;
				  }
				glEnd();
				// Determine orientation of text
				pos1 = displayModel_->modelToScreen(labpos);
				pos2 = displayModel_->modelToScreen(rj);
				rightalign = (pos1.x < pos2.x ? TRUE : FALSE);
				sprintf(text,"%f %s", m->value(), prefs.angleLabel());
				break;
			case (Measurement::Torsion):
				ri = atoms[0]->r();
				rj = atoms[1]->r();
				rk = atoms[2]->r();
				rl = atoms[3]->r();
				glBegin(GL_LINE_STRIP);
				  glVertex3d(ri.x, ri.y, ri.z);
				  glVertex3d(rj.x, rj.y, rj.z);
				  glVertex3d(rk.x, rk.y, rk.z);
				  glVertex3d(rl.x, rl.y, rl.z);
				glEnd();
				labpos = (rj + rk) * 0.5;
				rightalign = FALSE;
				sprintf(text,"%f Deg", m->value());
				break;
		}
		// Add text object to list
		pos1 = displayModel_->modelToScreen(labpos);
		if (pos1.z < 1.0)
		{
			TextObject *to = new TextObject(int(pos1.x), int(height_ - pos1.y), rightalign, text);
			textObjects_.own(to);
		}
	  }
	glPopMatrix();
	msg.exit("Canvas::renderModelMeasurements");
}

// Render force arrows
void Canvas::renderModelForceArrows()
{
	msg.enter("Canvas::renderModelForceArrows");
	for (Atom *i = displayModel_->atoms(); i != NULL; i = i->next)
	{
		// Scale forces to more reasonable values  TODO User scaling
		glArrow(i->r(),i->f() / 30.0);
	}
	msg.exit("Canvas::renderModelForceArrows");
}

// Render model cell
void Canvas::renderModelCell()
{
	// Draw the unit cell of the model
	GLfloat fgcol[4];
	prefs.copyColour(Prefs::ForegroundColour, fgcol);
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, fgcol);
	glColor4fv(fgcol);
	glLineWidth(1.0f);
	static Vec3<double> cellCentre, lengths;
	static Mat4<double> matrix;
	Cell *cell = displayModel_->cell();
	if (cell->type() != Cell::NoCell)
	{
		// All cell types are transformations of a unit cube.
		// So, multiply modelview matrix by cell axes matrix and draw a unit cube
		//Mat4<double> mat = displayModel_->get_cell()->get_transpose_as_mat4();
		//mat.get_column_major(glmat);
		GLdouble glmat[16];
		displayModel_->cell()->axesForGl(glmat);
		glPushMatrix();
		  glMultMatrixd(glmat);
		  if (prefs.isVisibleOnScreen(Prefs::ViewCell)) glCallList(list_[GLOB_WIREUNITCUBE]);
		  lengths = displayModel_->cell()->lengths();
		  // Render cell axis arrows
		  if (prefs.isVisibleOnScreen(Prefs::ViewCellAxes))
		  {
			glTranslated(-0.5,-0.5,-0.5);
			glScaled(1.0/lengths.x,1.0/lengths.y,1.0/lengths.z);
			glCallList(list_[GLOB_CELLAXES]);
		  }
		glPopMatrix();
		// Here, translate the initial drawing position to be 0,0,0 in cell coordinates
		cellCentre = -cell->centre();
		glTranslated(cellCentre.x,cellCentre.y,cellCentre.z);
	}
}
