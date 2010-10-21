/*
	*** Model parts rendering
	*** src/render/model.cpp
	Copyright T. Youngs 2007-2010

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

#include "render/canvas.h"
#include "model/model.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/sysfunc.h"

// Render atom labels
void Canvas::renderModelLabels(Model *sourceModel)
{
	msg.enter("Canvas::renderModelLabels");
	// Annotate the model with 2D labels
	Dnchar text;
	static int labels;
	static ForcefieldAtom *ffa;
	static Vec3<double> cellCentre;
	// Check for valid model
	if (sourceModel == NULL)
	{
		printf("NULL Model passed to Canvas::renderModelLabels\n");
		msg.exit("Canvas::renderModelLabels");
		return;
	}
	// If we have a unit cell we must account for the origin translation
	cellCentre = sourceModel->cell()->centre();
	for (Atom *i = sourceModel->atoms(); i != NULL; i = i->next)
	{
		// Check if atom has labels and is visible
		if ((!i->hasLabels()) || (i->isHidden())) continue;
		labels = i->labels();
		ffa = i->type();
		// Blank label string
		text.clear();
		// Now add on all parts of the label that are required
		if (labels&(1 << Atom::IdLabel))
		{
			text.strcat(itoa(i->id()+1));
			text += ' ';
		}
		if (labels&(1 << Atom::ElementLabel))
		{
			text.strcat(elements().symbol(i));
			text += ' ';
		}
		if (labels&(1 << Atom::TypeLabel))
		{
			text += '[';
			if (ffa == NULL) text.strcat("None");
			else
			{
				text.strcat(itoa(ffa->typeId()));
				text += ' ';
				text.strcat(ffa->name());
			}
			text.strcat("] ");
		}
		if (labels&(1 << Atom::EquivLabel))
		{ 
			text.strcat("[=");
			text.strcat(ffa == NULL ? "None" : ffa->equivalent());
			text.strcat("] ");
		}
		if (labels&(1 << Atom::ChargeLabel))
		{
			text += '(';
			text.strcat(ftoa(i->charge()));
			text.strcat(" e)");
		}
		//glText(i->r() - cellCentre, text);
		// Add text object to list
		sourceModel->projectAtom(i);
		if (i->rScreen().z < 1.0)
		{
			TextObject *to = new TextObject((int)i->rScreen().x, int(height_ - i->rScreen().y), FALSE, text);
			textObjects_.own(to);
		}
	}
	msg.exit("Canvas::renderModelLabels");
}

// Render measurements
void Canvas::renderModelMeasurements(Model *sourceModel)
{
	msg.enter("Canvas::renderModelMeasurements");
	static Vec3<double> ri, rj, rk, rl, labpos, cellCentre, rji, rjk;
	static Vec3<double> pos1, pos2;
	double gamma, t;
	bool rightalign;
	static Dnchar text;
	static Atom **atoms;
	// Check for valid model
	if (sourceModel == NULL)
	{
		printf("NULL Model passed to Canvas::renderModelMeasurements\n");
		msg.exit("Canvas::renderModelMeasurements");
		return;
	}
	// Grab cell origin to get correct positioning
	cellCentre = -sourceModel->cell()->centre();
	glPushMatrix();
	  glTranslated(cellCentre.x, cellCentre.y, cellCentre.z);
	  // Distances
	  for (Measurement *m = sourceModel->distanceMeasurements(); m != NULL; m = m->next)
	  {
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden()) continue;
		ri = atoms[0]->r();
		rj = atoms[1]->r();
		labpos = (ri + rj) * 0.5;
		glBegin(GL_LINE_STRIP);
		  glVertex3d(ri.x, ri.y, ri.z);
		  glVertex3d(rj.x, rj.y, rj.z);
		glEnd();
		text.sprintf("%f %s", m->value(), prefs.distanceLabel());
		// Add text object to list
		pos1 = sourceModel->modelToScreen(labpos);
		if (pos1.z < 1.0)
		{
			TextObject *to = new TextObject(int(pos1.x), int(height_ - pos1.y), FALSE, text);
			textObjects_.own(to);
		}
	  }
	  // Angles
	  for (Measurement *m = sourceModel->angleMeasurements(); m != NULL; m = m->next)
	  {
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden()) continue;
		ri = atoms[0]->r();
		rj = atoms[1]->r();
		rk = atoms[2]->r();
		glBegin(GL_LINE_STRIP);
		  glVertex3d(ri.x, ri.y, ri.z);
		  glVertex3d(rj.x, rj.y, rj.z);
		  glVertex3d(rk.x, rk.y, rk.z);
		glEnd();
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
		labpos = (rji + rjk) * 0.2 + rj;
		pos1 = sourceModel->modelToScreen(labpos);
		pos2 = sourceModel->modelToScreen(rj);
		rightalign = (pos1.x < pos2.x ? TRUE : FALSE);
		text.sprintf("%f %s", m->value(), prefs.angleLabel());
		// Add text object to list
		pos1 = sourceModel->modelToScreen(labpos);
		if (pos1.z < 1.0)
		{
			TextObject *to = new TextObject(int(pos1.x), int(height_ - pos1.y), rightalign, text);
			textObjects_.own(to);
		}
	  }
	  // Torsions
	  for (Measurement *m = sourceModel->torsionMeasurements(); m != NULL; m = m->next)
	  {
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden() || atoms[3]->isHidden()) continue;
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
		text.sprintf("%f Deg", m->value());
		// Add text object to list
		pos1 = sourceModel->modelToScreen(labpos);
		if (pos1.z < 1.0)
		{
			TextObject *to = new TextObject(int(pos1.x), int(height_ - pos1.y), FALSE, text);
			textObjects_.own(to);
		}
	  }
	glPopMatrix();
	msg.exit("Canvas::renderModelMeasurements");
}

// Render force arrows
void Canvas::renderModelForceArrows() const
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
void Canvas::renderModelCell(Model *sourceModel) const
{
	// Draw the unit cell of the model
	GLfloat fgcol[4];
	prefs.copyColour(Prefs::ForegroundColour, fgcol);
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, fgcol);
	glColor4fv(fgcol);
	glLineWidth(1.0f);
	static Vec3<double> cellCentre, lengths;
	static Mat4<double> matrix;
	// Check for valid model
	if (sourceModel == NULL)
	{
		printf("NULL Model passed to Canvas::renderModelCell\n");
		msg.exit("Canvas::renderModelCell");
		return;
	}
	Cell *cell = sourceModel->cell();
	if (cell->type() != Cell::NoCell)
	{
		// All cell types are transformations of a unit cube.
		// So, multiply modelview matrix by cell axes matrix and draw a unit cube
		glPushMatrix();
		  glMultMatrixd( sourceModel->cell()->axesForGL() );
		  if (prefs.isVisibleOnScreen(Prefs::ViewCell)) glCallList(glob(WireUnitCubeGlob));
		  lengths = sourceModel->cell()->lengths();
		  // Render cell axis arrows
		  if (prefs.isVisibleOnScreen(Prefs::ViewCellAxes))
		  {
			glTranslated(-0.5,-0.5,-0.5);
			glScaled(1.0/lengths.x,1.0/lengths.y,1.0/lengths.z);
			glCallList(glob(CellAxesGlob));
		  }
		glPopMatrix();
		// Here, translate the initial drawing position to be 0,0,0 in cell coordinates
		cellCentre = -cell->centre();
		glTranslated(cellCentre.x,cellCentre.y,cellCentre.z);
	}
}
