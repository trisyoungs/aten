/*
	*** Model parts rendering
	*** src/render/model.cpp
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
#include "classes/forcefield.h"
#include "base/elements.h"
#include "gui/canvas.h"

// Render atom labels
void canvas::render_model_labels()
{
	dbg_begin(DM_CALLS,"canvas::render_model_labels");
	// Annotate the model with 2D labels
	static char text[64];
	static atom *i;
	static int labels;
	static ffatom *ffa;
	static vec3<double> cellorigin;
	// If we have a unit cell we must account for the origin translation
	cellorigin = displaymodel->get_cellorigin();
	for (atom *i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		// Check if atom has labels
		if (!i->has_labels()) continue;
		labels = i->get_labels();
		ffa = i->get_type();
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
			if (ffa == NULL) strcat(text, "None");
			else
			{
				strcat(text,itoa(ffa->get_ffid()));
				strcat(text," ");
				strcat(text,ffa->get_name());
			}
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
		textbitmap(cellorigin + i->r(), text);
	}
	dbg_end(DM_CALLS,"canvas::render_model_labels");
}

// Render measurements
void canvas::render_model_measurements()
{
	dbg_begin(DM_CALLS,"canvas::render_model_measurements");
	static vec3<double> ri, rj, rk, rl, labpos, cellorigin;
	static char text[256];
	static atom **atoms;
	// Grab cell origin to get correct positioning
	cellorigin = displaymodel->get_cellorigin();
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
				ri = atoms[0]->r();
				rj = atoms[1]->r();
				labpos = (ri + rj) * 0.5;
				glVertex3d(ri.x, ri.y, ri.z);
				glVertex3d(rj.x, rj.y, rj.z);
				sprintf(text,"%f A",m->get_value());
				break;
			case (GT_ANGLE):
				ri = atoms[0]->r();
				rj = atoms[1]->r();
				rk = atoms[2]->r();
				labpos = rj;
				glVertex3d(ri.x, ri.y, ri.z);
				glVertex3d(rj.x, rj.y, rj.z);
				glVertex3d(rk.x, rk.y, rk.z);
				sprintf(text,"%f Deg",m->get_value());
				break;
			case (GT_TORSION):
				ri = atoms[0]->r();
				rj = atoms[1]->r();
				rk = atoms[2]->r();
				rl = atoms[3]->r();
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
	dbg_end(DM_CALLS,"canvas::render_model_measurements");
}

// Render force arrows
void canvas::render_model_forcearrows()
{
	dbg_begin(DM_CALLS,"canvas::render_model_forcearrows");
	for (atom *i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		// Scale forces to more reasonable values  TODO User scaling
		gl_arrow(i->r(),i->f() / 30.0);
	}
	dbg_end(DM_CALLS,"canvas::render_model_forcearrows");
}

// Render model cell
void canvas::render_model_cell()
{
	// Draw the unit cell of the model
	glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, prefs.colours[COL_PEN]);
	glColor3iv(prefs.colours[COL_PEN]);
	glLineWidth(1.0f);
	static vec3<double> origin;
	if (displaymodel->get_celltype() != CT_NONE)
	{
		// All cell types are transformations of a unit cube.
		// So, multiply modelview matrix by cell axes matrix and draw a unit cube
		//mat4<double> mat = displaymodel->get_cell()->get_transpose_as_mat4();
		//mat.get_column_major(glmat);
		double glmat[16];
		displaymodel->get_cell()->get_transpose_as_mat4().get_column_major(glmat);
		glPushMatrix();
		  glMultMatrixd(glmat);
		  if (prefs.should_render(VO_CELL)) glCallList(list[GLOB_WIREUNITCUBE]);
		  vec3<double> l = displaymodel->get_cell()->get_lengths();
		  // Render cell axis arrows
		  if (prefs.should_render(VO_CELLAXES))
		  {
			glTranslated(-0.5,-0.5,-0.5);
			glScaled(1.0/l.x,1.0/l.y,1.0/l.z);
			glCallList(list[GLOB_CELLAXES]);
		  }
		glPopMatrix();
		// Here, translate the initial drawing position to be 0,0,0 in cell coordinates
		origin = displaymodel->get_cellorigin();
		glTranslated(origin.x,origin.y,origin.z);
	}
}
