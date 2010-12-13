/*
	*** Window Extra Rendering
	*** src/render/engine_windows.cpp
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

#include "render/engine.h"
#include "model/model.h"
#include "gui/gui.h"
#include "gui/vibrations.h"

// Render addition elements related to visible windows
void RenderEngine::renderWindowExtras(Model *source, Matrix baseTransform, TCanvas *canvas)
{
	Vec3<double> r1, r2;
	GLfloat colour[4];
	Matrix A;
	int lod;
	double rij, phi;

	// Vibrations Window - Draw vibration arrows
	if ((gui.vibrationsWindow->isVisible()) && (gui.vibrationsWindow->ui.ShowVectorsCheck->isChecked()))
	{
		int row = gui.vibrationsWindow->ui.VibrationsList->currentRow();
		if (row != -1)
		{
			prefs.copyColour(Prefs::VibrationArrowColour, colour);
			// Get relevant model and vibration
			Model *m = (source->type() == Model::VibrationFrameType ? source->parent() : source);
			// Grab displacements array
			Vibration *vib = m->vibration(row);
			// Get vector scale factor
			double scale = gui.vibrationsWindow->ui.VectorScaleSpin->value();
			if (vib != NULL)
			{
				Vec3<double> *disp = vib->displacements();
				// Cycle over model atoms and draw associated displacement vectors
				int count = 0;
				for (Atom *i = source->atoms(); i != NULL; i = i->next)
				{
					// Grab atom position and calculated level of detail
					r1 = i->r();
					lod = levelOfDetail(r1, canvas);
					if (lod == -1) continue;
					
					r2 = disp[count]*scale;
					// Create basic transformation matrix
					A = baseTransform;
					A.applyTranslation(r1);
					rij = r2.magnitude();
					phi = DEGRAD * acos(r2.z/rij);
					// Special case where the bond is exactly in the XY plane.
					if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
					else A.applyRotationAxis(-r2.y, r2.x, 0.0, phi, TRUE);
					// Draw arrows
					if (prefs.renderStyle() == Atom::StickStyle)
					{
						glBegin(GL_LINES);
						glVertex3d(r1.x, r1.y, r1.z);
						glVertex3d(r1.x+r2.x, r1.y+r2.y, r1.z+r2.z);
						glEnd();
						// Move to endpoint
						A.applyTranslation(0.0,0.0,rij*0.9);
						A.applyScaling(rij*0.02,rij*0.02,rij*0.1);
						renderPrimitive(cones_, lod, colour, A, GL_LINE);
					}
					else
					{
						// Draw cylinder
						A.applyScaling(rij*0.05,rij*0.05,rij*0.9);
						renderPrimitive(cylinders_, lod, colour, A, GL_FILL);
						// Move to endpoint
						A.applyTranslation(0.0,0.0,1.0);
						A.applyScaling(2.0,2.0,0.1/0.9);
						renderPrimitive(cones_, lod, colour, A, GL_FILL);
					}
					++count;
				}
			}
			else printf("Internal Error : Couldn't get vibration from model.\n");
		}
	}

}
