/*
	*** Window Extra Rendering
	*** src/render/engine_windows.cpp
	Copyright T. Youngs 2007-2011

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
#include "gui/celltransform.h"
#include "gui/vibrations.h"

// Render addition elements related to visible windows
void RenderEngine::renderWindowExtras(Model *source, Matrix baseTransform, TCanvas *canvas)
{
	Vec3<double> r1, r2, translate, scale;
	Vec3<int> ineg, ipos;
	GLfloat colour[4];
	Matrix A;
	int lod, i, j, k;
	double rij, phi;

	// Vibrations Window - Draw vibration arrows
	if ((gui.vibrationsWidget->isVisible()) && (gui.vibrationsWidget->ui.ShowVectorsCheck->isChecked()))
	{
		int row = gui.vibrationsWidget->ui.VibrationsList->currentRow();
		if (row != -1)
		{
			prefs.copyColour(Prefs::VibrationArrowColour, colour);
			// Get relevant model and vibration
			Model *m = (source->type() == Model::VibrationFrameType ? source->parent() : source);
			// Grab displacements array
			Vibration *vib = m->vibration(row);
			// Get vector scale factor
			double scale = gui.vibrationsWidget->ui.VectorScaleSpin->value();
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

	// Cell Transform Window
	if (gui.cellTransformWidget->isVisible())
	{
		switch (gui.cellTransformWidget->ui.CellTransformTabs->currentIndex())
		{
			// Replicate tab - draw on end results of replication
			case (0):
				r1.x = gui.cellTransformWidget->ui.CellReplicateNegXSpin->value();
				r1.y = gui.cellTransformWidget->ui.CellReplicateNegYSpin->value();
				r1.z = gui.cellTransformWidget->ui.CellReplicateNegZSpin->value();
				r2.x = gui.cellTransformWidget->ui.CellReplicatePosXSpin->value();
				r2.y = gui.cellTransformWidget->ui.CellReplicatePosYSpin->value();
				r2.z = gui.cellTransformWidget->ui.CellReplicatePosZSpin->value();
				ineg.set(ceil(r1.x), ceil(r1.y), ceil(r1.z));
				ipos.set(floor(r2.x), floor(r2.y), floor(r2.z));
				prefs.copyColour(Prefs::UnitCellColour, colour);
				glColor4fv(colour);
				glPushMatrix();
				glMultMatrixd(source->cell()->axes().matrix());
				glTranslated(0.5, 0.5, 0.5);
				glEnable(GL_LINE_STIPPLE);
				glLineStipple(1,0x5555);
				for (i = ineg.x-1; i<=ipos.x; ++i)
				{
					// Construct translation vector (partial part)
					if (i == ineg.x-1) { scale.x = ineg.x-r1.x; translate.x = ineg.x - 0.5*scale.x - 0.5; }
					else if (i == ipos.x) { scale.x = r2.x-ipos.x; translate.x = ipos.x-1 + 0.5*scale.x + 0.5; }
					else { scale.x = 1.0; translate.x = i*1.0; }
					for (j = ineg.y-1; j<=ipos.y; ++j)
					{
						// Construct translation vector (partial part)
						if (j == ineg.y-1) { scale.y = ineg.y-r1.y; translate.y = ineg.y - 0.5*scale.y - 0.5; }
						else if (j == ipos.y) { scale.y = r2.y-ipos.y; translate.y = ipos.y-1 + 0.5*scale.y + 0.5; }
						else { scale.y = 1.0; translate.y = j*1.0; }
						
						for (k = ineg.z-1; k<=ipos.z; ++k)
						{
							// Construct translation vector (partial part)
							if (k == ineg.z-1) { scale.z = ineg.z-r1.z; translate.z = ineg.z - 0.5*scale.z - 0.5; }
							else if (k == ipos.z) { scale.z = r2.z-ipos.z; translate.z = ipos.z-1 + 0.5*scale.z + 0.5; }
							else { scale.z = 1.0; translate.z = k*1.0; }
							
							// Check scaling factors
							if ((scale.x < 0.001) || (scale.y < 0.001) || (scale.z < 0.001)) continue;
							
							glPushMatrix();
							glTranslated(translate.x, translate.y, translate.z);
							glScaled(scale.x, scale.y, scale.z);
							wireCube_.sendToGL();
							glPopMatrix();
						}
					}
				}
				glDisable(GL_LINE_STIPPLE);
				glPopMatrix();
		}
	}
}
