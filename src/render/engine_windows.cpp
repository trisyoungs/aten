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
#include "gui/disorderwizard.h"
#include "gui/vibrations.h"
#include "main/aten.h"
#include "base/wrapint.h"

// Render addition elements related to visible windows
void RenderEngine::renderWindowExtras(Model *source)
{
	Vec3<double> r1, r2, translate, scale;
	Vec3<int> ineg, ipos;
	GLfloat colour[4];
	Matrix A;
	int i, j, k;
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
				Atom *i, **ii = source->atomArray();
				for (int n = 0; n < source->nAtoms(); ++n)
				{
					// Get atom pointer
					i = ii[n];

					// Skip hidden atoms
					if (i->isHidden()) continue;

					// Grab atom position and calculated level of detail
					r1 = i->r();
					
					r2 = disp[n]*scale;
					// Create basic transformation matrix
					A.setIdentity();
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
						renderPrimitive(RenderEngine::MiscObject, primitives_[Q_].cones_, colour, A, GL_LINE);
					}
					else
					{
						// Draw cylinder
						A.applyScaling(rij*0.05,rij*0.05,rij*0.9);
						renderPrimitive(RenderEngine::MiscObject, primitives_[Q_].cylinders_, colour, A, GL_FILL);
						// Move to endpoint
						A.applyTranslation(0.0,0.0,1.0);
						A.applyScaling(2.0,2.0,0.1/0.9);
						renderPrimitive(RenderEngine::MiscObject, primitives_[Q_].cones_, colour, A, GL_FILL);
					}
				}
			}
			else printf("Internal Error : Couldn't get vibration from model.\n");
		}
	}

	// Cell Transform Window
	if (gui.cellTransformWidget->isVisible())
	{
		Vec3<int> hkl;
		int n, anindex = -1, notanindex = -1, ncoords = 0;
		Vec3<double> coords[4], origin;
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
							primitives_[Q_].wireCube_.sendToGL();
							glPopMatrix();
						}
					}
				}
				glDisable(GL_LINE_STIPPLE);
				glPopMatrix();
				break;
			// Miller Plane tab
			case (3):
				hkl.set(gui.cellTransformWidget->ui.MillerHSpin->value(), gui.cellTransformWidget->ui.MillerKSpin->value(), gui.cellTransformWidget->ui.MillerLSpin->value());
				if ((hkl.x == 0) && (hkl.y == 0) && (hkl.z == 0)) return;
				// Plane Eq : hx + ky + lz = 1    (h, k, and l are reciprocals)
				for (n=0; n<3; ++n)
				{
					if (hkl[n] != 0)
					{
						coords[ncoords++].set(n, 1.0 / hkl[n]);
						anindex = n;
					}
					else notanindex = n;
				}
				// Generate other coordinates if necessary
				if (ncoords == 1)
				{
					// {100}
					i = (anindex+1)%3;
					j = (i+1)%3;
					for (n=1; n<4; ++n) coords[n] = coords[0];
					coords[1].set(i, 1.0);
					coords[2].set(i, 1.0);
					coords[2].set(j, 1.0);
					coords[3].set(j, 1.0);
					ncoords = 4;
				}
				else if (ncoords == 2)
				{
					// {110}
					coords[2] = coords[1];
					coords[2].set(notanindex, 1.0);
					coords[3] = coords[0];
					coords[3].set(notanindex, 1.0);
					ncoords = 4;
				}
				glPushMatrix();
				glMultMatrixd(source->cell()->axes().matrix());
				if (ncoords == 3)
				{
					glBegin(GL_TRIANGLES);
						glVertex3d(coords[0].x, coords[0].y, coords[0].z);
						glVertex3d(coords[1].x, coords[1].y, coords[1].z);
						glVertex3d(coords[2].x, coords[2].y, coords[2].z);
						glVertex3d(1-coords[0].x, 1-coords[0].y, 1-coords[0].z);
						glVertex3d(1-coords[1].x, 1-coords[1].y, 1-coords[1].z);
						glVertex3d(1-coords[2].x, 1-coords[2].y, 1-coords[2].z);
					glEnd();
				}
				else
				{
					glBegin(GL_QUADS);
						glVertex3d(coords[0].x, coords[0].y, coords[0].z);
						glVertex3d(coords[1].x, coords[1].y, coords[1].z);
						glVertex3d(coords[2].x, coords[2].y, coords[2].z);
						glVertex3d(coords[3].x, coords[3].y, coords[3].z);
						glVertex3d(1-coords[0].x, 1-coords[0].y, 1-coords[0].z);
						glVertex3d(1-coords[1].x, 1-coords[1].y, 1-coords[1].z);
						glVertex3d(1-coords[2].x, 1-coords[2].y, 1-coords[2].z);
						glVertex3d(1-coords[3].x, 1-coords[3].y, 1-coords[3].z);
					glEnd();
				}
				break;
		}
	}

	// Disorder Wizard
	static List<GridPrimitive> disorderGridPrimitives_;
	disorderGridPrimitives_.clear();
	if ((gui.disorderWizard->isVisible()) && (gui.disorderWizard->currentId() > 2))
	{
		// Get currently-selected partitioning scheme
		PartitioningScheme *ps = gui.disorderWizard->partitioningScheme();
		if (ps != NULL)
		{
			// Grab the grid structure and list of partitions from the scheme
			Grid &grid = ps->grid();

			// Initialise colour counter
			int colcount = 0;

			for (PartitionData *pd = ps->partitions(); pd != NULL; pd = pd->next)
			{
				if (pd->id() == 0) continue;
				
				// Use first three bits of colcount to set our colour values
				colour[0] = colcount%1 ? 1.0 : 0.0;
				colour[1] = colcount%2 ? 1.0 : 0.0;
				colour[2] = colcount%4 ? 1.0 : 0.0;
				colour[3] = 0.75;

				// Construct a surface for each partition in the model (except 0 == unit cell)
				GridPrimitive *prim = disorderGridPrimitives_.add();
				prim->setSource(&ps->grid());
				
				Matrix mat = gui.disorderWizard->cell()->axes();
				Vec3<int> npoints = grid.nPoints();
				mat.applyScaling(1.0/npoints.x, 1.0/npoints.y, 1.0/npoints.z);
				grid.setAxes(mat);
				grid.setLowerPrimaryCutoff(pd->id()-0.5);
				grid.setUpperPrimaryCutoff(pd->id()+0.5);
				prim->createSurfaceMarchingCubes();
				A.setIdentity();
				A.multiplyRotation(mat);
				renderPrimitive(RenderEngine::MiscObject, &prim->primaryPrimitive(), TRUE, colour, A, GL_FILL);
				
				// Increase colour counter
				++colcount;
				if (colcount > 7) colcount = 0;
			}
		}
	}
}
