/*
	*** Model Rendering
	*** src/gui/viewer_model.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/viewer.hui"
#include "model/model.h"
#include "base/grid.h"

ATEN_USING_NAMESPACE

// Render full Model in specified viewport
void Viewer::renderModel(Model* source, int viewPortX, int viewPortY, int viewPortWidth, int viewPortHeight, bool drawRotationGlobe)
{
	Messenger::enter("Viewer::renderModel");

	// Valid pointer passed?
	if (source == NULL)
	{
		Messenger::exit("Viewer::renderModel");
		return;
	}
	Messenger::print(Messenger::Verbose, " --> RENDERING BEGIN : source model pointer = %p, renderpoint = %d", source, source->log(Log::Total));

	// Set the source Model pointer to be the current trajectory frame if relevant
	if (source->renderSource() == Model::TrajectorySource)
	{
		if (source->hasTrajectory()) source = source->trajectoryCurrentFrame();
		else Messenger::error("Internal Error: Requested trajectory rendering, but no trajectory associated to current model.");
	}
// 	else if (source->renderSource() == Model::VibrationSource)
// 	{
		// ATEN2 TODO
// 	}

	// Setup view for model, in the supplied viewport
	source->setupView(viewPortX, viewPortY, viewPortWidth, viewPortHeight);

	// Set initial transformation matrix, including any translation occurring from cell or viewOrigin...
	modelTransformationMatrix_ = source->modelViewMatrix();
	modelTransformationMatrix_.applyTranslation(-source->viewOriginOrCellOrigin());
	
	// Set target matrix mode and reset it, and set colour mode
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	// Render rotation globe in small viewport in lower right-hand corner
	if (drawRotationGlobe)
	{
		int n = prefs.globeSize();
// 		if (aten_->nVisibleModels() > 2) n /= 2;
		glViewport(viewPortX+viewPortWidth-n, viewPortY, n, n);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho(-1.0, 1.0, -1.0, 1.0, -10.0, 10.0);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		glEnable(GL_LIGHTING);

		// Draw globe axes, rotated by current view
		Matrix A = modelTransformationMatrix_;

		A.removeTranslationAndScaling();
		A[14] = -1.2;
		glLoadMatrixd(A.matrix());
		primitives_[primitiveSet_].rotationGlobeAxes().sendToGL(QOpenGLContext::currentContext());

		// Draw globe - no view rotation, just translate it back a bit
		A.setIdentity();
		A[14] = -1.2;
		glLoadMatrixd(A.matrix());
		primitives_[primitiveSet_].rotationGlobe().sendToGL(QOpenGLContext::currentContext());
	}

	// Prepare for model rendering
	glViewport(viewPortX, viewPortY, viewPortWidth, viewPortHeight);
	glMatrixMode(GL_PROJECTION);
	glLoadMatrixd(source->modelProjectionMatrix().matrix());
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	// Draw unit cell (if one exists)
	if (source->cell().type() != UnitCell::NoCell)
	{
		// Copy colour for cell
		GLfloat colour[4];
		colour[0] = 0.0f;
		colour[1] = 0.0f;
		colour[2] = 0.0f;
		colour[3] = 1.0f;

		// Set current view matrix to account for model rotation, cell axes size, and translation to view origin
		Matrix A = source->modelViewMatrix() * source->cell().axes();
		glLoadMatrixd(A.matrix());
		Vec3<double> translation = source->cell().inverse() * source->viewOriginOrCellOrigin();
		glTranslated(-translation.x, -translation.y, -translation.z);

		// Draw a wire cube for the cell
		glColor4fv(colour);
		primitives_[primitiveSet_].wireCube().sendToGL(QOpenGLContext::currentContext());

		// Draw cell axes
		Vec3<double> v = source->cell().lengths();
		glScaled(1.0 / v.x, 1.0 / v.y, 1.0 / v.z);
		primitives_[primitiveSet_].cellAxes().sendToGL(QOpenGLContext::currentContext());
	}

	// Get RenderGroup for model (it will be updated if necessary by the called function)
	RenderGroup& modelGroup = source->renderGroup(primitives_[primitiveSet_]);

	// Draw main model (atoms, bonds, etc.)
	Matrix offset;
	Vec3<int> repeatMin = -source->repeatCellsNegative();
	Vec3<int> repeatMax = source->repeatCellsPositive();
	if (source->cell().type() == UnitCell::NoCell)
	{
		repeatMin = 0;
		repeatMax = 0;
	}
	int x, y, z;
	for (x = repeatMin.x; x <= repeatMax.x; ++x)
	{
		for (y = repeatMin.y; y <= repeatMax.y; ++y)
		{
			for (z = repeatMin.z; z <= repeatMax.z; ++z)
			{
				offset = modelTransformationMatrix_;
				offset.addTranslation(source->cell().axes() * Vec3<double>(x,y,z));

				// Render model
				modelGroup.sendToGL(offset);

				// Render grids
				for (Grid* g = source->grids(); g != NULL; g = g->next)
				{
					if (!g->isVisible()) continue;

					glPushMatrix();
					glLoadMatrixd((offset * g->voxelMatrix()).matrix());

					g->sendPrimaryPrimitive();
					if (g->useSecondary()) g->sendSecondaryPrimitive();
					if (g->outlineVolume())
					{
						glDisable(GL_LIGHTING);
						glColor4f(0.0f, 0.0f, 0.0f, 1.0f);
						Vec3<int> nXYZ = g->nXYZ();
						if (!g->periodic()) nXYZ -= 1;
						glScaled(nXYZ.x, nXYZ.y, nXYZ.z);
						primitives_[primitiveSet_].wireCube().sendToGL(QOpenGLContext::currentContext());
					}

					glPopMatrix();
				}
			}
		}
	}

	Messenger::exit("Viewer::renderModel");
}
