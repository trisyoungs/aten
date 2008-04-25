/*
	*** Master rendering routines
	*** src/render/render.cpp
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

#include "base/master.h"
#include "gui/canvas.h"
#include "model/model.h"

// Render model
void Canvas::renderScene(Model *source)
{
	dbgBegin(Debug::Calls,"Canvas::renderScene");
	static double rotmat[16], cammat[16];
	static Model *trajparent;
	static double camrot;

	// If the canvas is stil resttricted, don't draw anything
	if (noDraw_)
	{
		dbgEnd(Debug::Calls,"Canvas::renderScene");
		return;
	}

	// Begin the GL commands
	if (!beginGl())
	{
		dbgEnd(Debug::Calls,"Canvas::renderScene");
		return;
	}

	// Check the supplied model against the previous one rendered to see if we must outdate the display list
	if ((source != displayModel_) || (source == NULL)) renderPoint_ = -1;

	// Store the source model pointer and grab the trajectoryparent pointer (if there is one)
	displayModel_ = source;
	trajparent = source->trajectoryParent();
	if (displayModel_ == NULL)
	{
		// Select projection matrix and load the identity matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		// Set up a 2D canvas
		glOrtho(0.0,width_,0.0,height_,-1,1);
		// Draw on our default message
		glMatrixMode(GL_MODELVIEW);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		glRasterPos2i(1,(int)height_-13);
		glText(1.0,height_-10.0,"No model to display.");
		dbgEnd(Debug::Calls,"Canvas::renderScene");
		return;
	}

	// Clear colour
	GLfloat *clrcol = prefs.penColour(Prefs::BackgroundColour);
	glClearColor(clrcol[0],clrcol[1],clrcol[2],clrcol[3]);
	// Clear colour and depth buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Grab rotation & camera matrices, and camera rotation for the model. If we're displaying a trajectory frame, grab the parent's matrix instead.
	if (trajparent == NULL)
	{
		displayModel_->copyRotationMatrix(rotmat);
		displayModel_->copyCameraMatrix(cammat);
		camrot = displayModel_->cameraRotation();
	}
	else
	{
		trajparent->copyRotationMatrix(rotmat);
		trajparent->copyCameraMatrix(cammat);
		camrot = trajparent->cameraRotation();
	}

	// Draw on the rotation globe
	if (prefs.shouldRender(Prefs::ViewGlobe)) renderRotationGlobe(rotmat, camrot);

	// Reset projection matrix and set perspective view
	double top, bottom;
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	if (prefs.hasPerspective())
	{
		bottom = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
		top = -bottom;
		glFrustum(aspect_*top, aspect_*bottom, top, bottom, prefs.clipNear(), prefs.clipFar());
	}
	else
	{
		bottom = displayModel_->orthoSize();
		top = -bottom;
		top = -bottom;
		glOrtho(aspect_*top, aspect_*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
	}

	// Reset GLs modelview matrix and apply camera matrix from model
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glMultMatrixd(cammat);

	// Draw guide if visible
	if (prefs.isGuideVisible())
	{
		glTranslated(0.0,0.0,-prefs.drawDepth());
		glCallList(list_[GLOB_GUIDE]);
		glTranslated(0.0,0.0,prefs.drawDepth());
	}

	// Apply model's rotation matrix (which we grabbed earlier)
	glMultMatrixd(rotmat);

	// Set the initial state of lighting in the model
	prefs.renderStyle() == Atom::StickStyle ? glDisable(GL_LIGHTING) : glEnable(GL_LIGHTING);
	// Draw the main model parts
	// If renderPoint_ matches the model's total change point (from get_point()) then just re-render the stored display list. If not, create the display list.
	glPushMatrix();
	  if (renderPoint_ == displayModel_->log(Change::TotalLog)) glCallList(list_[GLOB_MODEL]);
	  else
	  {
		msg(Debug::Verbose,"Recreating display list for model '%s'...", displayModel_->name());
		//glDeleteLists(list_[GLOB_MODEL],1);
		glNewList(list_[GLOB_MODEL],GL_COMPILE_AND_EXECUTE);
		  // Draw the model cell (this also translates our drawing position to the -half cell point.
		  renderModelCell();
		  // Draw the model's atoms, bonds, and selection
		  if (prefs.shouldRender(Prefs::ViewAtoms)) renderModelAtoms();
		  // Render glyphs associated with the model
		  renderModelGlyphs();
		  // Render force arrows
		  if (prefs.shouldRender(Prefs::ViewForceArrows)) renderModelForceArrows();
		glEndList();
		renderPoint_ = displayModel_->log(Change::TotalLog);
		msg(Debug::Verbose," Done. (New point = %i)\n",renderPoint_);
	  }
	  // Render surfaces
	  if (prefs.shouldRender(Prefs::ViewSurfaces)) renderSurfaces();
	  // Render MC regions
	  if ((displayModel_->cell()->type() != Cell::NoCell) && prefs.shouldRender(Prefs::ViewRegions)) renderRegions();
	  glColor3fv(prefs.penColour(Prefs::ForegroundColour));
	  renderExtra3d();
	glPopMatrix();

	// Draw replicated cells (using display list)
	if (prefs.shouldRender(Prefs::ViewCellRepeat))
	{
		static Mat3<double> cellmat;
		static Vec3<double> cx, cy, cz;
		cellmat = displayModel_->cell()->axes();
		cx = cellmat.rows[0];
		cy = cellmat.rows[1];
		cz = cellmat.rows[2];
		for (int i=-prefs.repeatCellsNeg(0); i<=prefs.repeatCellsPos(0); i++)
		{
			glPushMatrix();
			glTranslated(i*cx.x,i*cx.y,i*cx.z);
			for (int j=-prefs.repeatCellsNeg(1); j<=prefs.repeatCellsPos(1); j++)
			{
				glPushMatrix();
				glTranslated(j*cy.x,j*cy.y,j*cy.z);
				for (int k=-prefs.repeatCellsNeg(2); k<=prefs.repeatCellsPos(2); k++)
				{
					if ((i == 0) && (j == 0) && (k == 0)) continue;
					glPushMatrix();
					glTranslated(k*cz.x,k*cz.y,k*cz.z);
					glCallList(list_[GLOB_MODEL]);
					glPopMatrix();
				}
				glPopMatrix();
			}
			glPopMatrix();
		}
	}

	// Render measurements / labels, also in 3D
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, prefs.penColour(Prefs::ForegroundColour));
	glDisable(GL_LIGHTING);

	if (prefs.shouldRender(Prefs::ViewLabels)) renderModelLabels();
	if (prefs.shouldRender(Prefs::ViewMeasurements)) renderModelMeasurements();

	renderExtra2d();
	glDisable(GL_COLOR_MATERIAL);

	glFlush();
	endGl();
	dbgEnd(Debug::Calls,"Canvas::renderScene");
}
