 /*
	*** Master rendering routines
	*** src/render/render.cpp
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

// Render model
void Canvas::renderScene(Model *source)
{
	msg.enter("Canvas::renderScene");
	static GLdouble rotmat[16], cammat[16];
	static GLdouble camrot;
	static Model *lastDisplayed_ = NULL;

	msg.print(Messenger::GL, " --> RENDERING BEGIN\n");

	// If the canvas is stil restricted, don't draw anything
	if (noDraw_)
	{
		msg.exit("Canvas::renderScene");
		msg.print(Messenger::GL, " --> RENDERING END (NODRAW)\n");
		return;
	}
	checkGlError();

	// Begin the GL commands
	if (!beginGl())
	{
		msg.exit("Canvas::renderScene");
		msg.print(Messenger::GL, " --> RENDERING END (BAD BEGIN)\n");
		return;
	}
	checkGlError();

	// Check the supplied model against the previous one rendered to see if we must outdate the display list
	if ((source != displayModel_) || (source == NULL)) renderPoint_.reset();
	msg.print(Messenger::GL, "Begin rendering pass : source model pointer = %p, renderpoint = %d\n", source, renderPoint_.log(Log::Total));

	// Store the source model pointer and grab the trajectoryparent pointer (if there is one)
	displayModel_ = source;
	if (displayModel_ == NULL)
	{
		// Select projection matrix and load the identity matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		// Set up a 2D canvas
		glOrtho(0.0,width_*1.0,0.0,height_*1.0,-1.0,1.0);
		// Draw on our default message
		glMatrixMode(GL_MODELVIEW);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		glText(width_/2,height_/2,"No model to display.");
		msg.exit("Canvas::renderScene");
		return;
	}

	// If this is a trajectory frame, check its ID against the last one rendered
	if (source->parent() != NULL)
	{
		if (source->parent()->trajectoryFrameIndex() != displayFrameId_) renderPoint_.reset();
		displayFrameId_ = source->parent()->trajectoryFrameIndex();
		msg.print(Messenger::GL, " --> Source model is a trajectory frame - index = %i\n", displayFrameId_);
	}

	// Prep for drawing
	msg.print(Messenger::GL, " --> Preparing lights, shading, aliasing, etc.\n");
	prepGl();
	checkGlError();

	// Clear colour and depth buffers
	checkGlError();
	msg.print(Messenger::GL, " --> Clearing context, background, and setting pen colour\n");
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Grab rotation & camera matrices, and camera rotation for the model. If we're displaying a trajectory frame, grab the parent's matrix instead.
	displayModel_->copyRotationMatrix(rotmat);
	displayModel_->copyCameraMatrix(cammat);
	camrot = displayModel_->cameraRotation();

	// Setup pen colour
	GLfloat fgcol[4];
	prefs.copyColour(Prefs::ForegroundColour, fgcol);
	glDisable(GL_COLOR_MATERIAL);
	glColor4fv(fgcol);

	// Draw on the rotation globe
	if (prefs.isVisibleOnScreen(Prefs::ViewGlobe)) renderRotationGlobe(rotmat, camrot);

	// Reset projection matrix and set perspective view
	checkGlError();
	msg.print(Messenger::GL, " --> Setting projection matrix\n");
	GLdouble top, bottom;
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
		top = tan(prefs.perspectiveFov() / DEGRAD) * displayModel_->camera().z;
		bottom = -top;
		glOrtho(aspect_*top, aspect_*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
	}

	// Reset GLs modelview matrix and apply camera matrix from model
	checkGlError();
	msg.print(Messenger::GL, " --> Setting modelview matrix\n");
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glMultMatrixd(cammat);

	// Draw guide if visible
	if (prefs.isGuideVisible())
	{
		glTranslated(0.0,0.0,-prefs.drawDepth());
		glCallList(glob(GuideGlob));
		glTranslated(0.0,0.0,prefs.drawDepth());
	}

	// Apply model's rotation matrix (which we grabbed earlier)
	glMultMatrixd(rotmat);

	// Set the initial state of lighting in the model
	prefs.renderStyle() == Atom::StickStyle ? glDisable(GL_LIGHTING) : glEnable(GL_LIGHTING);
	// Draw the main model parts
	// If renderPoint_ (Log) matches the model's then just re-use the stored display list. If not, create the display list.
	checkGlError();
	msg.print(Messenger::GL, " --> Drawing model\n");
	glPushMatrix();
	  if ((renderPoint_ == displayModel_->changeLog) && (lastDisplayed_ == source)) glCallList(glob(ModelGlob));
	  else
	  {
		msg.print(Messenger::Verbose,"Recreating display list for model '%s'...", displayModel_->name());
		//glDeleteLists(glob(ModelGlob),1);
		glNewList(glob(ModelGlob),GL_COMPILE_AND_EXECUTE);
		  // Draw the model cell (this also translates our drawing position to the -half cell point.
		  checkGlError();
		  msg.print(Messenger::GL, " --> ...rendering model cell\n");
		  renderModelCell(displayModel_);
		  // Draw the model's atoms, bonds, and selection
		  checkGlError();
		  msg.print(Messenger::GL, " --> ...rendering model atoms\n");
		  if (prefs.isVisibleOnScreen(Prefs::ViewAtoms)) renderModelAtoms(displayModel_);
		  // Render glyphs associated with the model
		  checkGlError();
		  msg.print(Messenger::GL, " --> ...rendering model glyphs\n");
		  renderModelGlyphs(displayModel_);
		  // Render force arrows
		  if (prefs.isVisibleOnScreen(Prefs::ViewForceArrows)) renderModelForceArrows();
		glEndList();
		renderPoint_ = displayModel_->changeLog;
		msg.print(Messenger::Verbose," Done. (New point = %i)\n",renderPoint_.log(Log::Total));
	  }
	  // Render surfaces
	  checkGlError();
	  msg.print(Messenger::GL, " --> ...rendering model surfaces\n");
	  if (prefs.isVisibleOnScreen(Prefs::ViewSurfaces)) renderSurfaces(displayModel_);
	  // Render MC regions
	  checkGlError();
	  msg.print(Messenger::GL, " --> ...rendering regions\n");
	  if ((displayModel_->cell()->type() != Cell::NoCell) && prefs.isVisibleOnScreen(Prefs::ViewRegions)) renderRegions();
	  checkGlError();
	  msg.print(Messenger::GL, " --> ...rendering extra 3d\n");
	  glColor4fv(fgcol);
	  renderExtra3d();
	glPopMatrix();

	// Draw replicated cells (using display list)
	if (prefs.isVisibleOnScreen(Prefs::ViewCellRepeat))
	{
		checkGlError();
		msg.print(Messenger::GL, " --> ...rendering cell repeat units\n");
		static Mat3<GLdouble> cellmat;
		static Vec3<GLdouble> cx, cy, cz;
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
					  glCallList(glob(ModelGlob));
					glPopMatrix();
				}
				glPopMatrix();
			  }
			glPopMatrix();
		}
	}

	// Render measurements / labels, also in 3D
	checkGlError();
	msg.print(Messenger::GL, " --> Setting up context for extra view items\n");
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, fgcol);
	glDisable(GL_LIGHTING);

	// Reproject atoms if necessary
	displayModel_->projectAll();

	// Render text glyphs associated with the model
	checkGlError();
	msg.print(Messenger::GL, " --> Rendering text glyphs\n");
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	renderModelTextGlyphs(displayModel_);
	checkGlError();
	msg.print(Messenger::GL, " --> Rendering model labels\n");
	if (prefs.isVisibleOnScreen(Prefs::ViewLabels)) renderModelLabels(displayModel_);
	checkGlError();
	msg.print(Messenger::GL, " --> Rendering model measurements\n");
	if (prefs.isVisibleOnScreen(Prefs::ViewMeasurements)) renderModelMeasurements(displayModel_);

	checkGlError();
	msg.print(Messenger::GL, " --> Rendering extra 2D\n");
	renderExtra2d();

	// Render colourscales
	checkGlError();
	msg.print(Messenger::GL, " --> Rendering colourscales\n");
	renderColourscales();

	glDisable(GL_COLOR_MATERIAL);
	checkGlError();

	//glFlush();
	endGl();
	checkGlError();

	msg.print(Messenger::GL, " --> RENDERING END\n");
	lastDisplayed_ = source;

	msg.exit("Canvas::renderScene");
}

// Render list of TextObjects 
void Canvas::renderText(QPainter &painter)
{
	msg.enter("Canvas::renderText");

	// If the canvas is still restricted, don't draw anything
	if (noDraw_)
	{
		msg.exit("Canvas::renderText");
		return;
	}

	// Render text
	for (TextObject *to = textObjects_.first(); to != NULL; to = to->next)
	{
		if (!prefs.useNiceText()) glText(to->x, to->y, to->text);
		else
		{
			if (to->rightAlign) painter.drawText(0, to->y, to->x, to->y, Qt::AlignRight, to->text, NULL);
			else painter.drawText(to->x, to->y, to->text);
		}
	}

	// Clear list
	textObjects_.clear();

	msg.exit("Canvas::renderText");
}
