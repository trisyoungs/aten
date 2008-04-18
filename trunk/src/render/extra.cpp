/*
	*** Extra rendering
	*** src/render/extra.cpp
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
#include "base/master.h"
#include "base/elements.h"
#include "methods/mc.h"
#include "gui/canvas.h"

// Render other 3D objects
void Canvas::renderExtra3d()
{
	dbgBegin(Debug::Calls,"Canvas::renderExtra3d");
	// Draw on 3D embellishments for active modes
	static double radius;
	static Vec3<double> r, mouse;
	static Vec3<double> tempv;
	static Atom *i;
	// Draw on the selection highlights (for atoms in canvas.subsel)
	glSubsel3d();
	// Other modes
	switch (activeMode_)
	{
		// Draw on the bounding sphere of a radial selection
		case (Canvas::SelectRadialAction):
			i = atomHover_;
			if (i == NULL) break;
			// Work out the radius of the sphere
			tempv = rMouseDown_ - rMouseUp_;
			radius = tempv.x * tempv.y;
			radius /= i->screenRadius();
			// Convert the pixel radius into model coordinate radius. We will have the selection 'hotspot'
			// radius of the atom from its screen projection, which itself depends on the drawing style...
			radius *= prefs.screenRadius(i);
			r = i->rWorld();
			glPushMatrix();
			  glTranslatef(r.x,r.y,r.z);
			  glScalef(radius,radius,radius);
			  glCallList(list_[GLOB_SELSPHEREATOM]);
			glPopMatrix();
			break;
		// Draw on bond and new atom for chain drawing
		case (Canvas::EditChainAction):
			if (atomHover_ == NULL) break;
			r = atomHover_->r();
			// We need to project a point from the mouse position onto the canvas plane, unless the mouse is over an existing atom in which case we snap to its position instead
			i = displayModel_->atomOnScreen(rMouseLast_.x, rMouseLast_.y);
			if (i == NULL) mouse = displayModel_->guideToModel(rMouseLast_);
			else mouse = i->r();
			mouse -= r;
			glPushMatrix();
			  glTranslated(r.x,r.y,r.z);
			  // Determine how we'll draw the new bond / atom
			  if (prefs.renderStyle() == Atom::StickStyle)
			  {
				// Simple - draw line from atomHover_ to mouse position
				glBegin(GL_LINES);
				  glVertex3d(0.0,0.0,0.0);
				  glVertex3d(mouse.x,mouse.y,mouse.z);
				glEnd();
			  }
			  else
			  {
				glCylinder(mouse,mouse.magnitude(),1);
				glTranslated(mouse.x, mouse.y, mouse.z);
				switch (prefs.renderStyle())
				{
					case (Atom::TubeStyle):
						glCallList(list_[GLOB_WIRETUBEATOM]);
						break;
					case (Atom::SphereStyle):
						glCallList(list_[GLOB_WIRESPHEREATOM]);
						break;
					case (Atom::ScaledStyle):
						glCallList(list_[GLOB_WIRESPHEREATOM]);
						break;
				}
			  }
			glPopMatrix();
			break;
	}
	dbgEnd(Debug::Calls,"Canvas::renderExtra3d");
}

// Render 2D objects
void Canvas::renderExtra2d()
{
	dbgBegin(Debug::Calls,"Canvas::renderExtra2d");
	// Draw on any 2D objects, e.g. selection boxes, labels etc.
	static int n, i;
	static double dx, dy, halfw;
	// First set up a 2D drawing area...
	glMatrixMode(GL_PROJECTION);		// Swap to projection matrix...
	glLoadIdentity();			// ...clear it...
	glOrtho(0.0,width_,0.0,height_,-1,1);	// ...and setup a 2D canvas.
	// Now draw
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	// We add various 2D features depending on the current interaction mode.
	// Some we draw when the mode is active, some whtn it is just selected
	// Those for active modes (when button is down)...
	switch (activeMode_)
	{
		case (Canvas::NoAction):
			break;
		// Only selection mode where we draw a selection box
		case (Canvas::SelectAction):
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(1,0x5555);
			rectanglePrimitive(rMouseDown_.x, height_-rMouseDown_.y, rMouseLast_.x, height_-rMouseLast_.y);
			glDisable(GL_LINE_STIPPLE);
			break;
		// Draw line from last atom in selection list (if any) to the current mouse pos
		case (Canvas::MeasureDistanceAction):
		case (Canvas::MeasureAngleAction):
		case (Canvas::MeasureTorsionAction):
			break;
	}
	// ...and those for selected modes (whether the button is down or not).
	switch (selectedMode_)
	{
		// Draw on distance ruler for drawing modes
		case (Canvas::EditDrawAction):
		case (Canvas::EditChainAction):
			// Get angstrom length
			dx = 1.0 / drawPixelWidth_;
			halfw = width_ / 2.0;
			i = int( halfw / dx);
			if (i < 2) break;
			glBegin(GL_LINES);
			  for (n = -i; n <= i; n++)
			  {
				glVertex2d(halfw + n*dx, 10);
				glVertex2d(halfw + n*dx, 20);
				glVertex2d(halfw + (n+0.5)*dx, 10);
				glVertex2d(halfw + (n+0.5)*dx, 15);
			  }
			  glVertex2d(halfw - i*dx, 11);
			  glVertex2d(halfw + i*dx, 11);
			glEnd();
			for (n = -i; n < 0; n++) glText(halfw + n*dx - 8, 1, itoa(n));
			for (n = 0; n <= i; n++) glText(halfw + n*dx - 3, 1, itoa(n));
			break;
	}
	// If the mouse is hovering over an atom, draw a circle around it...
	if (atomHover_ != NULL)
	{
		Vec3<double> hoverpos = atomHover_->rScreen();
		circlePrimitive(hoverpos.x,hoverpos.y,atomHover_->screenRadius());
	}
	// Add text
	//text(1.0,h-12.0,displayModel_->name());
	// Draw on colour scale if necessary
	if (prefs.colourScheme() != Prefs::ElementScheme)
	{
		float midy = height_ / 2;
		//glBegin(
	}
	dbgEnd(Debug::Calls,"Canvas::renderExtra2d");
}

// Render disordered insertion regions
void Canvas::renderRegions()
{
	dbgBegin(Debug::Calls,"Canvas::renderRegions");
	static Vec3<double> centre, size;
	static GLfloat colour[4];
	int i = 0;
	// Enable alpha component and make sure lighting is on
	glEnable(GL_BLEND);
	glEnable(GL_LIGHTING);
	for (Model *m = master.models(); m != NULL; m = m->next)
	{
		elements.copyAmbientColour(i, colour);
		colour[3] = 0.4f;
		glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE, colour);
		glPushMatrix();
		  centre = m->area.centre();
		  size = m->area.size();
		  switch (m->area.shape())
		  {
			case (ComponentRegion::WholeCell):
				break;
			case (ComponentRegion::CuboidRegion):
				glTranslated(centre.x,centre.y,centre.z);
				glScaled(size.x,size.y,size.z);
				glCallList(list_[GLOB_UNITCUBE]);
				break;
			case (ComponentRegion::SpheroidRegion):
				glTranslated(centre.x,centre.y,centre.z);
				glScaled(size.x,size.y,size.z);
				glCallList(list_[GLOB_UNITATOM]);
				break;
			default:
				printf("renderRegions :: ComponentRegion type not done.\n");
				break;
		  }
		glPopMatrix();
		i ++;
	}
	// Turn off blending (if not antialiasing)
	if (!prefs.hasGlOption(Prefs::LineAliasOption) && !prefs.hasGlOption(Prefs::PolyAliasOption)) glDisable(GL_BLEND);
	glDisable(GL_LIGHTING);
	dbgEnd(Debug::Calls,"Canvas::renderRegions");
}
