/*
	*** Model canvas stub
	*** src/gui/canvas.cpp
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
#include "base/prefs.h"
#include "gui/gui.h"
#include "gui/canvas.h"
#include "gui/tcanvas.uih"
#include "render/gl2ps.h"
#include "model/model.h"

// Constructor
Canvas::Canvas()
{
	// Private variables
	valid_ = FALSE;
	renderPoint_ = -1;
	drawing_ = FALSE;
	displayModel_ = NULL;
	drawPixelWidth_ = 1.0;
	activeMode_ = UA_NONE;
	selectedMode_ = UA_PICKSELECT;
	list_[0] = 0;
	contextWidget_ = NULL;
	subselectEnabled_ = FALSE;
	for (int i=0; i<3; i++)
	{
		mouseButton_[i] = FALSE;
		keyModifier_[i] = FALSE;
	}
}

// Set the internal name of the canvas
void Canvas::setName(const char *s)
{
	name_ = s;
}

// Return the current height of the drawing area
float Canvas::height()
{
	return height_;
}

// Return the current width of the drawing area
float Canvas::width()
{
	return width_;
}

// Return whether the canvas is currently drawing
bool Canvas::isDrawing()
{
	return drawing_;
}

// Return if the canvas is valid
bool Canvas::isValid()
{
	return valid_;
}

// Begin GL
bool Canvas::beginGl()
{
	if (!valid_) return FALSE;
	drawing_ = TRUE;
	return TRUE;
}

// Finalize GL commands
void Canvas::endGl()
{
	drawing_ = FALSE;
}

/*
// Widget Canvas
*/

// Set widget
bool Canvas::setWidget(TCanvas *w)
{
	contextWidget_ = w;
	return TRUE;
}

// Widget realize
void Canvas::realize()
{
	// Sets the canvas to use a widget for output.
	dbgBegin(Debug::Calls,"Canvas::realize");
	valid_ = TRUE;
	initGl();
	dbgEnd(Debug::Calls,"Canvas::realize");
}

// Invalidate
void Canvas::postRedisplay()
{
	dbgBegin(Debug::Calls,"Canvas::postRedisplay");
	if (gui.exists()) contextWidget_->paintGL();
	dbgEnd(Debug::Calls,"Canvas::postRedisplay");
}

// Widget Expose
void Canvas::expose()
{
	if ((!gui.exists()) || gui.noRendering() ) return;
	// Render from the current rendering source
	renderScene(master.currentModel()->renderSource());
}

// Widget configure
void Canvas::configure()
{
	// Store the new width and height of the widget and re-do projection
	width_ = (float)contextWidget_->width();
	height_ = (float)contextWidget_->height();
	doProjection();
	// Flag that render source needs to be reprojected
	if (displayModel_ != NULL) displayModel_->logChange(LOG_VISUAL);
}

/*
// Rendering
*/

// Return the current display model
Model *Canvas::displayModel()
{
	return displayModel_;
}

// Calculate drawing pixel width
void Canvas::calculateDrawPixelWidth()
{
	// Get the Angstrom width of a single pixel at the current draw depth in the current view
	static Vec3<double> r1, r2;
	if (displayModel_ != NULL)
	{
		r1 = displayModel_->guideToModel(width_/2, height_/2);
		r2 = displayModel_->guideToModel(width_/2+1, height_/2);
		r2 -= r1;
		drawPixelWidth_ = r2.x;
	}
	else drawPixelWidth_ = 1.0;
}

// Set GL options
void Canvas::initGl()
{
	if (!valid_) return;
	dbgBegin(Debug::Calls,"Canvas::initGl");
	if (beginGl())
	{
		// Create lists for globs if this is the first call to init_gl()
		if (list_[0] == 0)
		{
			list_[GLOB_STICKATOM] = glGenLists(GLOB_NITEMS);
			for (int n=1; n<GLOB_NITEMS; n++) list_[n] = list_[GLOB_STICKATOM]+n;
		}

		// Fill display lists
		createLists();

		// Clear colour
		GLfloat *clrcol = prefs.colour(COL_BG);
		glClearColor(clrcol[0],clrcol[1],clrcol[2],clrcol[3]);
		glClearDepth(1.0);
		// Perspective hint
		glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST);
		// Enable depth buffer
		glEnable(GL_DEPTH_TEST);
		// Smooth shading
		glShadeModel(GL_SMOOTH);
		// Auto-calculate surface normals
		glEnable(GL_NORMALIZE);
		// Set alpha-blending function
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
		//glBlendFunc(GL_SRC_ALPHA_SATURATE, GL_ONE);
		// Set up the light model
		glEnable(GL_LIGHTING);
		glLightfv(GL_LIGHT0,GL_AMBIENT,prefs.spotlightColour(SL_AMBIENT));
		glLightfv(GL_LIGHT0,GL_DIFFUSE,prefs.spotlightColour(SL_DIFFUSE));
		glLightfv(GL_LIGHT0,GL_SPECULAR,prefs.spotlightColour(SL_SPECULAR));
		glLightfv(GL_LIGHT0,GL_POSITION,prefs.spotlightPosition());
		prefs.spotlightActive() ? glEnable(GL_LIGHT0) : glDisable(GL_LIGHT0);
		glDisable(GL_BLEND);
		glDisable(GL_LINE_SMOOTH);
		glDisable(GL_POLYGON_SMOOTH);
		// Configure antialiasing
		if (prefs.hasGlOption(GO_LINEALIASING))
		{
			glEnable(GL_BLEND);
			glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_LINE_SMOOTH);
		}
		if (prefs.hasGlOption(GO_POLYALIASING))
		{
			glEnable(GL_BLEND);
			glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_POLYGON_SMOOTH);
		}
		// Configure fog effects
		if (prefs.hasGlOption(GO_FOG))
		{
			glFogi(GL_FOG_MODE, GL_LINEAR);
			glFogfv(GL_FOG_COLOR, prefs.colour(COL_BG));
			glFogf(GL_FOG_DENSITY, 0.35f);
			glHint(GL_FOG_HINT, GL_NICEST);
			glFogf(GL_FOG_START,prefs.fogNear());
			glFogf(GL_FOG_END,prefs.fogFar());
			glEnable(GL_FOG);
		}
		else glDisable(GL_FOG);
		// Configure face culling
		glCullFace(GL_BACK);
		prefs.hasGlOption(GO_BACKCULLING) ? glEnable( GL_CULL_FACE ) : glDisable(GL_CULL_FACE);
		// Test
		// End Test
		endGl();
	}
	else printf("Failed to set-up OpenGL on canvas.\n");
	dbgEnd(Debug::Calls,"Canvas::initGl");
}

// Create display lists
void Canvas::createLists()
{
	if (!isValid()) return;
	dbgBegin(Debug::Calls,"Canvas::createLists");

	int n,m, atomdetail, ticks, extent;
	double delta, tickdelta, tickheight, ticktop, tickbottom, spacing;
	// Grab some oft-used values
	atomdetail = prefs.atomDetail();
	spacing = prefs.guideSpacing();
	extent = prefs.guideExtent();
	ticks = prefs.guideTicks();

	/*
	// Selected Atoms
	*/
	// Enlarged sphere (for selections with DS_TUBE)
	glNewList(list_[GLOB_SELTUBEATOM],GL_COMPILE);
	  glSphere(prefs.tubeSize()*prefs.selectionScale(), TRUE);
	glEndList();
	// Enlarged sphere (for selections with DS_SPHERE)
	glNewList(list_[GLOB_SELSPHEREATOM],GL_COMPILE);
	  glSphere(prefs.atomSize(Atom::SphereStyle)*prefs.selectionScale(), TRUE);
	glEndList();
	// Enlarged sphere (for selections with DS_SCALED)
	glNewList(list_[GLOB_SELUNITATOM],GL_COMPILE);
	  glSphere(prefs.selectionScale(), TRUE);
	glEndList();

	/*
	// Atoms
	*/
	// Stick Atom (for DS_STICK)
	glNewList(list_[GLOB_STICKATOM],GL_COMPILE);
	  glBegin(GL_LINES);
	    glVertex3f(-0.5f,0.0f,0.0f); glVertex3f(0.5f,0.0f,0.0f);
	    glVertex3f(0.0f,-0.5f,0.0f); glVertex3f(0.0f,0.5f,0.0f);
	    glVertex3f(0.0f,0.0f,-0.5f); glVertex3f(0.0f,0.0f,0.5f);
	  glEnd();
	glEndList();
	// Atom Sphere (for DS_TUBE)
	glNewList(list_[GLOB_TUBEATOM],GL_COMPILE);
	  glSphere(prefs.atomSize(Atom::TubeStyle)*0.98, TRUE);
	glEndList();
	// Atom Sphere (for DS_SPHERE)
	glNewList(list_[GLOB_SPHEREATOM],GL_COMPILE);
	  glSphere(prefs.atomSize(Atom::SphereStyle), TRUE);
	glEndList();
	// Unit Atom Sphere (for DS_SCALED)
	glNewList(list_[GLOB_UNITATOM],GL_COMPILE);
	  glSphere(1.0, TRUE);
	glEndList();
	// Wire Atom Sphere (for DS_TUBE)
	glNewList(list_[GLOB_WIRETUBEATOM],GL_COMPILE);
	  glSphere(prefs.tubeSize()*1.1, FALSE);
	glEndList();
	// Wire Atom Sphere (for DS_SPHERE)
	glNewList(list_[GLOB_WIRESPHEREATOM],GL_COMPILE);
	  glSphere(prefs.atomSize(Atom::SphereStyle)*1.1, FALSE);
	glEndList();
	// Wire Unit Atom Sphere (for DS_SCALED)
	glNewList(list_[GLOB_WIREUNITATOM],GL_COMPILE);
	  glSphere(1.1, FALSE);
	glEndList();
	/*
	// Cylinders (bonds)
	*/
	// Solid cylinder
	glNewList(list_[GLOB_CYLINDER],GL_COMPILE);
	  glCylinder(prefs.tubeSize(), TRUE);
	glEndList();
	// Solid selected cylinder
	glNewList(list_[GLOB_SELCYLINDER],GL_COMPILE);
	  glCylinder(prefs.tubeSize()*prefs.selectionScale(), TRUE);
	glEndList();
	// Wireframe cylinder
	glNewList(list_[GLOB_WIRECYLINDER],GL_COMPILE);
	  glCylinder(prefs.tubeSize(), FALSE);
	glEndList();
	// Selected wireframe cylinder
	glNewList(list_[GLOB_SELWIRECYLINDER],GL_COMPILE);
	  glCylinder(prefs.tubeSize()*prefs.selectionScale(), FALSE);
	glEndList();
	/*
	// Objects
	*/
	// View axes
	glNewList(list_[GLOB_GLOBE],GL_COMPILE);
	  glBegin(GL_LINES);
	    // X
	    glVertex3f(0.6f,0.0f,0.0f); glVertex3f(0.0f,0.0f,0.0f);
	    glVertex3f(0.65f,-0.05f,0.0f); glVertex3f(0.85f,0.05f,0.0f);
	    glVertex3f(0.65f,0.05f,0.0f); glVertex3f(0.85f,-0.05f,0.0f);
	    // Y
	    glVertex3f(0.0f,0.6f,0.0f); glVertex3f(0.0f,0.0f,0.0f);
	    glVertex3f(0.0f,0.65f,0.0f); glVertex3f(0.0f,0.75f,0.0f);
	    glVertex3f(0.0f,0.75f,0.0f); glVertex3f(0.05f,0.85f,0.0f);
	    glVertex3f(0.0f,0.75f,0.0f); glVertex3f(-0.05f,0.85f,0.0f);
	    // Z
	    glVertex3f(0.0f,0.0f,0.6f); glVertex3f(0.0f,0.0f,0.0f);
	    glVertex3f(-0.05f,0.0f,0.65f); glVertex3f(0.05f,0.0f,0.65f);
	    glVertex3f(0.05f,0.0f,0.65f); glVertex3f(-0.05f,0.0f,0.85f);
	    glVertex3f(-0.05f,0.0f,0.85f); glVertex3f(0.05f,0.0f,0.85f);
	  glEnd();
	  glSphere(0.5, FALSE);
	glEndList();
	// Drawing guide
	delta = extent * spacing;
	tickdelta = spacing / ticks;
	tickheight = spacing * 0.05;
	glNewList(list_[GLOB_GUIDE],GL_COMPILE);
	  glBegin(GL_LINES);
	    for (n=-extent; n<=extent; n++)
	    {
		// Horizontal gridlines
	  	glVertex3f(-delta,spacing*n,0.0f); glVertex3f(delta,spacing*n,0.0f);
		// Vertical gridlines
	  	glVertex3f(spacing*n,-delta,0.0f); glVertex3f(spacing*n,delta,0.0f);
		// Tick marks
		n == -extent ? tickbottom = spacing*n : tickbottom = spacing*n-tickheight;
		n == extent ? ticktop = spacing*n : ticktop = spacing*n+tickheight;
		for (m=0; m<ticks*extent*2; m++)
			if (m % ticks != 0)
			{
				// Ticks on horizontal gridlines
				glVertex3f(-delta+m*tickdelta,ticktop,0.0f);
				glVertex3f(-delta+m*tickdelta,tickbottom,0.0f);
				// Ticks on vertical gridlines
				glVertex3f(ticktop,-delta+m*tickdelta,0.0f);
				glVertex3f(tickbottom,-delta+m*tickdelta,0.0f);
			}
	    }
	  glEnd();
	glEndList();
	// Unit Circle
	int nsegs = 36;
	glNewList(list_[GLOB_CIRCLE],GL_COMPILE);
	  glBegin(GL_LINE_LOOP);
	    for (int i=0; i < nsegs; i++)
	    {
		float degInRad = i*(360.0/nsegs)/DEGRAD;
		glVertex2f(cos(degInRad),sin(degInRad));
	    }
	  glEnd();
	glEndList();
	// Unit Wire Cube (centred at origin)
	glNewList(list_[GLOB_WIREUNITCUBE],GL_COMPILE);
	  glBegin(GL_LINE_LOOP);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	  glEnd();
	  glBegin(GL_LINES);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(0.5,0.5,0.5);
	  glEnd();
	glEndList();
	// Unit Solid Cube (centred at origin)
	glNewList(list_[GLOB_UNITCUBE],GL_COMPILE);
	  glBegin(GL_QUADS);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(-0.5,0.5,0.5);

	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(0.5,0.5,-0.5);

	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	  glEnd();
	glEndList();
	// Cell Axis Arrows
	glNewList(list_[GLOB_CELLAXES],GL_COMPILE);
	  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	  double asize = 0.5, awidth = 0.2, posoffset = 0.5;
	  for (int i=0; i<3; i++)
	  {
		glPushMatrix();
		  if (i == 1) glRotated(-90.0,0.0,1.0,0.0);
		  else if (i == 2) glRotated(90.0,0.0,0.0,1.0);
		  glPushMatrix();
		    glScaled(0.5,awidth,awidth);
		    glTranslated(0.5,0.0,0.0);
		    glCallList(list_[GLOB_UNITCUBE]);
		  glPopMatrix();
		  glTranslated(posoffset,0.0,0.0);
		  glBegin(GL_TRIANGLE_FAN);
		    glVertex3d(asize,0.0,0.0);
		    glVertex3d(0.0,awidth,awidth);
		    glVertex3d(0.0,awidth,-awidth);
		    glVertex3d(0.0,-awidth,-awidth);
		    glVertex3d(0.0,-awidth,awidth);
		    glVertex3d(0.0,awidth,awidth);
		  glEnd();
		glPopMatrix();
	  }
	glEndList();

	dbgEnd(Debug::Calls,"Canvas::createLists");
}

/*
// Configuration
*/

// Calculate Projection
void Canvas::doProjection()
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!gui.exists()) return;
	dbgBegin(Debug::Calls,"Canvas::doProjection");
	double pmat[16], bottom, top;
	// Check source
	if (beginGl())
	{
		// Set the viewport size to the whole area and grab the matrix
		glViewport(0,0,(int)width_,(int)height_);
		glGetIntegerv(GL_VIEWPORT,VMAT);
		// Calculate and store a projection matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		aspect_ = width_ / height_;
		if (prefs.hasPerspective())
		{
			// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
			bottom = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
			top = -bottom;
			glFrustum(aspect_*top,aspect_*bottom,top,bottom,prefs.clipNear(),prefs.clipFar());
		}
		else
		{
			bottom = displayModel_->orthoSize();
			top = -bottom;
			//glOrtho(aspect*top,aspect*bottom,top,bottom,-bottom*2.0,bottom*2.0);
			glOrtho(aspect_*top,aspect_*bottom,top,bottom,-prefs.clipNear(),prefs.clipFar());
		}
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		PMAT.setFromColumnMajor(pmat);
		// Rotation globe projection matrix (square)
		glLoadIdentity();
		glFrustum(-1.0, 1.0, -1.0, 1.0, 0.0, 10.0);
		glGetDoublev(GL_PROJECTION_MATRIX,pmat); // Store the resulting projection and
		GlobePMAT.setFromColumnMajor(pmat);
		glMatrixMode(GL_MODELVIEW);
		// Calculate the new drawpixelwidth
		calculateDrawPixelWidth();
		endGl();
	}
	else printf("Canvas::doProjection <<<< Failed to reset projection matrix >>>>\n");
	dbgEnd(Debug::Calls,"Canvas::doProjection");
}

/*
// Misc
*/

// Set valid
void Canvas::setValid(bool b)
{
	// Wait until the canvas is not drawing
	while (!valid_) gui.processEvents();
	// Now disallow drawing before we set the new status
	valid_ = FALSE;
	drawing_ = FALSE;
	valid_ = b;
}

/*
// Save vector image
*/
void Canvas::saveVector(Model *source, vector_format vf, const char *filename)
{
	// Open output file
	FILE *vectorfile = fopen(filename, "w");
	if (vectorfile == NULL)
	{
		msg(Debug::None,"Couldn't open output file for vector export.\n");
		return;
	}
	GLint result = GL2PS_OVERFLOW, bufsize = 0;
	// Loop until the feedback buffer is large enough
	while (result == GL2PS_OVERFLOW)
	{
		bufsize += 1024*1024;
		result = gl2psBeginPage(source->name(), "Aten", VMAT, vf, GL2PS_BSP_SORT, GL2PS_DRAW_BACKGROUND | GL2PS_OCCLUSION_CULL, GL_RGBA, 0, 0, 0, 0, 0, bufsize, vectorfile, filename);
		printf("Result = %i\n",result);
		renderScene(source);
		result = gl2psEndPage();
		printf("Result = %i\n",result);
	}
}

/*
// Modes
*/

// Returns the atom currently under the mouse
Atom *Canvas::atomHover()
{
	return atomHover_;
}

// Clears the subsel of atoms
void Canvas::clearSubselection()
{
	subselection_.clear();
}

// Set the active mode to the current user mode
void Canvas::useSelectedMode()
{
	activeMode_ = selectedMode_;
}

// Return the currently selected mode
UserAction Canvas::selectedMode()
{
	return selectedMode_;
}
