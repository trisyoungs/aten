/*
	*** Rendering Canvas
	*** src/render/canvas.cpp
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

#include "gui/gui.h"
#include "render/canvas.h"
#include "gui/tcanvas.uih"
#include "gui/mainwindow.h"
#include "model/model.h"

// Constructor
TextObject::TextObject(int xx, int yy, bool ralign, const char *txt) : x(xx), y(yy), rightAlign(ralign)
{
	strcpy(text,txt);
	prev = NULL;
	next = NULL;
}

// Constructor
Canvas::Canvas()
{
	// Private variables
	valid_ = FALSE;
	drawing_ = FALSE;
	noDraw_ = TRUE;
	renderOffScreen_ = FALSE;
	displayModel_ = NULL;
	displayFrameId_ = -1;
	atomClicked_ = NULL;
	activeMode_ = Canvas::NoAction;
	selectedMode_ = Canvas::SelectAction;
	temporaryGlobList_[0] = 0;
	globList_[0] = 0;
	contextWidget_ = NULL;
	pickEnabled_ = FALSE;
	for (int i=0; i<3; i++)
	{
		mouseButton_[i] = FALSE;
		keyModifier_[i] = FALSE;
	}
	width_ = 0;
	height_ = 0;
	actionBeforePick_ = NULL;
	pickAtomsCallback_ = NULL;
	nAtomsToPick_ = -1;
	currentDrawDepth_ = 0.0;
}

// Set the internal name of the canvas
void Canvas::setName(const char *s)
{
	name_ = s;
}

// Return the current height of the drawing area
GLsizei Canvas::height() const
{
	return height_;
}

// Return the current width of the drawing area
GLsizei Canvas::width() const
{
	return width_;
}

// Return whether the canvas is currently drawing
bool Canvas::isDrawing() const
{
	return drawing_;
}

// Return if the canvas is valid
bool Canvas::isValid() const
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

void Canvas::checkGlError()
{
	// Do GL error check
	if (msg.isOutputActive(Messenger::GL))
	{
		GLenum glerr = GL_NO_ERROR;
		do
		{
			switch (glGetError())
			{
				case (GL_INVALID_ENUM): msg.print(Messenger::GL, "GLenum argument out of range\n"); break;
				case (GL_INVALID_VALUE): msg.print(Messenger::GL, "Numeric argument out of range\n"); break;
				case (GL_INVALID_OPERATION): msg.print(Messenger::GL, "Operation illegal in current state\n"); break;
				case (GL_STACK_OVERFLOW): msg.print(Messenger::GL, "Command would cause a stack overflow\n"); break;
				case (GL_STACK_UNDERFLOW): msg.print(Messenger::GL, "Command would cause a stack underflow\n"); break;
				case (GL_OUT_OF_MEMORY): msg.print(Messenger::GL, "Not enough memory left to execute command\n"); break;
				case (GL_NO_ERROR): msg.print(Messenger::GL, "No GL error\n"); break;
				default:
					msg.print(Messenger::GL, "Unknown GL error?\n");
					break;
			}
		} while (glerr != GL_NO_ERROR);
	}
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

// Invalidate
void Canvas::postRedisplay()
{
	if (!valid_) return;
	contextWidget_->updateGL();
	if (prefs.manualSwapBuffers()) contextWidget_->swapBuffers();
}

// Widget configure
void Canvas::configure(int w, int h)
{
	// Store the new width and height of the widget and re-do projection
	width_ = (GLsizei) w;
	height_ = (GLsizei) h;
	doProjection();
	// Flag that render source needs to be reprojected
	if (displayModel_ != NULL) displayModel_->changeLog.add(Log::Visual);
	if (prefs.manualSwapBuffers()) contextWidget_->swapBuffers();
}

// Enable drawing
void Canvas::enableDrawing()
{
	noDraw_ = FALSE;
}

// Disable drawing
void Canvas::disableDrawing()
{
	noDraw_ = TRUE;
}

// Set whether offscreen rendering is being performed
void Canvas::setOffScreenRendering(bool b)
{
	renderOffScreen_ = b;
}

// Return whether offscreen rendering is being performed
bool Canvas::offScreenRendering() const
{
	return renderOffScreen_;
}

/*
// Rendering
*/

GLuint Canvas::glob(Canvas::GlObject ob) const
{
	return (renderOffScreen_ ? temporaryGlobList_[ob] : globList_[ob]);
}

// Return the current display model
Model *Canvas::displayModel() const
{
	return displayModel_;
}

// Set GL options
void Canvas::initGl()
{
	if (!valid_) return;
	msg.enter("Canvas::initGl");
	if (beginGl())
	{
		// Create lists for globs if this is the first call to init_gl()
		if (renderOffScreen_)
		{
			temporaryGlobList_[Canvas::StickAtomGlob] = glGenLists(Canvas::nGlobs);
			for (int n=1; n<Canvas::nGlobs; n++) temporaryGlobList_[n] = temporaryGlobList_[Canvas::StickAtomGlob]+n;
			//printf("Model glob id = %i\n", list_[ModelGlob]);
		}
		else if (globList_[0] == 0)
		{
			globList_[StickAtomGlob] = glGenLists(Canvas::nGlobs);
			msg.print(Messenger::GL, "Beginning of GL display list is %d\n", glob(StickAtomGlob));
			for (int n=1; n<Canvas::nGlobs; n++) globList_[n] = globList_[Canvas::StickAtomGlob]+n;
		}
		createLists();
		endGl();
	}
	else printf("Failed to set-up OpenGL on canvas.\n");
	msg.exit("Canvas::initGl");
}

// Set OpenGL options ready for drawing
void Canvas::prepGl()
{
	msg.enter("Canvas::prepGl");
	if (beginGl())
	{
		// Clear colour
		GLfloat col[4];
		prefs.copyColour(Prefs::BackgroundColour, col);
		glClearColor(col[0],col[1],col[2],col[3]);
		//glClearDepth(1.0);
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
		prefs.copySpotlightColour(Prefs::AmbientComponent, col);
		glLightfv(GL_LIGHT0, GL_AMBIENT, col);
		prefs.copySpotlightColour(Prefs::DiffuseComponent, col);
		glLightfv(GL_LIGHT0, GL_DIFFUSE, col);
		prefs.copySpotlightColour(Prefs::SpecularComponent, col);
		glLightfv(GL_LIGHT0, GL_SPECULAR, col);
		prefs.copySpotlightPosition(col);
		glLightfv(GL_LIGHT0, GL_POSITION, col);
		prefs.spotlightActive() ? glEnable(GL_LIGHT0) : glDisable(GL_LIGHT0);
		glDisable(GL_BLEND);
		glDisable(GL_LINE_SMOOTH);
		glDisable(GL_POLYGON_SMOOTH);
		// Configure antialiasing
		if (prefs.lineAliasing())
		{
			glEnable(GL_BLEND);
			glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_LINE_SMOOTH);
		}
		if (prefs.polygonAliasing())
		{
			glEnable(GL_BLEND);
			glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_POLYGON_SMOOTH);
		}
		// Configure fog effects
		if (prefs.depthCue())
		{
			glFogi(GL_FOG_MODE, GL_LINEAR);
			prefs.copyColour(Prefs::BackgroundColour, col);
			glFogfv(GL_FOG_COLOR, col);
			glFogf(GL_FOG_DENSITY, 0.35f);
			glHint(GL_FOG_HINT, GL_NICEST);
			glFogi(GL_FOG_START, prefs.depthNear());
			glFogi(GL_FOG_END, prefs.depthFar());
			glEnable(GL_FOG);
		}
		else glDisable(GL_FOG);
		// Configure face culling
		glCullFace(GL_BACK);
		prefs.backfaceCulling() ? glEnable(GL_CULL_FACE) : glDisable(GL_CULL_FACE);
		// Test
		glDisable(GL_DITHER);
		glDisable(GL_LOGIC_OP);
		// End Test
		endGl();
	}
	else printf("Failed to prepare OpenGL on canvas.\n");
	msg.exit("Canvas::prepGl");
}

// Create display lists
void Canvas::createLists()
{
	if (!isValid()) return;
	msg.enter("Canvas::createLists");

	int n,m, ticks, extent;
	double delta, tickdelta, tickheight, ticktop, tickbottom, spacing;
	// Grab some oft-used values
	spacing = prefs.guideSpacing();
	extent = prefs.guideExtent();
	ticks = prefs.guideTicks();

	/*
	// Selected Atoms
	*/
	// Enlarged sphere (for selections with DS_TUBE)
	glNewList(glob(SelectedTubeAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.atomStyleRadius(Atom::TubeStyle)*prefs.selectionScale(), TRUE);
	glEndList();
	// Enlarged sphere (for selections with DS_SPHERE)
	glNewList(glob(SelectedSphereAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.atomStyleRadius(Atom::SphereStyle)*prefs.selectionScale(), TRUE);
	glEndList();
	// Enlarged sphere (for selections with DS_SCALED)
	glNewList(glob(SelectedUnitAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.selectionScale(), TRUE);
	glEndList();

	/*
	// Atoms
	*/
	// Stick Atom (for DS_STICK)
	glNewList(glob(StickAtomGlob),GL_COMPILE);
	  glBegin(GL_LINES);
	    glVertex3d(-0.5,0.0,0.0); glVertex3d(0.5,0.0,0.0);
	    glVertex3d(0.0,-0.5,0.0); glVertex3d(0.0,0.5,0.0);
	    glVertex3d(0.0,0.0,-0.5); glVertex3d(0.0,0.0,0.5);
	  glEnd();
	glEndList();
	// Atom Sphere (for DS_TUBE)
	glNewList(glob(TubeAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.atomStyleRadius(Atom::TubeStyle), TRUE);
	glEndList();
	// Atom Sphere (for DS_SPHERE)
	glNewList(glob(SphereAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.atomStyleRadius(Atom::SphereStyle), TRUE);
	glEndList();
	// Unit Atom Sphere (for DSƒ_SCALED)
	glNewList(glob(UnitAtomGlob),GL_COMPILE);
	  spherePrimitive(1.0, TRUE);
	glEndList();
	// Wire Atom Sphere (for DS_TUBE)
	glNewList(glob(WireTubeAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.atomStyleRadius(Atom::TubeStyle), FALSE);
	glEndList();
	// Wire Atom Sphere (for DS_SPHERE)
	glNewList(glob(WireSphereAtomGlob),GL_COMPILE);
	  spherePrimitive(prefs.atomStyleRadius(Atom::SphereStyle), FALSE);
	glEndList();
	// Wire Unit Atom Sphere (for DS_SCALED)
	glNewList(glob(WireUnitAtomGlob),GL_COMPILE);
	  spherePrimitive(1.0, FALSE);
	glEndList();
	/*
	// Cylinders (bonds)
	*/
	// Solid cylinder of radius 1
	glNewList(glob(CylinderGlob),GL_COMPILE);
	  cylinderPrimitive(1.0, 1.0, TRUE);
	glEndList();
	// Solid selected cylinder
	glNewList(glob(SelectedCylinderGlob),GL_COMPILE);
	  cylinderPrimitive(prefs.selectionScale(), prefs.selectionScale(), TRUE);
	glEndList();
	// Wireframe cylinder
	glNewList(glob(WireCylinderGlob),GL_COMPILE);
	  cylinderPrimitive(1.0, 1.0, FALSE);
	glEndList();
	// Selected wireframe cylinder
	glNewList(glob(SelectedWireCylindedGlob),GL_COMPILE);
	  cylinderPrimitive(prefs.selectionScale(), prefs.selectionScale(), FALSE);
	glEndList();
	/*
	// Objects
	*/
	// Cylinder Arrow
	glNewList(glob(TubeArrowGlob),GL_COMPILE);
	  glPushMatrix();
	    glScaled(1.0,1.0,0.6);
	    cylinderPrimitive(0.1, 0.1, TRUE);
	  glPopMatrix();
	  glTranslated(0.0,0.0,0.6);
	  glPushMatrix();
	    glScaled(1.0,1.0,0.4);
	    cylinderPrimitive(0.2, 0.0, TRUE);
	  glPopMatrix();
	glEndList();
	// View axes
	glNewList(glob(GlobeGlob),GL_COMPILE);
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
	    glVertex3d(0.0f,0.0f,0.6f); glVertex3d(0.0f,0.0f,0.0f);
	    glVertex3d(-0.05f,0.0f,0.65f); glVertex3d(0.05f,0.0f,0.65f);
	    glVertex3d(0.05f,0.0f,0.65f); glVertex3d(-0.05f,0.0f,0.85f);
	    glVertex3d(-0.05f,0.0f,0.85f); glVertex3d(0.05f,0.0f,0.85f);
	  glEnd();
	  spherePrimitive(0.5, FALSE);
	glEndList();
	// Drawing guide
	delta = extent * spacing;
	tickdelta = spacing / ticks;
	tickheight = spacing * 0.05;
	glNewList(glob(GuideGlob),GL_COMPILE);
	  glBegin(GL_LINES);
	    for (n=-extent; n<=extent; n++)
	    {
		// Horizontal gridlines
	  	glVertex3d(-delta,spacing*n,0.0f);
		glVertex3d(delta,spacing*n,0.0f);
		// Vertical gridlines
	  	glVertex3d(spacing*n,-delta,0.0f);
		glVertex3d(spacing*n,delta,0.0f);
		// Tick marks
		n == -extent ? tickbottom = spacing*n : tickbottom = spacing*n-tickheight;
		n == extent ? ticktop = spacing*n : ticktop = spacing*n+tickheight;
		for (m=0; m<ticks*extent*2; m++)
			if (m % ticks != 0)
			{
				// Ticks on horizontal gridlines
				glVertex3d(-delta+m*tickdelta,ticktop,0.0f);
				glVertex3d(-delta+m*tickdelta,tickbottom,0.0f);
				// Ticks on vertical gridlines
				glVertex3d(ticktop,-delta+m*tickdelta,0.0f);
				glVertex3d(tickbottom,-delta+m*tickdelta,0.0f);
			}
	    }
	  glEnd();
	glEndList();
	// Unit Circle
	int nsegs = 36;
	double degInRad;
	glNewList(glob(CircleGlob),GL_COMPILE);
	  glBegin(GL_LINE_LOOP);
	    for (int i=0; i < nsegs; i++)
	    {
		degInRad = i*(360.0/nsegs)/DEGRAD;
		glVertex2d(cos(degInRad), sin(degInRad));
	    }
	  glEnd();
	glEndList();
	// Unit Wire Cube (centred at origin)
	glNewList(glob(WireUnitCubeGlob),GL_COMPILE);
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
	glNewList(glob(UnitCubeGlob),GL_COMPILE);
	  glPolygonMode(GL_FRONT_AND_BACK, GL_POLYGON);
	  glBegin(GL_QUADS);
	    // Back face, in plane z = -0.5
	    glNormal3d(0.0,0.0,-1.0);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    // Front face, in plane z = 0.5
	    glNormal3d(0.0,0.0,1.0);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    // Bottom face, in plane y = -0.5
	    glNormal3d(0.0,-1.0,0.0);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    // Top face, in plane y = 0.5
	    glNormal3d(0.0,1.0,0.0);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    // Left face, in plane x = -0.5
	    glNormal3d(-1.0,0.0,0.0);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    // Right face, in plane x = 0.5
	    glNormal3d(1.0,0.0,0.0);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	  glEnd();
	glEndList();
	// Unit cube with crosses on each face
	glNewList(glob(CrossedUnitCubeGlob), GL_COMPILE);
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
	  // Crosses
	  glBegin(GL_LINES);
	    glVertex3d(0.25,0.25,0.5);
	    glVertex3d(-0.25,-0.25,0.5);
	    glVertex3d(0.25,0.25,-0.5);
	    glVertex3d(-0.25,-0.25,-0.5);
	    glVertex3d(0.5,0.25,0.25);
	    glVertex3d(0.5,-0.25,-0.25);
	    glVertex3d(-0.5,0.25,0.25);
	    glVertex3d(-0.5,-0.25,-0.25);
	    glVertex3d(0.25,0.5,0.25);
	    glVertex3d(-0.25,0.5,-0.25);
	    glVertex3d(0.25,-0.5,0.25);
	    glVertex3d(-0.25,-0.5,-0.25);
	    glVertex3d(0.25,-0.25,0.5);
	    glVertex3d(-0.25,0.25,0.5);
	    glVertex3d(0.25,-0.25,-0.5);
	    glVertex3d(-0.25,0.25,-0.5);
	    glVertex3d(0.5,-0.25,0.25);
	    glVertex3d(0.5,0.25,-0.25);
	    glVertex3d(-0.5,-0.25,0.25);
	    glVertex3d(-0.5,0.25,-0.25);
	    glVertex3d(-0.25,0.5,0.25);
	    glVertex3d(0.25,0.5,-0.25);
	    glVertex3d(-0.25,-0.5,0.25);
	    glVertex3d(0.25,-0.5,-0.25);
	  glEnd();
	glEndList();
	// Cell Axis Arrows
	glNewList(glob(CellAxesGlob),GL_COMPILE);
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
		    glCallList(glob(UnitCubeGlob));
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

	msg.exit("Canvas::createLists");
}

/*
// Configuration
*/

// Calculate Projection
void Canvas::doProjection()
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!valid_) return;
	msg.enter("Canvas::doProjection");
	GLdouble pmat[16], bottom, top;
	// Check source
	if (beginGl())
	{
		// Set the viewport size to the whole area and grab the matrix
		glViewport( (GLint) 0, (GLint) 0, (GLsizei) width_, (GLsizei) height_);
		glGetIntegerv(GL_VIEWPORT,VMAT);
		// Calculate and store a projection matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		aspect_ = (width_*1.0) / height_;
		if (prefs.hasPerspective())
		{
			// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
			bottom = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
			top = -bottom;
			glFrustum(aspect_*top, aspect_*bottom, top, bottom, prefs.clipNear(), prefs.clipFar());
		}
		else
		{
			top = tan(prefs.perspectiveFov() / DEGRAD) * (displayModel_ == NULL ? 1.0 : displayModel_->camera().z);
			bottom = -top;
			glOrtho(aspect_*top, aspect_*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
		}
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		PMAT.setFromColumnMajor(pmat);
		// Rotation globe projection matrix (square)
		glLoadIdentity();
		glFrustum(-1.0, 1.0, -1.0, 1.0, 0.0, 10.0);
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		GlobePMAT.setFromColumnMajor(pmat);
		glMatrixMode(GL_MODELVIEW);
		endGl();
	}
	else printf("Canvas::doProjection <<<< Failed to reset projection matrix >>>>\n");
	msg.exit("Canvas::doProjection");
}

/*
// Misc
*/

// Set valid
void Canvas::setValid(bool b)
{
	// Now disallow drawing before we set the new status
	valid_ = FALSE;
	drawing_ = FALSE;
	valid_ = b;
}

/*
// Save vector image
*/
// void Canvas::saveVector(Model *source, vector_format vf, const char *filename)
// {
// 	// Open output file
// 	FILE *vectorfile = fopen(filename, "w");
// 	if (vectorfile == NULL)
// 	{
// 		msg.print("Couldn't open output file for vector export.\n");
// 		return;
// 	}
// 	GLint result = GL2PS_OVERFLOW, bufsize = 0;
// 	// Loop until the feedback buffer is large enough
// 	while (result == GL2PS_OVERFLOW)
// 	{
// 		bufsize += 1024*1024;
// 		result = gl2psBeginPage(source->name(), "Aten", VMAT, vf, GL2PS_BSP_SORT, GL2PS_DRAW_BACKGROUND | GL2PS_OCCLUSION_CULL, GL_RGBA, 0, 0, 0, 0, 0, bufsize, vectorfile, filename);
// 		//printf("Result = %i\n",result);
// 		renderScene(source);
// 		result = gl2psEndPage();
// 		//printf("Result = %i\n",result);
// 	}
// }

/*
// Modes
*/

// Returns the atom currently under the mouse
Atom *Canvas::atomClicked()
{
	return atomClicked_;
}

// Clears the list of picked atoms
void Canvas::clearPicked()
{
	pickedAtoms_.clear();
}

// Set the active mode to the current user mode
void Canvas::useSelectedMode()
{
	activeMode_ = selectedMode_;
}

// Return the currently selected mode
Canvas::UserAction Canvas::selectedMode() const
{
	return selectedMode_;
}

// Enter picking mode
void Canvas::beginManualPick(int natoms, void (*callback)(Reflist<Atom,int>*))
{
	msg.enter("Canvas::beginManualPick");
	// End old mode
	endManualPick(FALSE);
	// Store the current usermode, unless it is already PickAtomsAction
	if (selectedMode_ != Canvas::ManualPickAction) actionBeforePick_ = gui.mainWindow->uaGroup->checkedAction();
	setSelectedMode(Canvas::ManualPickAction);
	gui.mainWindow->dummyToolButton->activate(QAction::Trigger);
	pickAtomsCallback_ = callback;
	nAtomsToPick_ = natoms;
	msg.exit("Canvas::beginManualPick");
}

// End manual picking
void Canvas::endManualPick(bool resetaction)
{
	msg.enter("Canvas::endManualPick");
	// If a previous callback was defined then call it before we move on
	if (pickAtomsCallback_ != NULL) (*pickAtomsCallback_)(&pickedAtoms_);
	pickAtomsCallback_ = NULL;
	if (resetaction)
	{
		if (actionBeforePick_ == NULL) gui.mainWindow->ui.actionSelectAtoms->activate(QAction::Trigger);
		else actionBeforePick_->activate(QAction::Trigger);
	}
	pickedAtoms_.clear();
	nAtomsToPick_ = -1;
	gui.mainView.postRedisplay();
	msg.exit("Canvas::endManualPick");
}
