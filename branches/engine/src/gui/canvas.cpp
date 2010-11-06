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
#include "gui/canvas.h"
#include "gui/tcanvas.uih"
#include "gui/mainwindow.h"
#include "model/model.h"
#ifdef _WIN32
#include "glext.h"
#endif

// Constructor
TextObject::TextObject(int xx, int yy, bool ralign, const char *txt) : x(xx), y(yy), rightAlign(ralign)
{
	text = txt;
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
	activeMode_ = UserAction::NoAction;
	selectedMode_ = UserAction::SelectAction;
// 	temporaryGlobList_[0] = 0;
// 	globList_[0] = 0;
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
	editable_ = TRUE;
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

// Return the current display model
Model *Canvas::displayModel() const
{
	return displayModel_;
}

// Render a scene based on the specified model
void Canvas::renderModel(Model *source)
{
	msg.enter("Canvas::renderModel");
	static Model *lastDisplayed_ = NULL;

	msg.print(Messenger::GL, " --> RENDERING BEGIN\n");

	// If the canvas is stil restricted, don't draw anything
	if (noDraw_)
	{
		msg.exit("Canvas::renderModel");
		msg.print(Messenger::GL, " --> RENDERING END (NODRAW)\n");
		return;
	}
	checkGlError();

	// Begin the GL commands
	if (!beginGl())
	{
		msg.exit("Canvas::renderModel");
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
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		msg.exit("Canvas::renderModel");
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

	// Render model
	engine_.renderModel(displayModel_);

	// Setup pen colour
	GLfloat fgcol[4];
	prefs.copyColour(Prefs::ForegroundColour, fgcol);
	glDisable(GL_COLOR_MATERIAL);
	glColor4fv(fgcol);

	//glFlush();
	endGl();
	checkGlError();

	msg.print(Messenger::GL, " --> RENDERING END\n");
	lastDisplayed_ = source;

	msg.exit("RenderEngine::renderModel");

}

// Set GL options
void Canvas::initGl()
{
	if (!valid_) return;
	msg.enter("Canvas::initGl");
// 	if (beginGl())    TGAY
// 	{
		// Create lists for globs if this is the first call to init_gl()
// 		if (renderOffScreen_)
// 		{
// 			temporaryGlobList_[Canvas::StickAtomGlob] = glGenLists(Canvas::nGlobs);
// 			for (int n=1; n<Canvas::nGlobs; n++) temporaryGlobList_[n] = temporaryGlobList_[Canvas::StickAtomGlob]+n;
			//printf("Model glob id = %i\n", list_[ModelGlob]);
// 		}
// 		else if (globList_[0] == 0)
// 		{
// 			globList_[StickAtomGlob] = glGenLists(Canvas::nGlobs);
// // 			msg.print(Messenger::GL, "Beginning of GL display list is %d\n", glob(StickAtomGlob));
// 			for (int n=1; n<Canvas::nGlobs; n++) globList_[n] = globList_[Canvas::StickAtomGlob]+n;
// 		}
// 		createLists();
// 		endGl();
// 	}
// 	else printf("Failed to set-up OpenGL on canvas.\n");
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
		glDisable(GL_MULTISAMPLE);
		// Configure antialiasing
		if (prefs.multiSampling()) glEnable(GL_MULTISAMPLE);
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

/*
// Configuration
*/

// Calculate Projection
void Canvas::doProjection()
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!valid_) return;
	msg.enter("Canvas::doProjection");
	// Check source
	if (beginGl())
	{
		// Set the viewport size to the whole area and grab the matrix
		engine_.setupView(0, 0, width_, height_);
		// Rotation globe projection matrix (square)
/*		glLoadIdentity();
		glFrustum(-1.0, 1.0, -1.0, 1.0, 0.0, 10.0);
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		GlobePMAT.setFromColumnMajor(pmat);*/    // TGAY
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
UserAction::Action Canvas::selectedMode() const
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
	if (selectedMode_ != UserAction::ManualPickAction) actionBeforePick_ = gui.mainWindow->uaGroup->checkedAction();
	setSelectedMode(UserAction::ManualPickAction);
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

// Set whether to accept editing actions (i.e. anything other than view manipulation)
void Canvas::setEditable(bool b)
{
	editable_ = b;
}

// Return whether to accept editing actions (i.e. anything other than view manipulation)
bool Canvas::editable()
{
	return editable_;
}
