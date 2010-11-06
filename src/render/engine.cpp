/*
	*** Rendering Engine
	*** src/render/engine.cpp
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
#include "base/messenger.h"
#include "classes/prefs.h"
#include "model/model.h"
#include <math.h>

// Constructor
RenderEngine::RenderEngine()
{
	// Type
	type_ = NoFilter;

	createPrimitives();
}

/*
// Primitive Generation
*/

// (Re)Generate primitives
void RenderEngine::createPrimitives()
{
	msg.enter("RenderEngine::createPrimitives");
	// Atom Styles
	atomStyle_[Atom::StickStyle].createEmpty(GL_LINES, 2, 6);
	atomStyle_[Atom::StickStyle].addVertexAndNormal(-0.5,0.0,0.0,1.0,0.0,0.0);
	atomStyle_[Atom::StickStyle].addVertexAndNormal(0.5,0.0,0.0,1.0,0.0,0.0);
	atomStyle_[Atom::StickStyle].addVertexAndNormal(0.0,-0.5,0.0,1.0,0.0,0.0);
	atomStyle_[Atom::StickStyle].addVertexAndNormal(0.0,0.5,0.0,1.0,0.0,0.0);
	atomStyle_[Atom::StickStyle].addVertexAndNormal(0.0,0.0,-0.5,1.0,0.0,0.0);
	atomStyle_[Atom::StickStyle].addVertexAndNormal(0.0,0.0,0.5,1.0,0.0,0.0);
	atomStyle_[Atom::TubeStyle].createSphere(prefs.atomStyleRadius(Atom::TubeStyle), prefs.atomDetail(), prefs.atomDetail());
	atomStyle_[Atom::SphereStyle].createSphere(prefs.atomStyleRadius(Atom::SphereStyle), prefs.atomDetail(), prefs.atomDetail());
	atomStyle_[Atom::ScaledStyle].createSphere(1.0, prefs.atomDetail(), prefs.atomDetail());
	msg.exit("RenderEngine::createPrimitives");
}

/*
// View Control
*/

// Set-up viewport and projection matrices
void RenderEngine::setupView(GLint x, GLint y, GLint w, GLint h)
{
	// Setup and store viewport matrix
	viewportMatrix_[0] = x;
	viewportMatrix_[1] = y;
	viewportMatrix_[2] = w;
	viewportMatrix_[3] = h;
	glViewport( x, y, w, h );
	// Create projection matrix
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	GLdouble top, bottom, aspect = (GLdouble) w / (GLdouble) h;
	if (prefs.hasPerspective())
	{
		// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
		bottom = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
		top = -bottom;
		glFrustum(aspect*top, aspect*bottom, top, bottom, prefs.clipNear(), prefs.clipFar());
	}
	else
	{
		top = tan(prefs.perspectiveFov() / DEGRAD) * 1.0; // TGAY (displayModel_ == NULL ? 1.0 : displayModel_->camera().z);
		bottom = -top;
		glOrtho(aspect*top, aspect*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
	}
	GLdouble pmat[16];
	glGetDoublev(GL_PROJECTION_MATRIX,pmat);
	projectionMatrix_.setFromColumnMajor(pmat);
}

// Set current transformation matrix
void RenderEngine::setTransformationMatrix(Mat4<double> &mat)
{
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	transformationMatrix_ = mat;
	double m[16];
	transformationMatrix_.copyColumnMajor(m);
	glMultMatrixd(m);
}

/*
// Object Rendering
*/

// Render primitive at requested local position in specified colour, returning projected position
Vec3<double> &RenderEngine::renderPrimitive(Primitive *primitive, Vec3<double> pos, GLfloat *ambient, GLfloat *diffuse)
{
	static Vec3<double> projected;
	double alphadelta = 1.0-ambient[4];
	// Filter type determines what to do here...
	if ((type_ == NoFilter) || ((type_ == TransparencyFilter) && (alphadelta < 0.001)))
	{
		// Pass through direct to GL
		glTranslated(pos.x, pos.y, pos.z);
		glMaterialfv(GL_FRONT, GL_AMBIENT, ambient);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, ambient);
		primitive->sendToGL();
		glTranslated(-pos.x, -pos.y, -pos.z);
		// Calculate projected coordinate
		projected = transformationMatrix_ * pos;
	}
	else
	{
		// Store triangulation in local polygon list
	}
}

// Render scaled primitive at requested local position in specified colour, returning projected position
Vec3<double> &RenderEngine::renderPrimitiveScaled(Primitive *prmtv, GLfloat scale, Vec3<double> pos, GLfloat *ambient, GLfloat *diffuse)
{
	static Vec3<double> projected;
	double alphadelta = 1.0-ambient[4];
	// Filter type determines what to do here...
	if ((type_ == NoFilter) || ((type_ == TransparencyFilter) && (alphadelta < 0.001)))
	{
		// Pass through direct to GL
		glTranslated(pos.x, pos.y, pos.z);
		glMaterialfv(GL_FRONT, GL_AMBIENT, ambient);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, diffuse);
		prmtv->sendScaledToGL(scale);
		glTranslated(-pos.x, -pos.y, -pos.z);
		// Calculate projected coordinate
		projected = transformationMatrix_ * pos;
	}
	else
	{
		// Store triangulation in local polygon list
	}
}

// Render specified model
void RenderEngine::renderModel(Model *source)
{
	GLfloat ambient[4], diffuse[4], scaledradius;

	// Set transformation matrix
	setTransformationMatrix(source->viewMatrix());

	// Grab style values....
	Prefs::ColouringScheme scheme = prefs.colourScheme();
	Atom::DrawStyle style = prefs.renderStyle();
	scaledradius = prefs.atomStyleRadius(Atom::ScaledStyle);

	// Atoms
	for (Atom *i = source->atoms(); i != NULL; i = i->next)
	{
		// Skip hidden atoms
		if (i->isHidden()) continue;
		// Select colour
		if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, ambient);
		else switch (scheme)
		{
			case (Prefs::ElementScheme):
				elements().copyAmbientColour(i->element(), ambient);
// 				elements().copyDiffuseColour(i->element(), diffuse);
				break;
			case (Prefs::ChargeScheme):
				prefs.colourScale[0].colour(i->charge(), ambient);
// 				prefs.colourScale[0].colour(i->charge(), diffuse);
				break;
			case (Prefs::VelocityScheme):
				prefs.colourScale[1].colour(i->v().magnitude(), ambient);
// 				prefs.colourScale[1].colour(cval, diffuse);
				break;
			case (Prefs::ForceScheme):
				prefs.colourScale[2].colour(i->f().magnitude(), ambient);
// 				prefs.colourScale[2].colour(cval, diffuse);
				break;
			case (Prefs::CustomScheme):
				i->copyColour(ambient);
// 				i->copyColour(diffuse);
				break;
			default:
				break;
		}
		// Set diffuse colour as 0.75*ambient
		diffuse[0] = ambient[0] * 0.75;
		diffuse[1] = ambient[1] * 0.75;
		diffuse[2] = ambient[2] * 0.75;
		diffuse[3] = ambient[3];
		if (style != Atom::IndividualStyle)
		{
			if (style != Atom::ScaledStyle) renderPrimitive(&atomStyle_[style], i->r(), ambient, diffuse);
			else renderPrimitiveScaled(&atomStyle_[Atom::ScaledStyle], scaledradius*elements().atomicRadius(i), i->r(), ambient, diffuse);
		}
		else if (i->style() != Atom::ScaledStyle) renderPrimitive(&atomStyle_[i->style()], i->r(), ambient, diffuse);
		else renderPrimitiveScaled(&atomStyle_[Atom::ScaledStyle], scaledradius*elements().atomicRadius(i), i->r(), ambient, diffuse);
	}
}
