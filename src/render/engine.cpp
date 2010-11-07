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

	// Primitives
	scaledAtom_ = new PrimitiveGroup[elements().nElements()];
	selectedScaledAtom_ = new PrimitiveGroup[elements().nElements()];

	createPrimitives();
}

/*
// Primitive Generation
*/

// (Re)Generate primitives
void RenderEngine::createPrimitives()
{
	msg.enter("RenderEngine::createPrimitives");
	double radius;
	int lod, nstacks, nslices;
	// Atom Styles (Atom::StickStyle, Atom::TubeStyle, and Atom::SphereStyle)
	for (lod=0; lod < prefs.levelsOfDetail(); ++lod)
	{
		atom_[Atom::StickStyle].primitive(lod).createCross(0.5,3-lod);
		nstacks = max(2,(int) (prefs.atomDetail()*(1.0-lod*0.2)));
		nslices = max(2,(int) (prefs.atomDetail()*(1.0-lod*0.2)));
		atom_[Atom::TubeStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::TubeStyle), nstacks, nslices);
		atom_[Atom::SphereStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::SphereStyle), nstacks, nslices);
		selectedAtom_[Atom::TubeStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::TubeStyle)*prefs.selectionScale(), nstacks, nslices);
		selectedAtom_[Atom::SphereStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::SphereStyle)*prefs.selectionScale(), nstacks, nslices);
		// Atom Styles (Atom::ScaledStyle)
		for (int n = 0; n<elements().nElements(); ++n)
		{
			radius = prefs.atomStyleRadius(Atom::ScaledStyle) * elements().el[n].atomicRadius;
			scaledAtom_[n].primitive(lod).createSphere(radius, nstacks, nslices);
			radius *= prefs.selectionScale();
			selectedScaledAtom_[n].primitive(lod).createSphere(radius, nstacks, nslices);
		}
	}
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
// 	double m[16];
// 	transformationMatrix_.copyColumnMajor(m);
// 	glMultMatrixd(m);
}

// Project given model coordinates into world coordinates (and screen coordinates if requested)
Vec3<double> &RenderEngine::modelToWorld(Vec3<double> &modelr, Mat4<double> &viewMatrix, Vec4<double> *screenr, double screenradius)
{
	msg.enter("RenderEngine::modelToWorld");
	static Vec3<double> worldr;
	Vec4<double> pos, temp, tempscreen;
// 	if (!gui.mainView.isValid())
// 	{
// 		msg.exit("RenderEngine::modelToWorld");
// 		result.zero();
// 		return result;
// 	}
	// Projection formula is : worldr = P x M x modelr
	pos.set(modelr, 1.0);
	// We also need to subtract the cell centre coordinate
// 	pos -= cell_.centre();	BROKEN
	// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
	temp = viewMatrix * pos;
	worldr.set(temp.x, temp.y, temp.z);
	// Calculate 2D screen coordinates - Multiply world coordinates by P
	if (screenr != NULL)
	{
		*screenr = projectionMatrix_ * temp;
		screenr->x /= screenr->w;
		screenr->y /= screenr->w;
		screenr->x = viewportMatrix_[0] + viewportMatrix_[2]*(screenr->x+1)*0.5;
		screenr->y = viewportMatrix_[1] + viewportMatrix_[3]*(screenr->y+1)*0.5;
		screenr->z = screenr->z / screenr->w;
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		if (screenradius > 0.0)
		{
			temp.x += screenradius;
			tempscreen = projectionMatrix_ * temp;
			tempscreen.x /= tempscreen.w;
			screenr->w = fabs( (viewportMatrix_[0] + viewportMatrix_[2]*(tempscreen.x+1)*0.5) - screenr->x);
		}
	}
	msg.exit("RenderEngine::modelToWorld");
	return worldr;
}

// Project the specified world coordinates into 2D screen coords
Vec4<double> &RenderEngine::worldToScreen(const Vec3<double> &v, Mat4<double> &viewMatrix)
{
	// The returned vec4's 'w' component is the unit 'radius' at that point.
	msg.enter("RenderEngine::worldToScreen");
	static Vec4<double> modelr, screenr, worldr, result;
	static double x1,x2,radius;
	screenr.zero();
// 	if (!gui.mainView.isValid() )
// 	{
// 		msg.exit("RenderEngine::worldToScreen");
// 		return screenr;
// 	}
	// Projection formula is : worldr = P x M x modelr
	// Get the 3D coordinates of the atom - Multiply by modelview matrix 'view'
	modelr.set(v.x, v.y, v.z, 1.0);
	worldr = viewMatrix * modelr;
	//viewMatrix_.print();
	// Calculate 2D 'radius' of the atom - Multiply worldr[x+delta] coordinates by P
	screenr = projectionMatrix_ * worldr;
	screenr.x /= screenr.w;
	screenr.y /= screenr.w;
	result = screenr;
	x1 = viewportMatrix_[0] + viewportMatrix_[2]*(screenr.x+1)/2.0;
	worldr.x += 1.0;
	screenr = projectionMatrix_ * worldr;
	screenr.x /= screenr.w;
	x2 = viewportMatrix_[0] + viewportMatrix_[2]*(screenr.x+1)/2.0;
	radius = fabs(x2 - x1);
	// Store info and return
	result.w = radius;
	msg.exit("RenderEngine::worldToScreen");
	return result;
}

/*
// Object Rendering
*/

// Render primitive at requested local position in specified colour, returning projected position
Vec3<double> &RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, Vec3<double> pos, GLfloat *ambient, GLfloat *diffuse)
{
	double alphadelta = 1.0-ambient[4];
	// Filter type determines what to do here...
	if ((type_ == NoFilter) || ((type_ == TransparencyFilter) && (alphadelta < 0.001)))
	{
		// Pass through direct to GL
		glTranslated(pos.x, pos.y, pos.z);
		glMaterialfv(GL_FRONT, GL_AMBIENT, ambient);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, ambient);
		pg.sendToGL(lod);
		glTranslated(-pos.x, -pos.y, -pos.z);
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
	Vec3<double> pos;
	int lod;

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
		// Calculate world position and level of detail
		pos = transformationMatrix_ * i->r();
		lod = -pos.z / prefs.levelOfDetailWidth();
// 		printf("lod = %i, clipnear = %f, pos.z = %f\n", lod, prefs.clipNear(), pos.z);
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
		if (style == Atom::IndividualStyle) style = i->style();
		switch (style)
		{
			case (Atom::StickStyle):
				renderPrimitive(atom_[style], lod, pos, ambient, diffuse);
				break;
			case (Atom::TubeStyle):
			case (Atom::SphereStyle):
				renderPrimitive(atom_[style], lod, pos, ambient, diffuse);
				if (i->isSelected())
				{
					ambient[3] = 0.5;
					renderPrimitive(selectedAtom_[style], lod, pos, ambient, diffuse);
				}
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(scaledAtom_[i->element()], lod, pos, ambient, diffuse);
				if (i->isSelected())
				{
					ambient[3] = 0.5;
					renderPrimitive(selectedScaledAtom_[i->element()], lod, pos, ambient, diffuse);
				}
				break;
		}
	}
}
