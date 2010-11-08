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
	// Loop over levels of detail
	for (lod=0; lod < prefs.levelsOfDetail(); ++lod)
	{
		// Atom Styles (Atom::StickStyle, Atom::TubeStyle, and Atom::SphereStyle)
		atom_[Atom::StickStyle].primitive(lod).createCross(0.5,3-lod);
		nstacks = max(3,(int) (prefs.atomDetail()*(1.0-lod*0.2)*0.75));
		nslices = max(3,(int) (prefs.atomDetail()*(1.0-lod*0.2)*1.5));
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
		// Bond Styles (all)
		bond_[Atom::StickStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::TubeStyle), prefs.bondStyleRadius(Atom::TubeStyle), 1.0, nstacks, nslices);
		bond_[Atom::TubeStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::TubeStyle), prefs.bondStyleRadius(Atom::TubeStyle), 1.0, nstacks, nslices);
		bond_[Atom::SphereStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::SphereStyle), prefs.bondStyleRadius(Atom::SphereStyle), 1.0, nstacks, nslices);
		bond_[Atom::ScaledStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::ScaledStyle), prefs.bondStyleRadius(Atom::ScaledStyle), 1.0, nstacks, nslices);
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
	double m[16];
	transformationMatrix_.copyColumnMajor(m);
	glMultMatrixd(m);
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
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *ambient, GLfloat *diffuse, Vec3<double> &pos)
{
	double alphadelta = 1.0-ambient[4];
	// Filter type determines what to do here...
	if ((type_ == NoFilter) || ((type_ == TransparencyFilter) && (alphadelta < 0.001)))
	{
		// Pass through direct to GL
		glMaterialfv(GL_FRONT, GL_AMBIENT, ambient);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, ambient);
		pg.sendToGL(lod);
	}
	else
	{
		// Add primitive info to local buffer (it will be rednered later
		
	}
}

// Render primitive in specified colour and level of detail (coords/transform used only if filtered)
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *ambient, GLfloat *diffuse, Vec3<double> &local, GLdouble *transform)
{
	double alphadelta = 1.0-ambient[4];
	// Filter type determines what to do here...
	if ((type_ == NoFilter) || ((type_ == TransparencyFilter) && (alphadelta < 0.001)))
	{
		// Pass through direct to GL
		glMaterialfv(GL_FRONT, GL_AMBIENT, ambient);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, ambient);
		pg.sendToGL(lod);
	}
	else
	{
		// Add primitive info to local buffer (it will be rednered later
		
	}
}

// Render specified model
void RenderEngine::renderModel(Model *source)
{
	GLfloat ambient[4], diffuse[4], scaledradius;
	int lod, id_i;
	double z, phi, rij;
	Atom *i, *j;
	Vec3<double> pos, v;
	Vec4<double> transformZ;
	GLdouble transform[16];
	Refitem<Bond,int> *ri;

	// Set transformation matrix
	setTransformationMatrix(source->viewMatrix());
	transformZ = transformationMatrix_.z();

	// Grab style values....
	Prefs::ColouringScheme scheme = prefs.colourScheme();
	Atom::DrawStyle style = prefs.renderStyle();
	scaledradius = prefs.atomStyleRadius(Atom::ScaledStyle);

	// Atoms and Bonds
	for (i = source->atoms(); i != NULL; i = i->next)
	{
		// Skip hidden atoms
		if (i->isHidden()) continue;

		// Grab atom coordinate - we'll need it a lot
		pos = i->r();
		// Calculate projected Z coordinate and level of detail
		z = pos.x*transformZ.x + pos.y*transformZ.y + pos.z*transformZ.z + transformZ.w;
		lod = int(-z / prefs.levelOfDetailWidth());
		printf("lod = %i, clipnear = %f, pos.z = %f\n", lod, prefs.clipNear(), z);

		// Move to local atom position
		glTranslated(pos.x, pos.y, pos.z);

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
				renderPrimitive(atom_[style], lod, ambient, diffuse, pos);
				break;
			case (Atom::TubeStyle):
			case (Atom::SphereStyle):
				renderPrimitive(atom_[style], lod, ambient, diffuse, pos);
				if (i->isSelected())
				{
					ambient[3] = 0.5;
					renderPrimitive(selectedAtom_[style], lod, ambient, diffuse, pos);
				}
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(scaledAtom_[i->element()], lod, ambient, diffuse, pos);
				if (i->isSelected())
				{
					ambient[3] = 0.5;
					renderPrimitive(selectedScaledAtom_[i->element()], lod, ambient, diffuse, pos);
				}
				break;
		}
		// Bonds
		id_i = i->id();
		for (ri = i->bonds(); ri != NULL; ri = ri->next)
		{
			j = ri->item->partner(i);
			if (j->id() > id_i) continue;
			v = j->r() - i->r();
			rij = v.magnitude();
			// Calculate angle out of XZ plane
			phi = DEGRAD * acos(v.z/rij);
			// Special case where the bond is exactly in the XY plane.
			glPushMatrix();
			if ((180.0 - phi) < 0.0001) glRotated(phi,1.0,0.0,0.0);
			else glRotated(phi, -v.y/rij , v.x/rij, 0.0);
		glGetDoublev(GL_MODELVIEW_MATRIX, m);
		printf(" %f  %f  %f  %f\n", m[0], m[1], m[2], m[3]);
		printf(" %f  %f  %f  %f\n", m[4], m[5], m[6], m[7]);
		printf(" %f  %f  %f  %f\n", m[8], m[9], m[10], m[11]);
		printf(" %f  %f  %f  %f\n", m[12], m[13], m[14], m[15]);
			glScaled(1.0,1.0,rij);
		glGetDoublev(GL_MODELVIEW_MATRIX, m);
		printf(" %f  %f  %f  %f\n", m[0], m[1], m[2], m[3]);
		printf(" %f  %f  %f  %f\n", m[4], m[5], m[6], m[7]);
		printf(" %f  %f  %f  %f\n", m[8], m[9], m[10], m[11]);
		printf(" %f  %f  %f  %f\n", m[12], m[13], m[14], m[15]);

			glPopMatrix();

			glPushMatrix();
			if ((180.0 - phi) < 0.0001) transform.createRotationX(phi);
			else transform.createRotationAxis(-v.y/rij , v.x/rij, 0.0, phi);
			transform.print();
// 			glScaled(1.0,1.0,rij);
// 			glCallList(glob(TubeArrowGlob));
			glGetDoublev(GL_MODELVIEW_MATRIX, m);
		printf(" %f  %f  %f  %f\n", m[0], m[1], m[2], m[3]);
		printf(" %f  %f  %f  %f\n", m[4], m[5], m[6], m[7]);
		printf(" %f  %f  %f  %f\n", m[8], m[9], m[10], m[11]);
		printf(" %f  %f  %f  %f\n", m[12], m[13], m[14], m[15]);
	transform.copyColumnMajor(m);
	glMultMatrixd(m);
			glGetDoublev(GL_MODELVIEW_MATRIX, m);
		printf(" %f  %f  %f  %f\n", m[0], m[1], m[2], m[3]);
		printf(" %f  %f  %f  %f\n", m[4], m[5], m[6], m[7]);
		printf(" %f  %f  %f  %f\n", m[8], m[9], m[10], m[11]);
		printf(" %f  %f  %f  %f\n", m[12], m[13], m[14], m[15]);
			renderPrimitive(bond_[Atom::SphereStyle], lod, ambient, diffuse, pos, transform);
			glPopMatrix();
		}

		// Move back to 'zero' position
		glTranslated(-pos.x, -pos.y, -pos.z);
	}
}
