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
	// Primitives
	scaledAtom_ = new PrimitiveGroup[elements().nElements()];
	selectedScaledAtom_ = new PrimitiveGroup[elements().nElements()];

	createPrimitives();
	triangleChopper_.initialise(0.0, 20.0, 0.1);
}

// Destructor
RenderEngine::~RenderEngine()
{
	delete[] scaledAtom_;
	delete[] selectedScaledAtom_;
}

/*
// Primitive Generation
*/

// (Re)Generate primitives
void RenderEngine::createPrimitives()
{
	msg.enter("RenderEngine::createPrimitives");
	double radius;
	int n, lod, nstacks, nslices, quality = prefs.primitiveQuality();
	// Clear old primitive groups
	for (n=0; n<Atom::nDrawStyles; ++n)
	{
		atom_[n].clear();
		selectedAtom_[n].clear();	
		bond_[n].clear();
		selectedBond_[n].clear();
		for (int m=0; m<elements().nElements(); ++m)
		{
			scaledAtom_[m].clear();
			selectedScaledAtom_[m].clear();
		}
	}
	// Loop over levels of detail
	for (lod=0; lod < prefs.levelsOfDetail(); ++lod)
	{
		// Atom Styles (Atom::StickStyle, Atom::TubeStyle, and Atom::SphereStyle)
		atom_[Atom::StickStyle].primitive(lod).createCross(0.5,3-lod);
		nstacks = max(3,(int) (quality*(1.0-lod*0.2)*0.75));
		nslices = max(3,(int) (quality*(1.0-lod*0.2)*1.5));
		atom_[Atom::TubeStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::TubeStyle), nstacks, nslices);
		atom_[Atom::SphereStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::SphereStyle), nstacks, nslices);
		selectedAtom_[Atom::TubeStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::TubeStyle)*prefs.selectionScale(), nstacks, nslices);
		selectedAtom_[Atom::SphereStyle].primitive(lod).createSphere(prefs.atomStyleRadius(Atom::SphereStyle)*prefs.selectionScale(), nstacks, nslices);
		// Atom Styles (Atom::ScaledStyle)
		for (n = 0; n<elements().nElements(); ++n)
		{
			radius = prefs.atomStyleRadius(Atom::ScaledStyle) * elements().el[n].atomicRadius;
			scaledAtom_[n].primitive(lod).createSphere(radius, nstacks, nslices);
			radius *= prefs.selectionScale();
			selectedScaledAtom_[n].primitive(lod).createSphere(radius, nstacks, nslices);
		}
		// Bond Styles (all)
		nstacks = max(1,(int) (quality*(1.0-lod*0.2)*0.25));
		nslices = max(3,(int) (quality*(1.0-lod*0.2)));
		bond_[Atom::TubeStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::TubeStyle), prefs.bondStyleRadius(Atom::TubeStyle), 1.0, nstacks, nslices);
		bond_[Atom::SphereStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::SphereStyle), prefs.bondStyleRadius(Atom::SphereStyle), 1.0, nstacks, nslices);
		bond_[Atom::ScaledStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::ScaledStyle), prefs.bondStyleRadius(Atom::ScaledStyle), 1.0, nstacks, nslices);
		selectedBond_[Atom::TubeStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::TubeStyle)*prefs.selectionScale(), prefs.bondStyleRadius(Atom::TubeStyle)*prefs.selectionScale(), 1.0, nstacks, nslices);
		selectedBond_[Atom::SphereStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::SphereStyle)*prefs.selectionScale(), prefs.bondStyleRadius(Atom::SphereStyle)*prefs.selectionScale(), 1.0, nstacks, nslices);
		selectedBond_[Atom::ScaledStyle].primitive(lod).createCylinder(prefs.bondStyleRadius(Atom::ScaledStyle)*prefs.selectionScale(), prefs.bondStyleRadius(Atom::ScaledStyle)*prefs.selectionScale(), 1.0, nstacks, nslices);
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

// Render primitive in specified colour and level of detail (coords/transform used only if filtered)
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *ambient, GLfloat *diffuse, GLMatrix &transform, bool transformInGL)
{
	if (diffuse[3] > 0.99f)
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_.add();
		pi->set(&pg.primitive(lod), ambient, diffuse, transform);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_.add();
		pi->set(&pg.primitive(lod), ambient, diffuse, transform);
	}
}

// Render specified model
void RenderEngine::renderModel(Model *source)
{
	GLfloat ambienti[4], diffusei[4], ambientj[4], diffusej[4], scaledradius;
	int lod, id_i;
	double z, phi, halfr;
	Atom *i, *j;
	Vec3<double> pos, v;
	Vec4<double> transformZ;
	GLMatrix transformbase, atomtransform, bondtransform;
	Refitem<Bond,int> *ri;

	// Clear filtered primitives list
	solidPrimitives_.clear();
	transparentPrimitives_.clear();

	// Set transformation matrix
	setTransformationMatrix(source->viewMatrix());
	transformZ = transformationMatrix_.z();
	transformbase = transformationMatrix_;

	// Set polygon fill mode and specular reflection
	prefs.copyColour(Prefs::SpecularColour, ambienti);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, ambienti);
	glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, prefs.shininess());

	// Grab style values....
	Prefs::ColouringScheme scheme = prefs.colourScheme();
	Atom::DrawStyle stylei, stylej, globalstyle = prefs.renderStyle();
	scaledradius = prefs.atomStyleRadius(Atom::ScaledStyle);

	// Atoms and Bonds // OPTIMIZE - use atom array instead
	for (i = source->atoms(); i != NULL; i = i->next)
	{
		// Skip hidden atoms
		if (i->isHidden()) continue;

		// Grab atom coordinate - we'll need it a lot
		pos = i->r();
		// Calculate projected Z distance from viewer
		// If z is less than 0, don't even bother continuing since its behind the viewer
		z = -(pos.x*transformZ.x + pos.y*transformZ.y + pos.z*transformZ.z + transformZ.w);
		if (z < 0) continue;

		// Determine level of detail to use for primitives
		if (z < prefs.levelOfDetailStartZ()) lod = 0;
		else lod = int(z / prefs.levelOfDetailWidth());
// 		printf("lod = %i, clipnear = %f, pos.z = %f\n", lod, prefs.clipNear(), z);

		// Move to local atom position
		atomtransform = transformbase;
		atomtransform.applyTranslation(pos.x, pos.y, pos.z);
// 		glTranslated(pos.x, pos.y, pos.z);

		// Select colour
		if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, ambienti);
		else switch (scheme)
		{
			case (Prefs::ElementScheme):
				elements().copyAmbientColour(i->element(), ambienti);
				break;
			case (Prefs::ChargeScheme):
				prefs.colourScale[0].colour(i->charge(), ambienti);
				break;
			case (Prefs::VelocityScheme):
				prefs.colourScale[1].colour(i->v().magnitude(), ambienti);
				break;
			case (Prefs::ForceScheme):
				prefs.colourScale[2].colour(i->f().magnitude(), ambienti);
				break;
			case (Prefs::CustomScheme):
				i->copyColour(ambienti);
				break;
			default:
				break;
		}
		// Set diffuse colour as 0.75*ambient
		diffusei[0] = ambienti[0] * 0.75f;
		diffusei[1] = ambienti[1] * 0.75f;
		diffusei[2] = ambienti[2] * 0.75f;
		diffusei[3] = ambienti[3];

		// Get atom style
		stylei = (globalstyle == Atom::IndividualStyle ? i->style() : globalstyle);
		
		switch (stylei)
		{
			case (Atom::StickStyle):
				if (i->nBonds() == 0) renderPrimitive(atom_[stylei], lod, ambienti, diffusei, atomtransform, FALSE);
				break;
			case (Atom::TubeStyle):
			case (Atom::SphereStyle):
				renderPrimitive(atom_[stylei], lod, ambienti, diffusei, atomtransform, FALSE);
				if (i->isSelected())
				{
					diffusei[3] = 0.5f;
					renderPrimitive(selectedAtom_[stylei], lod, ambienti, diffusei, atomtransform, FALSE);
					diffusei[3] = ambienti[3];
				}
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(scaledAtom_[i->element()], lod, ambienti, diffusei, atomtransform, FALSE);
				if (i->isSelected())
				{
					diffusei[3] = 0.5f;
					renderPrimitive(selectedScaledAtom_[i->element()], lod, ambienti, diffusei, atomtransform, FALSE);
					diffusei[3] = ambienti[3];
				}
				break;
		}

		// LABELS ETC SHOULD BE DONE HERE....

		// Move back to 'zero' position
// 		glTranslated(-pos.x, -pos.y, -pos.z);

		// Bonds
		// Translate local matrix to atom centre
		bondtransform = atomtransform;
// 		.createTranslation(pos.x, pos.y, pos.z);
		id_i = i->id();
		for (ri = i->bonds(); ri != NULL; ri = ri->next)
		{
			j = ri->item->partner(i);
			if (j->id() > id_i) continue;
			v = j->r() - i->r();

			// Grab colour of second atom
			if (j->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, ambientj);
			else switch (scheme)
			{
				case (Prefs::ElementScheme):
					elements().copyAmbientColour(j->element(), ambientj);
					break;
				case (Prefs::ChargeScheme):
					prefs.colourScale[0].colour(j->charge(), ambientj);
					break;
				case (Prefs::VelocityScheme):
					prefs.colourScale[1].colour(j->v().magnitude(), ambientj);
					break;
				case (Prefs::ForceScheme):
					prefs.colourScale[2].colour(j->f().magnitude(), ambientj);
					break;
				case (Prefs::CustomScheme):
					j->copyColour(ambientj);
					break;
				default:
					break;
			}
			// Get atom style
			stylej = (globalstyle == Atom::IndividualStyle ? j->style() : globalstyle);

			// Set diffuse colour as 0.75*ambient
			diffusej[0] = ambientj[0] * 0.75f;
			diffusej[1] = ambientj[1] * 0.75f;
			diffusej[2] = ambientj[2] * 0.75f;
			diffusej[3] = ambientj[3];

			// Don't bother calculating transformation if both atom styles are Stick
			if ((stylei == Atom::StickStyle) && (stylej == Atom::StickStyle))
			{
				glNormal3d(0.0,0.0,1.0);
				glLineWidth( i->isSelected() ? 3.0 : 1.0 );
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, ambienti);
				glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, diffusei);
				glBegin(GL_LINES);
				glVertex3d(pos.x, pos.y, pos.z);
				glVertex3d(pos.x+0.5*v.x, pos.y+0.5*v.y, pos.z+0.5*v.z);
				glEnd();
				glLineWidth( j->isSelected() ? 3.0 : 1.0 );
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, ambientj);
				glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, diffusej);
				glBegin(GL_LINES);
				glVertex3d(pos.x+0.5*v.x, pos.y+0.5*v.y, pos.z+0.5*v.z);
				glVertex3d(pos.x+v.x, pos.y+v.y, pos.z+v.z);
				glEnd();
			}
			else
			{
				// OPTIMISE - Shift position so as not to draw 'inside' spherical atoms
				// Scale to half-bond length
				v *= 0.5;
				halfr = v.magnitude();
				// Calculate angle out of XZ plane
				// OPTIMISE - Precalculate acos()
				phi = DEGRAD * acos(v.z/halfr);
	
				// Special case where the bond is exactly in the XY plane.
// 				transform1 = transformbase;
				bondtransform = atomtransform;
				if ((fabs(phi) < 0.01) || (phi > 179.99)) bondtransform.applyRotationX(phi);
				else bondtransform.applyRotationAxis(-v.y, v.x, 0.0, phi, TRUE);
	
				// Scale to half length of bond
				bondtransform.applyScalingZ(halfr);
	
				// Draw bond halves
				switch (stylei)
				{
					case (Atom::StickStyle):
						glLineWidth( i->isSelected() ? 4.0 : 2.0 );
						glBegin(GL_LINES);
						glVertex3d(pos.x, pos.y, pos.z);
						glVertex3d(pos.x+v.x, pos.y+v.y, pos.z+v.z);
						glEnd();
						break;
					case (Atom::TubeStyle):
					case (Atom::SphereStyle):
					case (Atom::ScaledStyle):
						renderPrimitive(bond_[stylei], lod, ambienti, diffusei, bondtransform);
						if (i->isSelected())
						{
							diffusei[3] = 0.5f;
							renderPrimitive(selectedBond_[stylei], lod, ambienti, diffusei, bondtransform);
							diffusei[3] = ambienti[3];
						}
						break;
				}
				switch (stylej)
				{
					case (Atom::StickStyle):
						glLineWidth( j->isSelected() ? 3.0 : 1.0 );
						glBegin(GL_LINES);
						glVertex3d(pos.x+v.x, pos.y+v.y, pos.z+v.z);
						glVertex3d(pos.x+2*v.x, pos.y+2*v.y, pos.z+2*v.z);
						glEnd();
						break;
					case (Atom::TubeStyle):
					case (Atom::SphereStyle):
					case (Atom::ScaledStyle):
						bondtransform.applyTranslation(0.0, 0.0, halfr*2.0);
						renderPrimitive(bond_[stylej], lod, ambientj, diffusej, bondtransform);
						if (j->isSelected())
						{
							diffusej[3] = 0.5f;
							renderPrimitive(selectedBond_[stylej], lod, ambientj, diffusej, bondtransform);
							diffusej[3] = ambientj[3];
						}
						break;
				}
			}
		}

	}

	// All objects have now been filtered...
	sortAndSendGL();
}

// Sort and render filtered polygons by depth
void RenderEngine::sortAndSendGL()
{
	// Transform and render each solid primitive in the list
	Vec3<double> pos;
	for (PrimitiveInfo *pi = solidPrimitives_.first(); pi != NULL; pi = pi->next)
	{
		glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, pi->ambient());
		glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, pi->diffuse());
		glPushMatrix();
		glMultMatrixd(pi->localTransform().matrix());
		pi->primitive()->sendToGL();
		glPopMatrix();
	}
	
	// Transform and render each solid primitive in the list
	triangleChopper_.emptyTriangles();
	for (PrimitiveInfo *pi = transparentPrimitives_.first(); pi != NULL; pi = pi->next)
	{
		glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, pi->ambient());
		glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, pi->diffuse());
		triangleChopper_.storeTriangles(pi);
		continue;
		
		
		
		glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, pi->ambient());
		glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, pi->diffuse());
		glPushMatrix();
		glMultMatrixd(pi->localTransform().matrix());
		pi->primitive()->sendToGL();
		glPopMatrix();
	}
	triangleChopper_.sendToGL();
}
