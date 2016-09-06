/*
	*** Model Primitive Generation
	*** src/gui/viewer_model.cpp
	Copyright T. Youngs 2007-2016

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

#include "render/rendergroup.h"
#include "render/primitiveset.h"
#include "base/forcefieldatom.h"
#include "model/model.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

// Render bond
void RenderGroup::createSelectedBond(PrimitiveSet& primitiveSet, Matrix A, Vec3<double> vij, Atom* i, Prefs::DrawStyle style_i, Vec4<GLfloat>& colour_i, double radius_i, Atom* j, Prefs::DrawStyle style_j, Vec4<GLfloat>& colour_j, double radius_j, Bond::BondType bt, double selscale, Bond* bondInPlane)
{
	double dvisible, selvisible, factor, rij, phi;
	Vec3<double> ri, rj, localx, localy, localz, stickpos, dx, normz;
	GLfloat alpha_i, alpha_j;
	Matrix B;

	// Store copies of alpha values
	alpha_i = colour_i[3];
	alpha_j = colour_j[3];
	
	localz = vij;
	rij = localz.magAndNormalise();
	
	// If bond is not visible, don't bother drawing it...
	dvisible = 0.5 * (rij - radius_i - radius_j);
	if (dvisible < 0.0) return;
	selvisible = 0.5 * (rij - selscale*radius_i - selscale*radius_j);

	// Determine bond plane rotation if a multiple bond
	if (((bt == Bond::Double) || (bt == Bond::Triple)) && (bondInPlane != NULL))
	{
		// Desired z-axis is already known ( = vijn) so get normalised copy
		normz = localz / rij;
		
		// X axis is bond plane vector...
		localx = i->findBondPlane(j, bondInPlane,normz,true);

		// Generate Y-axis from cross-product of z and x axes
		localy = localx * normz;
		localy.normalise();
		A[0] = localx.x;
		A[1] = localx.y;
		A[2] = localx.z;
		A[4] = localy.x;
		A[5] = localy.y;
		A[6] = localy.z;
		A[8] = localz.x;
		A[9] = localz.y;
		A[10] = localz.z;
	}
	else
	{
		// Calculate dot product with vector {0,0,1}
		phi = DEGRAD * acos(localz.z);
		
		// Special case where the bond is exactly along Z already
		if (phi > 179.99) A.applyRotationX(phi);
		else if (phi >= 0.01) A.applyRotationAxis(-vij.y, vij.x, 0.0, phi, true);
	}
	
	// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
	B = A;
	A.applyTranslation(0.0, 0.0, radius_i);
	A.applyScalingZ(dvisible);
	// Move matrix B to be centred on atom j
	B.addTranslation(j->r() - i->r());
	B.applyTranslation(0.0, 0.0, -radius_j);
	B.applyScalingZ(-dvisible);

	// Draw first bond half
	switch (style_i)
	{
		case (Prefs::LineStyle):
			// First vertex is at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = A * Vec3<double>(0.0,0.0,1.0);
			// Determine how many sticks to draw (bond multiplicity : aromatic still counts as one bond)
			switch (bt)
			{
				case (Bond::Double):
					dx = A.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle)*0.5,0.0,0.0);
					extraBoldLines_.defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_i);
					break;
				case (Bond::Triple):
					dx = A.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle),0.0,0.0);
					extraBoldLines_.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_i);
					break;
				default:
					extraBoldLines_.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i);
					extraBoldLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i);
					break;
			}
			break;
		case (Prefs::TubeStyle):
			if (selvisible > 0.0) addTriangles(primitiveSet.selectedBond(style_i, bt), A, colour_i, GL_LINE);
			break;
		case (Prefs::SphereStyle):
		case (Prefs::ScaledStyle):
			if (selvisible > 0.0)
			{
				// Move to edge of selected atom and apply selection bond scaling
				A.applyTranslation(0.0, 0.0, (selscale*radius_i-radius_i) / dvisible);
				A.applyScalingZ(selvisible/dvisible);
				addTriangles(primitiveSet.selectedBond(style_i, bt), A, colour_i, GL_LINE);
			}
			break;
    default:
      break;  
	}
	
	// Draw second bond half
	switch (style_j)
	{
		case (Prefs::LineStyle):
			// First vertex is *still* at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = B * Vec3<double>(0.0,0.0,1.0);
			switch (bt)
			{
				case (Bond::Double):
					dx = B.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle)*0.5,0.0,0.0);
					extraBoldLines_.defineVertex(B[12]+dx.x, B[13]+dx.y, B[14]+dx.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(B[12]-dx.x, B[13]-dx.y, B[14]-dx.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_j);
					break;
				case (Bond::Triple):
					dx = B.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle),0.0,0.0);
					extraBoldLines_.defineVertex(B[12], B[13], B[14], 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(B[12]+dx.x, B[13]+dx.y, B[14]+dx.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(B[12]-dx.x, B[13]-dx.y, B[14]-dx.z, 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_j);
					break;
				default:
					extraBoldLines_.defineVertex(B[12], B[13], B[14], 0.0,0.0,1.0, colour_j);
					extraBoldLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j);
					break;
			}
			break;
		case (Prefs::TubeStyle):
			B.applyTranslation(0.0, 0.0, (selscale*radius_j-radius_j) / dvisible);
			addTriangles(primitiveSet.selectedBond(style_j, bt), B, colour_j, GL_LINE);
			break;
		case (Prefs::SphereStyle):
		case (Prefs::ScaledStyle):
			if (selvisible > 0.0)
			{
				B.applyTranslation(0.0, 0.0, (selscale*radius_j-radius_j) / dvisible);
				B.applyScalingZ(selvisible / dvisible);
				addTriangles(primitiveSet.selectedBond(style_j, bt), B, colour_j, GL_LINE);
			}
			break;
    default:
      break;  
	}
}

// Render bond
void RenderGroup::createBond(PrimitiveSet& primitiveSet, Matrix A, Vec3<double> vij, Atom* i, Prefs::DrawStyle style_i, Vec4<GLfloat>& colour_i, double radius_i, Atom* j, Prefs::DrawStyle style_j, Vec4<GLfloat>& colour_j, double radius_j, Bond::BondType bt, double selscale, Bond* bondInPlane)
{
	double dvisible, selvisible, factor, rij, phi;
	Vec3<double> ri, rj, localx, localy, localz, stickpos, dx, normz;
	Primitive& iLinePrimitives = (i->isSelected() ? extraBoldLines_ : extraNormalLines_);
	Primitive& jLinePrimitives = (j->isSelected() ? extraBoldLines_ : extraNormalLines_);
	Matrix B;

	localz = vij;
	rij = localz.magAndNormalise();
	
	// If bond is not visible, don't bother drawing it...
	dvisible = 0.5 * (rij - radius_i - radius_j);
	if (dvisible < 0.0) return;
	selvisible = 0.5 * (rij - selscale*radius_i - selscale*radius_j);

	// Determine bond plane rotation if a multiple bond
	if (((bt == Bond::Double) || (bt == Bond::Triple)) && (bondInPlane != NULL))
	{
		// Desired z-axis is already known ( = vijn) so get normalised copy
		normz = localz / rij;
		
		// X axis is bond plane vector...
		localx = i->findBondPlane(j, bondInPlane,normz,true);

		// Generate Y-axis from cross-product of z and x axes
		localy = localx * normz;
		localy.normalise();
		A[0] = localx.x;
		A[1] = localx.y;
		A[2] = localx.z;
		A[4] = localy.x;
		A[5] = localy.y;
		A[6] = localy.z;
		A[8] = localz.x;
		A[9] = localz.y;
		A[10] = localz.z;
	}
	else
	{
		// Calculate dot product with vector {0,0,1}
		phi = DEGRAD * acos(localz.z);
		
		// Special case where the bond is exactly along Z already
		if (phi > 179.99) A.applyRotationX(phi);
		else if (phi >= 0.01) A.applyRotationAxis(-vij.y, vij.x, 0.0, phi, true);
	}
	
	// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
	B = A;
	A.applyTranslation(0.0, 0.0, radius_i);
	A.applyScalingZ(dvisible);
	// Move matrix B to be centred on atom j
	B.addTranslation(j->r() - i->r());
	B.applyTranslation(0.0, 0.0, -radius_j);
	B.applyScalingZ(-dvisible);

	// Draw first bond half
	switch (style_i)
	{
		case (Prefs::LineStyle):
			// First vertex is at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = A * Vec3<double>(0.0,0.0,1.0);
			// Determine how many sticks to draw (bond multiplicity : aromatic still counts as one bond)
			switch (bt)
			{
				case (Bond::Double):
					dx = A.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle)*0.5,0.0,0.0);
					iLinePrimitives.defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_i);
					break;
				case (Bond::Triple):
					dx = A.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle),0.0,0.0);
					iLinePrimitives.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_i);
					break;
				default:
					iLinePrimitives.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i);
					iLinePrimitives.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i);
					break;
			}
			break;
		case (Prefs::TubeStyle):
			addTriangles(primitiveSet.bond(style_i, bt), A, colour_i);
			if (i->isSelected() && (selvisible > 0.0)) addLines(primitiveSet.selectedBond(style_i, bt), A, penColour_);
			break;
		case (Prefs::SphereStyle):
		case (Prefs::ScaledStyle):
			addTriangles(primitiveSet.bond(style_i, bt), A, colour_i);
			if (i->isSelected() && (selvisible > 0.0))
			{
				// Move to edge of selected atom and apply selection bond scaling
				A.applyTranslation(0.0, 0.0, (selscale*radius_i-radius_i) / dvisible);
				A.applyScalingZ(selvisible/dvisible);
				addLines(primitiveSet.selectedBond(style_i, bt), A, penColour_);
			}
			break;
    default:
      break;  
	}
	
	// Draw second bond half
	switch (style_j)
	{
		case (Prefs::LineStyle):
			// First vertex is *still* at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = B * Vec3<double>(0.0,0.0,1.0);
			switch (bt)
			{
				case (Bond::Double):
					dx = B.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle)*0.5,0.0,0.0);
					jLinePrimitives.defineVertex(B[12]+dx.x, B[13]+dx.y, B[14]+dx.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(B[12]-dx.x, B[13]-dx.y, B[14]-dx.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_j);
					break;
				case (Bond::Triple):
					dx = B.rotateVector(prefs.atomStyleRadius(Prefs::LineStyle),0.0,0.0);
					jLinePrimitives.defineVertex(B[12], B[13], B[14], 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(B[12]+dx.x, B[13]+dx.y, B[14]+dx.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(B[12]-dx.x, B[13]-dx.y, B[14]-dx.z, 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_j);
					break;
				default:
					jLinePrimitives.defineVertex(B[12], B[13], B[14], 0.0,0.0,1.0, colour_j);
					jLinePrimitives.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j);
					break;
			}
			break;
		case (Prefs::TubeStyle):
			addTriangles(primitiveSet.bond(style_j, bt), B, colour_j);
			if (j->isSelected())
			{
				B.applyTranslation(0.0, 0.0, (selscale*radius_j-radius_j) / dvisible);
				addLines(primitiveSet.selectedBond(style_j, bt), B, penColour_);
			}
			break;
		case (Prefs::SphereStyle):
		case (Prefs::ScaledStyle):
			addTriangles(primitiveSet.bond(style_j, bt), B, colour_j);
			if (j->isSelected() && (selvisible > 0.0))
			{
				B.applyTranslation(0.0, 0.0, (selscale*radius_j-radius_j) / dvisible);
				B.applyScalingZ(selvisible / dvisible);
				addLines(primitiveSet.selectedBond(style_j, bt), B, penColour_);
			}
			break;
    default:
      break;  
	}
}

// Render basic model information (atoms, bonds, labels, and glyphs)
void RenderGroup::createAtomsAndBonds(PrimitiveSet& primitiveSet, Model* source, Matrix baseTransform)
{
	Messenger::enter("RenderGroup::renderAtomsAndBonds");
	Vec4<GLfloat> colour_i, colour_j;
	GLfloat alpha_i;
	int id_i, labels, m, n, el_j;
	double selscale, radius_i, radius_j, phi, mag, best, delta;
	double aradius[Prefs::nDrawStyles];
	Atom *i, *j, *k, *l, **atoms;
	Vec3<double> pos, v, ijk, r1, r2, r3, r4;
	Vec4<double> screenr;
	Matrix atomTransform, A, B;
	RefListItem<Bond,int>* rb;
	RefListItem<Atom,int>* ra;
	Prefs::DrawStyle style_i, style_j, drawStyle;
	Prefs::ColouringScheme scheme;

	// Grab global style values and atom radii
	scheme = source->colourScheme();
	drawStyle = source->drawStyle();
	selscale = prefs.selectionScale();
	for (n=0; n<Prefs::nDrawStyles; ++n) aradius[n] = prefs.atomStyleRadius( (Prefs::DrawStyle) n);
	prefs.copyColour(prefs.currentForegroundColour(), penColour_);

	// Atoms and Bonds
	atoms = source->atomArray();
	for (n = 0; n<source->nAtoms(); ++n)
	{
		// Get atom pointer
		i = atoms[n];
		
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Grab atom coordinate - we'll need it a lot
		pos = i->r();

		// Move to local atom position
		atomTransform = baseTransform;
		atomTransform.applyTranslation(pos.x, pos.y, pos.z);
		
		// Select colour
		if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_i);
		else switch (scheme)
		{
			case (Prefs::ElementScheme):
				ElementMap::copyColour(i->element(), colour_i);
				break;
			case (Prefs::ChargeScheme):
				prefs.colourScale[0].colour(i->charge(), colour_i);
				break;
			case (Prefs::VelocityScheme):
				prefs.colourScale[1].colour(i->v().magnitude(), colour_i);
				break;
			case (Prefs::ForceScheme):
				prefs.colourScale[2].colour(i->f().magnitude(), colour_i);
				break;
			case (Prefs::BondsScheme):
				prefs.colourScale[3].colour(i->nBonds(), colour_i);
				break;
			case (Prefs::OwnScheme):
				i->copyColour(colour_i);
				break;
			default:
				break;
		}

		// Store copy of alpha value, since we might need to overwrite it later
		alpha_i = colour_i[3];
		
		// Get atom style and render associated object
		style_i = (drawStyle == Prefs::OwnStyle ? i->style() : drawStyle);
		
		if (style_i == Prefs::LineStyle)
		{
			// Only need to draw something if the atom has no bonds
			if (i->nBonds() == 0)
			{
				if (i->isSelected()) extraBoldLines_.plotCross(0.5, atomTransform, colour_i);
				else extraNormalLines_.plotCross(0.5, atomTransform, colour_i);
			}
		}
		else
		{
			radius_i = aradius[style_i];
			if (style_i == Prefs::ScaledStyle) radius_i *= ElementMap::atomicRadius(i->element());
			A = atomTransform;
			A.applyScaling(radius_i, radius_i, radius_i);
			addTriangles(primitiveSet.atom(), A, colour_i);
			if (i->isSelected()) addLines(primitiveSet.selectedAtom(), A, penColour_);
		}

		// Bonds
		// Grab some useful values from atom i
		id_i = i->id();
		if (style_i <= Prefs::TubeStyle) radius_i = 0.0;
		else if (style_i == Prefs::ScaledStyle) radius_i = source->styleRadius(Prefs::ScaledStyle, i->element()) - primitiveSet.scaledAtomAdjustment(i->element());
		else radius_i = source->styleRadius(style_i, i->element()) - primitiveSet.sphereAtomAdjustment();
		
		for (rb = i->bonds(); rb != NULL; rb = rb->next)
		{
			j = rb->item->partner(i);
			if (id_i > j->id()) continue;
			if (j->isHidden()) continue;
			
			// Grab colour of second atom
			if (j->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_j);
			else switch (scheme)
			{
				case (Prefs::ElementScheme):
					ElementMap::copyColour(j->element(), colour_j);
					break;
				case (Prefs::ChargeScheme):
					prefs.colourScale[0].colour(j->charge(), colour_j);
					break;
				case (Prefs::VelocityScheme):
					prefs.colourScale[1].colour(j->v().magnitude(), colour_j);
					break;
				case (Prefs::ForceScheme):
					prefs.colourScale[2].colour(j->f().magnitude(), colour_j);
					break;
				case (Prefs::BondsScheme):
					prefs.colourScale[3].colour(j->nBonds(), colour_j);
					break;
				case (Prefs::OwnScheme):
					j->copyColour(colour_j);
					break;
				default:
					break;
			}
			
			// Get atom style and radius
			style_j = (drawStyle == Prefs::OwnStyle ? j->style() : drawStyle);
			if (style_j <= Prefs::TubeStyle) radius_j = 0.0;
			else if (style_j == Prefs::ScaledStyle) radius_j = source->styleRadius(Prefs::ScaledStyle, j->element()) - primitiveSet.scaledAtomAdjustment(j->element());
			else radius_j = source->styleRadius(style_j, j->element()) - primitiveSet.sphereAtomAdjustment();
			
			// Calculate vector i->j
			v = source->cell().mimVector(i, j);
			
			// Render bond
			createBond(primitiveSet, atomTransform, v, i, style_i, colour_i, radius_i, j, style_j, colour_j, radius_j, rb->item->type(), selscale, rb->item);
		}
		
	}
	
	// Aromatic rings (needs valid pattern description)
	if (source->arePatternsValid())
	{
		prefs.copyColour(Prefs::AromaticRingColour, colour_i);

		atoms = source->atomArray();
		for (Pattern *p = source->patterns(); p != NULL; p = p->next)
		{
			// Continue if no rings present in pattern
			if (p->rings() == NULL) continue;

			// Cycle over rings
			for (Ring *r = p->rings(); r != NULL; r = r->next)
			{
				if (r->type() != Ring::AromaticRing) continue;
				
				// Loop over pattern molecules
				id_i = 0;
				for (m=0; m<p->nMolecules(); ++m)
				{
					// Determine ring centroid
					ra = r->atoms();
					i = atoms[id_i+ra->item->id()];
					r1 = i->r();
					pos = r1;
					for (ra = ra->next; ra != NULL; ra = ra->next)
					{
						j = atoms[id_i+ra->item->id()];
						pos += source->cell().mim(j->r(), r1);
					}
					pos /= r->nAtoms();

					// Determine average vector magnitude of ring atoms and y-axis vector (closest 90deg vector)
					mag = 0.0;
					r1 -= pos;
					r1.normalise();
					best = PI/2.0;
					r2.zero();
					radius_i = 0.0;
					
					for (ra = r->atoms(); ra != NULL; ra = ra->next)
					{
						// Grab atom pointer and get minimum image vector with centroid 'v'
						i = atoms[id_i+ra->item->id()];
						if (source->styleRadius(i->style(), i->element()) > radius_i) radius_i = source->styleRadius(i->style(), i->element());
						v = source->cell().mimVector(pos, i->r());
						// Accumulate magnitude
						mag += v.magnitude();
						v.normalise();
						phi = fabs(PI/2.0 - acos(r1.dp(v)));
						if (phi < best)
						{
							r2 = v;
							best = phi;
						}
					}

					// Finalise values and create z-vector from cross product
					r2.orthogonalise(r1);
					r2.normalise();
					r3 = r1*r2;
					mag /= r->nAtoms();
					mag -= radius_i*0.9;
					mag *= 0.75;

					// Construct transformation matrix
					atomTransform = baseTransform;
					atomTransform.applyTranslation(pos.x, pos.y, pos.z);
					A.setColumn(0, r1*mag, 0.0);
					A.setColumn(1, r2*mag, 0.0);
					A.setColumn(2, r3*mag, 0.0);
					A.setColumn(3, 0.0, 0.0, 0.0, 1.0);
					atomTransform *= A;

					// Render ring
					if (prefs.renderDashedAromatics())
					{
						if (drawStyle == Prefs::LineStyle) addLines(primitiveSet.segmentedLineRing(), atomTransform, colour_i);
						else addTriangles(primitiveSet.segmentedTubeRing(), atomTransform, colour_i);
					}
					else
					{
						if (drawStyle == Prefs::LineStyle) addLines(primitiveSet.lineRing(), atomTransform, colour_i);
						else addTriangles(primitiveSet.tubeRing(), atomTransform, colour_i);
					}

					id_i += p->nAtoms();
				}
			}
		}
	}
	
	// Hydrogen bonds
	if (prefs.drawHydrogenBonds())
	{
		double dotradius = prefs.hydrogenBondDotRadius();
		atoms = source->atomArray();
		prefs.copyColour(Prefs::HydrogenBondColour, colour_i);
		for (n = 0; n<source->nAtoms(); ++n)
		{
			// Get atom pointer
			i = atoms[n];
			
			// Skip hidden atoms
			if (i->isHidden()) continue;
			
			// Skip non-hydrogen atoms (i.e. 'i' is *always* the hydrogen atom)
			if (i->element() != 1) continue;
			
			// Get bond partner of the hydrogen atom (if there isn't on, skip it)
			if (i->nBonds() != 1) continue;
			k = i->bonds()->item->partner(i);

			// Grab atom coordinate and style
			pos = i->r();
			style_i = (drawStyle == Prefs::OwnStyle ? i->style() : drawStyle);
			if (style_i <= Prefs::TubeStyle) radius_i = 0.0;
			else if (style_i == Prefs::ScaledStyle) radius_i = source->styleRadius(Prefs::ScaledStyle, i->element()) - primitiveSet.scaledAtomAdjustment(i->element());
			else radius_i = source->styleRadius(style_i, i->element()) - primitiveSet.sphereAtomAdjustment();
			
			// Loop over other atoms
			for (m = 0; m<source->nAtoms(); ++m)
			{
				j = atoms[m];

				// Skip hidden atoms
				if (j->isHidden()) continue;
				
				// Is i bound to j?
				if (i->findBond(j) != NULL) continue;
				
				// Element check
				el_j = j->element();
				if ((el_j != 7) && (el_j != 8) && (el_j != 8) && (ElementMap::group(el_j) != 17)) continue;
				
				// Get (any) bond partner of atom j
				l = j->nBonds() == 0 ? NULL : j->bonds()->item->partner(j);

				// Apply the rules as given in:
				// "Satisfying Hydrogen-Bonding Potential in Proteins"
				// I. K. McDonald and J. M. Thornton, J. Mol. Biol. 238, 777793 (1994)

				// Rule 1 - Distance D-A is less than 3.9 Angstroms
				if (source->cell().distance(k,j) > 3.9) continue;
				
				// Rule 2 - Distance H-A is less than 2.5 Angstroms
				v = source->cell().mimVector(i,j);
				mag = v.magnitude();
				if (mag > 2.5) continue;
				
				// Rule 3 - Angle D-H-A is greater than 90 degrees
				if (source->cell().angle(k,i,j) <= 90.0) continue;
				
				// Rule 4 - Angle A'-A-D is greater than 90 degrees (only if atom j has a bond partner)
				if ((l != NULL) && (source->cell().angle(k,j,l) <= 90.0)) continue;
				
				// Rule 5 - Angle A'-A-H is greater than 90 degrees (only if atom j has a bond partner)
				if ((l != NULL) && (source->cell().angle(i,j,l) <= 90.0)) continue;
				
				// If we get here then its a hydrogen bond.
				// First, determine the 'drawable' region between the two atoms i,j
				// Adjust for atom radii in current style
				style_j = (drawStyle == Prefs::OwnStyle ? j->style() : drawStyle);
				if (style_j <= Prefs::TubeStyle) radius_j = 0.0;
				else if (style_j == Prefs::ScaledStyle) radius_j = source->styleRadius(Prefs::ScaledStyle, j->element()) - primitiveSet.scaledAtomAdjustment(j->element());
				else radius_j = source->styleRadius(style_j, j->element()) - primitiveSet.sphereAtomAdjustment();

				// Get normalised i-j vector
				r1 = v;
				r1.normalise();

				// Adjust starting position of drawing to the very closest point we will allow (where i and first dot would touch)
				pos += r1 * (radius_i + dotradius);
				// Adjust magnitude to be the available space between the two atoms
				mag -= (radius_i + radius_j + 2.0*dotradius);
				if (mag < 0.0) continue;
				
				// Determine spacing between dots (may be negative if there isn't enough room)
				delta = (mag - 6.0*dotradius) < 0.0 ? -1.0 : mag / 4.0;
				
				// The matrix 'atomtransform' will only contain the translation. Scaling will be applied to its copy in 'A'
				atomTransform = baseTransform;
				atomTransform.applyTranslation(pos);
				
				// H-bond dot 1
				if (delta > 0.0) atomTransform.applyTranslation(r1*delta);
				A = atomTransform;
				A.applyScaling(dotradius, dotradius, dotradius);
				addTriangles(primitiveSet.sphere(), A, colour_i);
				
				// H-bond dot 2
				if (delta > 0.0) atomTransform.applyTranslation(r1*delta);
				else atomTransform.applyTranslation(r1*mag*0.5);
				A = atomTransform;
				A.applyScaling(dotradius, dotradius, dotradius);
				addTriangles(primitiveSet.sphere(), A, colour_i);
				
				// H-bond dot 3
				if (delta > 0.0) atomTransform.applyTranslation(r1*delta);
				else atomTransform.applyTranslation(r1*mag*0.5);
				A = atomTransform;
				A.applyScaling(dotradius, dotradius, dotradius);
				addTriangles(primitiveSet.sphere(), A, colour_i);
			}
		}
	}
	Messenger::exit("RenderGroup::renderAtomsAndBonds");
}

// ATEN2 TODO

// 		// Render volume outline
// 		if (g->outlineVolume())
// 		{
// 			A.columnMultiply(0, g->nXYZ().x);
// 			A.columnMultiply(1, g->nXYZ().y);
// 			A.columnMultiply(2, g->nXYZ().z);
// 			renderPrimitive(Viewer::GridObject, primitives_[primitiveSet_].originCube(), textColour, A, GL_LINE, 1.0);
// 		}
// 
// 	}
// 	Messenger::exit("Viewer::renderGrids");
// }
// 
// // Render text for grids
// void Viewer::renderGridText(Model* source)
// {
// 	Messenger::enter("Viewer::renderGridText");
// 	// Cycle over grids stored in current model
// 	for (Grid *g = source->grids(); g != NULL; g = g->next)
// 	{
// 		// Check visibility
// 		if (!g->isVisible()) continue;
// 
// 		// Setup transformation matrix for Grid
// 		Matrix A;
// 		A.setIdentity();
// 		A.applyTranslation(g->origin());
// 		A.multiplyRotation(g->axes());
// 
// 		// Find GridPrimitive for this Grid
// 		GridPrimitive* gp = findGridPrimitive(g);
// 		if (gp == NULL)
// 		{
// 			printf("Internal Error: Tried to render text for Grid, but no GridPrimitive has been created.\n");
// 			continue;
// 		}
// 
// 		// Loop over axes
// 		for (int n=0; n<3; ++n)
// 		{
// 			// Is axis visible
// 			if (!g->isAxisVisible(n)) continue;
// 			
// 			// Dump text for this axis
// 			Vec3<double> r, r2;
// 			Vec4<double> screenr;
// 			// ATEN2 TODO
// // 			for (TextPrimitive3D *tp = gp->axisTextPrimitives(n).first(); tp != NULL; tp = tp->next)
// // 			{
// // 				r = A * tp->r();
// // 				r2 = source->modelToWorld(r, &screenr);
// // 				if (r2.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, tp->text(), 0, tp->rightAlign());
// // 			}
// 		}
// 	}
// 	Messenger::exit("Viewer::renderGridText");
// }


