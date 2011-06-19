/*
	*** Model Rendering
	*** src/render/engine_model.cpp
	Copyright T. Youngs 2007-2011

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
#include "classes/prefs.h"
#include "classes/forcefieldatom.h"
#include "classes/grid.h"
#include "model/model.h"
#include "base/pattern.h"
#include "gui/tcanvas.uih"
#include "base/sysfunc.h"

// Render bond
void RenderEngine::renderBond(Matrix A, Vec3<double> vij, Atom *i, Atom::DrawStyle style_i, GLfloat *colour_i, double radius_i, Atom *j, Atom::DrawStyle style_j, GLfloat *colour_j, double radius_j, Bond::BondType bt, double selscale, Bond *b)
{
	double dvisible, selvisible, factor, rij, phi;
	Vec3<double> ri, rj, localx, localy, localz, stickpos, dx;
	GLfloat alpha_i, alpha_j;
	Primitive *sticks[2] = { &stickLines_, &stickSelectedLines_ };
	int stickList;

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
	if (((bt == Bond::Double) || (bt == Bond::Triple)) && (b != NULL))
	{
		// Desired z-axis is already known ( = vijn)
		// X axis is bond plane vector
		// Y axis is xp of the two
		localx = i->findBondPlane(j,b,localz);
		localx = A.rotateVector(localx);
		localz = A.rotateVector(localz);
		localy = localx * localz;
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
		else if (phi >= 0.01) A.applyRotationAxis(-vij.y, vij.x, 0.0, phi, TRUE);
	}
	// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
	A.applyTranslationZ(radius_i);
	A.applyScalingZ(dvisible);
	
	// Draw first bond half
	switch (style_i)
	{
		case (Atom::StickStyle):
			if (!rebuildSticks_) break;
			// First vertex is at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = A * Vec3<double>(0.0,0.0,1.0);
			// Determine how many sticks to draw (bond multiplicity : aromatic still counts as one bond)
			stickList = i->isSelected() ? 1 : 0;
			switch (bt)
			{
				case (Bond::Double):
					dx = A.rotateVector(prefs.atomStyleRadius(Atom::StickStyle)*0.5,0.0,0.0);
					sticks[stickList]->defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					break;
				case (Bond::Triple):
					dx = A.rotateVector(prefs.atomStyleRadius(Atom::StickStyle),0.0,0.0);
					sticks[stickList]->defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_i, FALSE);
					break;
				default:
					sticks[stickList]->defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i, FALSE);
					sticks[stickList]->defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i, FALSE);
					break;
			}
			// Move to centre of visible bond, ready for next bond half
			A.applyTranslationZ(1.0);
			break;
		case (Atom::TubeStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_i][bt], colour_i, A);
			if (i->isSelected() && (selvisible > 0.0))
			{
				colour_i[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_i][bt], colour_i, A);
				colour_i[3] = alpha_i;
			}
			// Move to centre of visible bond, ready for next bond half
			A.applyTranslationZ(1.0);
			break;
		case (Atom::SphereStyle):
		case (Atom::ScaledStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_i][bt], colour_i, A);
			if (i->isSelected() && (selvisible > 0.0))
			{
				// Move to edge of selected atom and apply selection bond scaling
				A.applyTranslationZ((selscale*radius_i-radius_i) / dvisible);
				A.applyScalingZ(selvisible/dvisible);
				colour_i[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_i][bt], colour_i, A);
				colour_i[3] = alpha_i;
				// Move to centrepoint and reverse scaling back to 'dvisible'
				A.applyTranslationZ(1.0);
				A.applyScalingZ(dvisible/selvisible);
			}
			else A.applyTranslationZ(1.0);
			break;
	}
	
	// Draw second bond half
	switch (style_j)
	{
		case (Atom::StickStyle):
			if (!rebuildSticks_) break;
			// First vertex is *still* at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = A * Vec3<double>(0.0,0.0,1.0);
			stickList = j->isSelected() ? 1 : 0;
			switch (bt)
			{
				case (Bond::Double):
					dx = A.rotateVector(prefs.atomStyleRadius(Atom::StickStyle)*0.5,0.0,0.0);
					sticks[stickList]->defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					break;
				case (Bond::Triple):
					dx = A.rotateVector(prefs.atomStyleRadius(Atom::StickStyle),0.0,0.0);
					sticks[stickList]->defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(A[12]+dx.x, A[13]+dx.y, A[14]+dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(stickpos.x+dx.x, stickpos.y+dx.y, stickpos.z+dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(A[12]-dx.x, A[13]-dx.y, A[14]-dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(stickpos.x-dx.x, stickpos.y-dx.y, stickpos.z-dx.z, 0.0,0.0,1.0, colour_j, FALSE);
					break;
				default:
					sticks[stickList]->defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_j, FALSE);
					sticks[stickList]->defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j, FALSE);
					break;
			}
			break;
		case (Atom::TubeStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_j][bt], colour_j, A);
			if (j->isSelected())
			{
				colour_j[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_j][bt], colour_j, A);
				colour_j[3] = alpha_j;
			}
			break;
		case (Atom::SphereStyle):
		case (Atom::ScaledStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_j][bt], colour_j, A);
			if (j->isSelected() && (selvisible > 0.0))
			{
				A.applyScalingZ(selvisible / dvisible);
				colour_j[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_j][bt], colour_j, A);
				colour_j[3] = alpha_j;
			}
			break;
	}
}

// Render basic model information (atoms, bonds, labels, and glyphs)
void RenderEngine::renderModel(Model *source, Matrix basetransform)
{
	msg.enter("RenderEngine::renderModel");
	GLfloat colour_i[4], colour_j[4], alpha_i;;
	int id_i, labels, m, n, el_j;
	Dnchar text;
	double selscale, radius_i, radius_j, phi, mag, best, delta;
	double aradius[Atom::nDrawStyles];
	Atom *i, *j, *k, *l, **atoms;
	Vec3<double> pos, v, ijk, r1, r2, r3, r4;
	Vec4<double> screenr;
	Matrix atomtransform, A, B;
	Refitem<Bond,int> *rb;
	Refitem<Atom,int> *ra;
	Atom::DrawStyle style_i, style_j, globalstyle;
	Prefs::ColouringScheme scheme;
	ForcefieldAtom *ffa;
	
	// Grab global style values and atom radii
	scheme = prefs.colourScheme();
	globalstyle = prefs.renderStyle();
	selscale = prefs.selectionScale();
	for (n=0; n<Atom::nDrawStyles; ++n) aradius[n] = prefs.atomStyleRadius( (Atom::DrawStyle) n);
	
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
		atomtransform = basetransform;
		atomtransform.applyTranslation(pos.x, pos.y, pos.z);
		
		// Select colour
		if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_i);
		else switch (scheme)
		{
			case (Prefs::ElementScheme):
				elements().copyColour(i->element(), colour_i);
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
			case (Prefs::CustomScheme):
				i->copyColour(colour_i);
				break;
			default:
				break;
		}
		// Store copy of alpha value
		alpha_i = colour_i[3];
		
		// Get atom style and render associated object
		style_i = (globalstyle == Atom::IndividualStyle ? i->style() : globalstyle);
		
		if (style_i == Atom::StickStyle)
		{
			// Only need to draw something if the atom has no bonds
			if (i->nBonds() == 0)
			{
				if (i->isSelected()) stickSelectedLines_.plotCross(0.5, atomtransform, colour_i);
				else stickLines_.plotCross(0.5, atomtransform, colour_i);
			}
		}
		else
		{
			radius_i = aradius[style_i];
			if (style_i == Atom::ScaledStyle) radius_i *= elements().el[i->element()].atomicRadius;
			A = atomtransform;
			A.applyScaling(radius_i,radius_i,radius_i);
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].atom_, colour_i, A);
			if (i->isSelected())
			{
				colour_i[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedAtom_, colour_i, A);
				colour_i[3] = alpha_i;
			}
		}

		// Bonds
		// Grab some useful values from atom i
		id_i = i->id();
		if (style_i <= Atom::TubeStyle) radius_i = 0.0;
		else if (style_i == Atom::ScaledStyle) radius_i = prefs.styleRadius(i) - scaledAtomAdjustments_[i->element()];
		else radius_i = prefs.styleRadius(i) - sphereAtomAdjustment_;
		
		for (rb = i->bonds(); rb != NULL; rb = rb->next)
		{
			j = rb->item->partner(i);
			if (j->id() > id_i) continue;
			
			// Grab colour of second atom
			if (j->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_j);
			else switch (scheme)
			{
				case (Prefs::ElementScheme):
					elements().copyColour(j->element(), colour_j);
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
				case (Prefs::CustomScheme):
					j->copyColour(colour_j);
					break;
				default:
					break;
			}
			
			// Get atom style and radius
			style_j = (globalstyle == Atom::IndividualStyle ? j->style() : globalstyle);
			if (style_j <= Atom::TubeStyle) radius_j = 0.0;
			else if (style_j == Atom::ScaledStyle) radius_j = prefs.styleRadius(j) - scaledAtomAdjustments_[j->element()];
			else radius_j = prefs.styleRadius(j) - sphereAtomAdjustment_;
			
			// Calculate vector i->j
			v = source->cell()->mimd(j, i);
			
			// Render bond
			renderBond(atomtransform, v, i, style_i, colour_i, radius_i, j, style_j, colour_j, radius_j, rb->item->type(), selscale, rb->item);
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
						pos += source->cell()->mim(j->r(), r1);
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
						if (prefs.styleRadius(i) > radius_i) radius_i = prefs.styleRadius(i);
						v = source->cell()->mimd(i->r(), pos);
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
					atomtransform = basetransform;
					atomtransform.applyTranslation(pos.x, pos.y, pos.z);
					A.setColumn(0, r1*mag, 0.0);
					A.setColumn(1, r2*mag, 0.0);
					A.setColumn(2, r3*mag, 0.0);
					A.setColumn(3, 0.0, 0.0, 0.0, 1.0);
					atomtransform *= A;

					// Render ring
					if (prefs.renderDashedAromatics())
					{
						if (globalstyle == Atom::StickStyle) renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].segmentedLineRings_, colour_i, atomtransform);
						else renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].segmentedTubeRings_, colour_i, atomtransform);
					}
					else
					{
						if (globalstyle == Atom::StickStyle) renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].lineRings_, colour_i, atomtransform);
						else renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].tubeRings_, colour_i, atomtransform);
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
			style_i = (globalstyle == Atom::IndividualStyle ? i->style() : globalstyle);
			if (style_i <= Atom::TubeStyle) radius_i = 0.0;
			else if (style_i == Atom::ScaledStyle) radius_i = prefs.styleRadius(i) - scaledAtomAdjustments_[i->element()];
			else radius_i = prefs.styleRadius(i) - sphereAtomAdjustment_;
			
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
				if ((el_j != 7) && (el_j != 8) && (el_j != 8) && (elements().group(el_j) != 17)) continue;
				
				// Get (any) bond partner of atom j
				l = j->nBonds() == 0 ? NULL : j->bonds()->item->partner(j);

				// Apply the rules as given in:
				// "Satisfying Hydrogen-Bonding Potential in Proteins"
				// I. K. McDonald and J. M. Thornton, J. Mol. Biol. 238, 777793 (1994)

				// Rule 1 - Distance D-A is less than 3.9 Angstroms
				if (source->cell()->distance(k,j) > 3.9) continue;
				
				// Rule 2 - Distance H-A is less than 2.5 Angstroms
				v = source->cell()->mimd(j,i);
				mag = v.magnitude();
				if (mag > 2.5) continue;
				
				// Rule 3 - Angle D-H-A is greater than 90 degrees
				if (source->cell()->angle(k,i,j) <= 90.0) continue;
				
				// Rule 4 - Angle A'-A-D is greater than 90 degrees (only if atom j has a bond partner)
				if ((l != NULL) && (source->cell()->angle(k,j,l) <= 90.0)) continue;
				
				// Rule 5 - Angle A'-A-H is greater than 90 degrees (only if atom j has a bond partner)
				if ((l != NULL) && (source->cell()->angle(i,j,l) <= 90.0)) continue;
				
				// If we get here then its a hydrogen bond.
				// First, determine the 'drawable' region between the two atoms i,j
				// Adjust for atom radii in current style
				style_j = (globalstyle == Atom::IndividualStyle ? j->style() : globalstyle);
				if (style_j <= Atom::TubeStyle) radius_j = 0.0;
				else if (style_j == Atom::ScaledStyle) radius_j = prefs.styleRadius(j) - scaledAtomAdjustments_[j->element()];
				else radius_j = prefs.styleRadius(j) - sphereAtomAdjustment_;
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
				atomtransform = basetransform;
				atomtransform.applyTranslation(pos);
				
				// H-bond dot 1
				if (delta > 0.0) atomtransform.applyTranslation(r1*delta);
				A = atomtransform;
				A.applyScaling(dotradius, dotradius, dotradius);
				renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].spheres_, colour_i, A);
				
				// H-bond dot 2
				if (delta > 0.0) atomtransform.applyTranslation(r1*delta);
				else atomtransform.applyTranslation(r1*mag*0.5);
				A = atomtransform;
				A.applyScaling(dotradius, dotradius, dotradius);
				renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].spheres_, colour_i, A);
				
				// H-bond dot 3
				if (delta > 0.0) atomtransform.applyTranslation(r1*delta);
				else atomtransform.applyTranslation(r1*mag*0.5);
				A = atomtransform;
				A.applyScaling(dotradius, dotradius, dotradius);
				renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].spheres_, colour_i, A);
			}
		}
	}
	msg.exit("RenderEngine::renderModel");
}

// Render model cell
void RenderEngine::renderCell(Model *source)
{
	if (source->cell()->type() == UnitCell::NoCell) return;
	msg.enter("RenderEngine::renderCell");
	GLfloat colour[4];
	prefs.copyColour(Prefs::UnitCellColour, colour);
	glColor4fv(colour);
	glLoadIdentity();
	Matrix A = source->modelViewMatrix() * source->cell()->axes();
	glMultMatrixd(A.matrix());
	primitives_[Q_].wireCube_.sendToGL();
	glTranslated(-0.5, -0.5, -0.5);
	Vec3<double> v = source->cell()->lengths();
	glScaled(1.0 / v.x, 1.0 / v.y, 1.0 / v.z);
	prefs.copyColour(Prefs::UnitCellAxesColour, colour);
	glColor4fv(colour);
	primitives_[Q_].cellAxes_.sendToGL();
	msg.exit("RenderEngine::renderCell");
}

// Render grids
void RenderEngine::renderGrids(Model *source)
{
	msg.enter("RenderEngine::renderGrids");
	Matrix A;
	GLenum style;
	GLfloat colour[4], textcolour[4];
	
	// Copy text colour
	prefs.copyColour(Prefs::TextColour, textcolour);
	
	// Cycle over grids stored in current model
	for (Grid *g = source->grids(); g != NULL; g = g->next)
	{
		// Check visibility
		if (!g->isVisible()) continue;
		// Does a GridPrimitive already exist?
		GridPrimitive *gp = findGridPrimitive(g);
		// If the GridPrimitive exists and is 'in date', no need to regenerate it...
		if ((gp == NULL) || g->shouldRerender())
		{
			// Generate new GridPrimitive if required
			if (gp == NULL)
			{
				gp = gridPrimitives_.add();
				gp->setSource(g);
			}
			if (g->type() == Grid::RegularXYZData) gp->createSurfaceMarchingCubes();
			else if (g->type() == Grid::FreeXYZData)
			{
				// Construct the Delaunay triangulization of the surface
				// 				DelaunaySurface D(g);
			}
			else gp->createSurface2D();
			g->updateRenderPoint();
		}
		
		// Grid primitive now exists (or has been updated) so create transformation and render it
		A.setIdentity();
		A.applyTranslation(g->origin());
		A.multiplyRotation(g->axes());
		if (g->style() == Grid::TriangleSurface) style = GL_LINE;
		else if (g->style() == Grid::SolidSurface) style = GL_FILL;
		else style = GL_POINTS;
		if (g->useColourScale()) renderPrimitive(RenderEngine::GridObject, &gp->primaryPrimitive(), gp->primaryIsTransparent(), NULL, A, style);
		else
		{
			g->copyPrimaryColour(colour);
			renderPrimitive(RenderEngine::GridObject, &gp->primaryPrimitive(), gp->primaryIsTransparent(), colour, A, style);
		}
		if (g->useSecondary())
		{
			if (g->useColourScale()) renderPrimitive(RenderEngine::GridObject, &gp->secondaryPrimitive(), gp->secondaryIsTransparent(), NULL, A, style);
			else
			{
				g->copySecondaryColour(colour);
				renderPrimitive(RenderEngine::GridObject, &gp->secondaryPrimitive(), gp->secondaryIsTransparent(), colour, A, style);
			}
		}
		// Render volume outline
		if (g->outlineVolume())
		{
			A.columnMultiply(0, g->nPoints().x);
			A.columnMultiply(1, g->nPoints().y);
			A.columnMultiply(2, g->nPoints().z);
			renderPrimitive(RenderEngine::GridObject, primitives_[Q_].originCubes_, textcolour, A, GL_LINE, 1.0);
		}
	}
	msg.exit("RenderEngine::renderGrids");
}

// Render additional model information (measurements etc.)
void RenderEngine::renderModelOverlays(Model *source)
{
	Vec3<double> r1, r2, r3, r4, rji, rjk, pos;
	Vec4<double> screenr;
	GLfloat colour[4];
	double gamma, t;
	int labels;
	Dnchar text(512);
	Atom **atoms, *i;
	ForcefieldAtom *ffa;

	// Clear depth buffer to force lines on top of existing primitives,
	glDisable(GL_DEPTH_TEST);
	
	// Set colour
	prefs.copyColour(Prefs::TextColour, colour);
	glColor4fv(colour);
	
	// Atoms and Bonds
	atoms = source->atomArray();
	for (int n = 0; n<source->nAtoms(); ++n)
	{
		// Get atom pointer
		i = atoms[n];
		
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Labels
		labels = i->labels();
		if (labels == 0) continue;
		
		// Grab forcefield atom pointer
		ffa = i->type();
		
		// Blank label string
		text.clear();
		// Now add on all parts of the label that are required
		if (labels&(1 << Atom::IdLabel)) text.strcatf("%i ", i->id()+1);
		if (labels&(1 << Atom::ElementLabel)) text.strcatf("%s ", elements().symbol(i));
		if (labels&(1 << Atom::TypeLabel))
		{
			if (ffa == NULL) text.strcat("[None] ");
			else text.strcatf("[%i %s] ", ffa->typeId(), ffa->name());
		}
		if (labels&(1 << Atom::EquivLabel)) text.strcatf("[=%s] ", ffa == NULL ? "None" : ffa->equivalent());
		if (labels&(1 << Atom::ChargeLabel)) text.strcatf(prefs.chargeLabelFormat(), i->charge());
		
		// Add text object
		r2 = source->modelToWorld(i->r(), &screenr);
		if (r2.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, text.get());
	}
	
	// Measurements
	// Apply standard transformation matrix to OpenGL so we may just use local atom positions for vertices
	// Load model transformation matrix
	glLoadIdentity();
	glMultMatrixd(modelTransformationMatrix_.matrix());
	
	// Distances
	for (Measurement *m = source->distanceMeasurements(); m != NULL; m = m->next)
	{
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden()) continue;
		r1 = atoms[0]->r();
		r2 = atoms[1]->r();
		glBegin(GL_LINES);
		glVertex3d(r1.x, r1.y, r1.z);
		glVertex3d(r2.x, r2.y, r2.z);
		glEnd();
		r4 = (r1+r2)*0.5;
		r3 = source->modelToWorld(r4, &screenr);
		if (r3.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, ftoa(m->value(), prefs.distanceLabelFormat()), 0x212b);
	}
	
	// Angles
	for (Measurement *m = source->angleMeasurements(); m != NULL; m = m->next)
	{
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden()) continue;
		r1 = atoms[0]->r();
		r2 = atoms[1]->r();
		r3 = atoms[2]->r();
		glBegin(GL_LINE_STRIP);
		glVertex3d(r1.x, r1.y, r1.z);
		glVertex3d(r2.x, r2.y, r2.z);
		glVertex3d(r3.x, r3.y, r3.z);
		glEnd();
		// Curved angle marker
		rji = (r1 - r2);
		rjk = (r3 - r2);
		rji.normalise();
		rjk.normalise();
		gamma = acos(rji.dp(rjk));
		// Draw segments
		t = 0.0;
		glBegin(GL_LINES);
		for (int n=0; n<11; n++)
		{
			pos = (rji * sin((1.0-t)*gamma) + rjk * sin(t*gamma)) / sin(gamma);
			pos *= 0.2;
			pos += r2;
			glVertex3d(pos.x, pos.y, pos.z);
			t += 0.1;
			// Store text position
			if (n == 5) r4 = pos;
		}
		glEnd();
		// Determine left or right-alignment of text
		source->modelToWorld(r2, &screenr);
		gamma = screenr.x;
		r3 = source->modelToWorld(r4, &screenr);
		if (r3.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, ftoa(m->value(), prefs.angleLabelFormat()), 176, gamma > screenr.x);
	}
	
	// Torsions
	for (Measurement *m = source->torsionMeasurements(); m != NULL; m = m->next)
	{
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden() || atoms[3]->isHidden()) continue;
		r1 = atoms[0]->r();
		r2 = atoms[1]->r();
		r3 = atoms[2]->r();
		r4 = atoms[3]->r();
		glBegin(GL_LINE_STRIP);
		glVertex3d(r1.x, r1.y, r1.z);
		glVertex3d(r2.x, r2.y, r2.z);
		glVertex3d(r3.x, r3.y, r3.z);
		glVertex3d(r4.x, r4.y, r4.z);
		glEnd();
		r1 = (r2+r3)*0.5;
		r4 = source->modelToWorld(r1, &screenr);
		if (r4.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, ftoa(m->value(), prefs.angleLabelFormat()), 176);
	}
	
	// Re-enable depth buffer
	glEnable(GL_DEPTH_TEST);
}
