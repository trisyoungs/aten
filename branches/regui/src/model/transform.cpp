/*
	*** Model transformation functions
	*** src/model/transform.cpp
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
#include "classes/atom.h"
#include "base/master.h"
#include "base/prefs.h"

// Variables
Vec3<double> cog, localcog;

// Return the translation scale
double Model::translateScale()
{
	return translateScale_;
}

// Puts the selections centre of geometry at 0,0,0
void Model::centre(const Vec3<double> &v)
{
	centre(v.x, v.y, v.z);
}

// Prepare model for atom transform
void Model::prepareTransform()
{
	// Called at the beginning of a transform operation, prior to mouse movement
	// Take the first selected atom to be the initial reference point to calculate
	// the centre of geometry. Then, mim every other selected atom so that the
	// atomic positions are all minimum image to the reference (so transforms
	// work properly in periodic systems). We re-fold the positions on mouse-up.
	// Return if no cog could be defined (i.e. no atoms selected)
	dbgBegin(Debug::Calls,"Model::prepareTransform");
	if (nSelected_ < 1)
	{
		dbgEnd(Debug::Calls,"Model::prepareTransform");
		return;
	}
	cog.zero();
	// Reference point for mim will be the updating cog
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) cog += i->rWorld();
	cog /= nSelected_;
	// Calculate a unit radius for the centre of geometry
	localcog = cog;
	Vec4<double> pvec = worldToScreen(localcog);
	translateScale_ = pvec.w;
	dbgEnd(Debug::Calls,"Model::prepareTransform");
}

// Finalize Model Manipulation
void Model::finalizeTransform(Reflist< Atom,Vec3<double> > &originalr)
{
	// Called after mouse-up.
	// Atom positions may have moved outside the boundaries of the box, so need to re-fold.
	foldAllAtoms();
	logChange(Change::CoordinateLog);
	projectAll();
	beginUndostate("Transform Selection");
	// Go through list of atoms in 'originalr', work out delta, and store
	if (recordingState_ != NULL)
	{
		Change *newchange;
		Vec3<double> delta;
		for (Refitem< Atom,Vec3<double> > *ri = originalr.first(); ri != NULL; ri = ri->next)
		{
			delta = ri->item->r() - ri->data;
			newchange = recordingState_->addChange();
			newchange->set(Change::TranslateEvent,ri->item->id());
			newchange->set(Change::TranslateEvent,&delta);
		}
	}
	endUndostate();
}

// Free Rotation of Selection
void Model::rotateSelectionWorld(double dx, double dy)
{
	// Rotate the selection about the calculated centre of geometry.
	// We are passed the 2D-movement of the mouse, which we use to generate a rotation matrix.
	// We then apply this to the stored *world* coordinates of 
	// the selected atoms, which we then unproject to get the new model coordinates.
	dbgBegin(Debug::Calls,"Model::rotateSelectionWorld");
	static double rotx, roty, cosx, cosy, sinx, siny;
	static Vec3<double> newr;
	static Mat3<double> rotmat;
	rotx = dy / 10.0;
	roty = dx / 10.0;
	cosx = cos(rotx);
	cosy = cos(roty);
	sinx = sin(rotx);
	siny = sin(roty);
	rotmat.set(0,cosy,0.0,siny);
	rotmat.set(1,-sinx*-siny,cosx,-sin(rotx)*cos(roty));
	rotmat.set(2,cosx*-siny,sinx,cosx*cosy);
	// Now, make the rotation 
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		if (!i->isSelected()) continue;
		// Rotate this atom's position about the geometric centre of all selected atoms.
		newr = i->rWorld() - localcog;
		newr = (rotmat * newr) + localcog;
		i->r() = (viewMatrixInverse_ * newr) + cell_.centre();;
	}
	logChange(Change::VisualLog);
	projectSelection();
	dbgEnd(Debug::Calls,"Model::rotateSelectionWorld");
}

// Rotate about defined vector
void Model::rotateSelectionVector(Vec3<double> origin, Vec3<double> vector, double step)
{
	dbgBegin(Debug::Calls,"Model::rotateSelectionVector");
	static Mat3<double> r, u, ut, gr, Igr;
	Vec3<double> tempv;
	Atom *i = firstSelected();
	if (i == NULL)
	{
		msg(Debug::None,"No atoms selected!\n");
		dbgEnd(Debug::Calls,"Model::rotateSelectionVector");
		return;
	}
	// Generate target coordinate system, defined from xaxis == v and orthogonal vectors from first atom
	vector.normalise();
	u.rows[0] = vector;
	tempv = i->r() - origin;
	tempv.normalise();
	u.rows[1] = tempv - vector * tempv.dp(vector);
	u.rows[1].normalise();
	u.rows[2] = vector * u.rows[1];
	u.rows[2].normalise();
	ut = u.transpose();

	// Create rotation matrix
	step /= DEGRAD;
	r.set(0,1.0,0.0,0.0);
	r.set(1,0.0,cos(step),sin(step));
	r.set(2,0.0,-sin(step),cos(step));

	// Create grand rotation matrix
	gr = ut * r * u;
	Igr.setIdentity();
	Igr = Igr - gr;

	// Loop over atoms
	while (i != NULL)
	{
		tempv = gr * i->r();
		tempv += Igr * origin;
		positionAtom(i, tempv);
		//i->r() = tempv;
		i = i->nextSelected();
	}
	logChange(Change::StructureLog);
	dbgEnd(Debug::Calls,"Model::rotateSelectionVector");
}

// Rotation of selection about screen Z-axis
void Model::rotateSelectionZaxis(double dz)
{
	// Rotate about the perceived z-axis by changing the up vector of the camera.
	dbgBegin(Debug::Calls,"Model::rotateSelectionZaxis");
	//GLdouble newx, newy;   TODO
	//dx = (dx / DEGRAD ) * 2.0f;
	//master.activemodel->adjust_camera(0.0,0.0,0.0,dx);
	//master.activemodel->mmatTransform_all();
	dbgEnd(Debug::Calls,"Model::rotateSelectionZaxis");
}

// Translate Selection in world coordinates
void Model::translateSelectionWorld(const Vec3<double> &v)
{
	// Translate the selected atoms in the local XY plane
	dbgBegin(Debug::Calls,"Model::translateSelectionWorld");
	static Vec3<double> newr;
	// No need to account for orientation / rotation of view, since we do the transformation in world coordinates.
	// So, take the local coordinates of each selected atom and add our position delta to it.
	// We then unproject this new local coordinate to get the new model (world) coordinate.
	// Grab unit cell origin
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected())
	{
		newr = i->rWorld() + v;
		//newr += v;
		newr = (viewMatrixInverse_ * newr) + cell_.centre();
		positionAtom(i, newr);
	}
	logChange(Change::VisualLog);
	projectSelection();
	dbgEnd(Debug::Calls,"Model::translateSelectionWorld");
}

// Move selected atoms in local space
void Model::translateSelectionLocal(const Vec3<double> &tvec)
{
	// Translate the model's current selection by the vector supplied.
	dbgBegin(Debug::Calls,"Model::translateSelectionLocal");
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected()) translateAtom(i,tvec);
	//logChange(Change::VisualLog);
	projectSelection();
	dbgEnd(Debug::Calls,"Model::translateSelectionLocal");
}

// Mirror selection in local coordinates
void Model::mirrorSelectionLocal(int axis)
{
	dbgBegin(Debug::Calls,"Model::mirrorSelectionLocal");
	// Get selection's local COG in the desired coordinate
	Vec3<double> cog = selectionCog();
	for (int n=0; n<3; n++) if (n != axis) cog.set(n, 0.0);
	Vec3<double> newr;
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected())
	{
		// Calculate newr
		newr = i->r() - cog;
		newr.set(axis, -newr.get(axis));
		newr += cog;
		positionAtom(i, newr);

		// Get coordinates relative to COG
		//mimd = cell_.mimd(i->r(), cog);
		// Flip specified coordinate
		//mimd.set(axis, -mimd.get(axis));
		// Store new coordinate
		//positionAtom(i, mimd + cog);
	}
	logChange(Change::VisualLog);
	projectSelection();
	dbgEnd(Debug::Calls,"Model::mirrorSelectionLocal");
}

// Centre current selection at specified coordinates
void Model::centre(double newx, double newy, double newz)
{
	dbgBegin(Debug::Calls,"Model::centre");
	Vec3<double> cog(newx, newy, newz);
	cog -= selectionCog();
	translateSelectionLocal(cog);
	dbgEnd(Debug::Calls,"Model::centre");
}
