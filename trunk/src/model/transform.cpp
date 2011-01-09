/*
	*** Model transformation functions
	*** src/model/transform.cpp
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

#include "model/model.h"
#include "model/undoevent.h"
#include "model/undostate.h"
#include "gui/gui.h"

// Variables
Vec3<double> transformCOG;

// Return the translation scale
double Model::translateScale() const
{
	return translateScale_;
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
	msg.enter("Model::prepareTransform");
	if (selection_.nItems() == 0)
	{
		msg.exit("Model::prepareTransform");
		return;
	}
	// Reference point for mim will be the updating cog
	// At the same time, calculate a unit radius for the centre of geometry
	Vec4<double> screenr;
	transformCOG.zero();
	translateScale_ = 0.0;
	for (Refitem<Atom,int> *ri = selection_.first(); ri != NULL; ri = ri->next)
	{
		transformCOG += gui.mainWidget->modelToWorld(ri->item->r(), &screenr, 1.0);
		translateScale_ += screenr.w;
	}
	transformCOG /= selection_.nItems();
	translateScale_ /= selection_.nItems();
	msg.exit("Model::prepareTransform");
}

// Finalize Model Manipulation
void Model::finalizeTransform(Reflist< Atom,Vec3<double> > &originalr, const char *statetitle, bool nofold)
{
	// Called after mouse-up.
	// Atom positions may have moved outside the boundaries of the box, so need to re-fold.
	if (!nofold) foldAllAtoms();
	beginUndoState(statetitle);
	// Go through list of atoms in 'originalr', work out delta, and store
	if (recordingState_ != NULL)
	{
		TranslateEvent *newchange;
		Vec3<double> delta;
		for (Refitem< Atom,Vec3<double> > *ri = originalr.first(); ri != NULL; ri = ri->next)
		{
			delta = ri->item->r() - ri->data;
			newchange = new TranslateEvent;
			newchange->set(ri->item->id(), delta);
			recordingState_->addEvent(newchange);
		}
	}
	changeLog.add(Log::Coordinates);
	endUndoState();
}

// Free Rotation of Selection
void Model::rotateSelectionWorld(double dx, double dy)
{
	// Rotate the selection about the calculated centre of geometry.
	// We are passed the 2D-movement of the mouse, which we use to generate a rotation matrix.
	// We then apply this to the stored *world* coordinates of
	// the selected atoms, which we then unproject to get the new model coordinates.
	msg.enter("Model::rotateSelectionWorld");
	double rotx, roty;
	Vec3<double> newr;
	Matrix rotmat, inverse;
	rotx = dy;
	roty = dx;
	rotmat.createRotationXY(rotx, roty);
	inverse = modelViewMatrixInverse();

	for (Refitem<Atom,int> *ri = selection_.first(); ri != NULL; ri = ri->next)
	{
		// Rotate this atom's position about the geometric centre of all selected atoms.
		newr = (rotmat * (gui.mainWidget->modelToWorld(ri->item->r()) - transformCOG)) + transformCOG;
		newr = inverse.transform(newr);
		ri->item->r() = newr + cell_.centre();
	}

	// Update model measurements
	updateMeasurements();

	changeLog.add(Log::Visual);

	msg.exit("Model::rotateSelectionWorld");
}

// Rotate about defined vector
void Model::rotateSelectionVector(Vec3<double> origin, Vec3<double> vector, double angle, bool markonly)
{
	msg.enter("Model::rotateSelectionVector");
	Matrix A;
	Vec3<double> tempv;
	if (selection(markonly) == NULL)
	{
		if (!markonly) msg.print("No atoms selected!\n");
		msg.exit("Model::rotateSelectionVector");
		return;
	}
	
	// Create rotation matrix
	A.createRotationAxis(vector.x, vector.y, vector.z, angle, TRUE);

	// Loop over selected atoms
	for (Refitem<Atom,int> *ri = selection(markonly); ri != NULL; ri = ri->next)
	{
		tempv = A * (ri->item->r() - origin);
		positionAtom(ri->item, tempv+origin);
	}

	// Update model measurements
	updateMeasurements();

	changeLog.add(Log::Visual);

	msg.exit("Model::rotateSelectionVector");
}

// Rotation of selection about screen Z-axis
void Model::rotateSelectionZaxis(double dz)
{
	msg.enter("Model::rotateSelectionZaxis");
	Vec3<double> newr;
	Matrix rotmat, inverse;
	rotmat.createRotationZ(dz);
	inverse = modelViewMatrixInverse();
	
	for (Refitem<Atom,int> *ri = selection_.first(); ri != NULL; ri = ri->next)
	{
		// Rotate this atom's position about the geometric centre of all selected atoms.
		newr = (rotmat * (gui.mainWidget->modelToWorld(ri->item->r()) - transformCOG)) + transformCOG;
		newr = inverse.transform(newr);
		ri->item->r() = newr + cell_.centre();
	}
	
	// Update model measurements
	updateMeasurements();
	
	changeLog.add(Log::Visual);
	msg.exit("Model::rotateSelectionZaxis");
}

// Translate Selection in world coordinates
void Model::translateSelectionWorld(const Vec3<double> &v, bool markonly)
{
	// Translate the selected atoms in the local XY plane
	msg.enter("Model::translateSelectionWorld");
	Vec3<double> newr;
	Matrix inverse;
	// Take the local coordinates of each selected atom and add our position delta to it.
	// We then unproject this new local coordinate to get the new model (world) coordinate.
	inverse = modelViewMatrixInverse();
	for (Refitem<Atom,int> *ri = selection(markonly); ri != NULL; ri = ri->next)
	{
		newr = gui.mainWidget->modelToWorld(ri->item->r()) + v;
		newr = inverse * newr + cell_.centre();
		positionAtom(ri->item, newr);
	}

	// Update model measurements
	updateMeasurements();

	changeLog.add(Log::Visual);
	msg.exit("Model::translateSelectionWorld");
}

// Move selected atoms in local space
void Model::translateSelectionLocal(const Vec3<double> &tvec, bool markonly)
{
	// Translate the model's current selection by the vector supplied.
	msg.enter("Model::translateSelectionLocal");
	for (Refitem<Atom,int> *ri = selection(markonly); ri != NULL; ri = ri->next) translateAtom(ri->item,tvec);

	// Update model measurements
	updateMeasurements();

	changeLog.add(Log::Visual);

	msg.exit("Model::translateSelectionLocal");
}

// Mirror selection in local coordinates
void Model::mirrorSelectionLocal(int axis, bool markonly)
{
	msg.enter("Model::mirrorSelectionLocal");
	// Get selection's local COG in the desired coordinate
	Vec3<double> newr, vec, cog = selectionCentreOfGeometry();
	for (int n=0; n<3; n++) vec.set(n, n == axis ? -1.0 : 1.0);
	for (Refitem<Atom,int> *ri = selection(markonly); ri != NULL; ri = ri->next)
	{
		// Calculate newr
		newr = (ri->item->r() - cog);
		newr.multiply(vec);
		newr += cog;
		positionAtom(ri->item, newr);
	}

	// Update model measurements
	updateMeasurements();

	changeLog.add(Log::Visual);

	msg.exit("Model::mirrorSelectionLocal");
}

// Puts the selections centre of geometry at 0,0,0
void Model::centre(const Vec3<double> &v, bool lockx, bool locky, bool lockz)
{
	centre(v.x, v.y, v.z, lockx, locky, lockz);
}

// Centre current selection at specified coordinates
void Model::centre(double newx, double newy, double newz, bool lockx, bool locky, bool lockz)
{
	msg.enter("Model::centre");
	Vec3<double> cog(newx, newy, newz);
	cog -= selectionCentreOfGeometry();
	if (lockx) cog.x = 0.0;
	if (locky) cog.y = 0.0;
	if (lockz) cog.z = 0.0;
	translateSelectionLocal(cog);
	msg.exit("Model::centre");
}

// Matrix transform current selection
void Model::matrixTransformSelection(Vec3<double> origin, Matrix matrix, bool markedonly)
{
	msg.enter("Model::matrixTransformSelection");
	Vec3<double> newr;
	for (Refitem<Atom,int> *ri = selection(markedonly); ri != NULL; ri = ri->next)
	{
		newr = matrix * (ri->item->r() - origin);
		positionAtom(ri->item, newr + origin);
	}

	// Update model measurements
	updateMeasurements();

	changeLog.add(Log::Visual);

	msg.exit("Model::matrixTransformSelection");
}
