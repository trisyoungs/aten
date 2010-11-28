/*
	*** Model view functions
	*** src/model/view.cpp
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
#include "gui/gui.h"

// Set rendering source
void Model::setRenderSource(Model::RenderSource rs)
{
	renderSource_ = rs;
}

// Return rendering source
Model::RenderSource Model::renderSource() const
{
	return renderSource_;
}

// Return the current rendering source for the model
Model *Model::renderSourceModel()
{
	switch (renderSource_)
	{
		case (Model::ModelSource):
			return this;
		case (Model::TrajectorySource):
			return trajectoryCurrentFrame_;
	}
	return NULL;
}

// Set whether to render from vibration frames
void Model::setRenderFromVibration(bool b)
{
	renderFromVibration_ = b;
}

// Return whether to render from vibration frames
bool Model::renderFromVibration()
{
	return renderFromVibration_;
}

// Set the current modelview matrix
void Model::setModelViewMatrix(Mat4<double> &rmat)
{
	if (parent_ == NULL) modelViewMatrix_ = rmat;
	else parent_->setModelViewMatrix(rmat);
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the current modelview matrix
Mat4<double> &Model::modelViewMatrix()
{
	return (parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix());
}

// Spin the model about the z axis
void Model::zRotateView(double angle)
{
// 	adjustCamera(0.0, 0.0, 0.0);   BROKEN
}

// Adjust Camera
void Model::adjustCamera(double dx, double dy, double dz)
{
	// Adjust the models camera variables
	msg.enter("Model::adjustCamera");
	if (parent_ == NULL)
	{
		modelViewMatrix_.rows[3].add(dx, -dy, dz, 0.0);
		// Never let camera z go below -1.0...
		if (modelViewMatrix_.rows[3].z > -1.0) modelViewMatrix_.rows[3].z = -1.0;
	}
	else parent_->adjustCamera(dx, dy, dz);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::adjustCamera");
}

// Adjust camera zoom
void Model::adjustZoom(bool zoomin)
{
	msg.enter("Model::adjustZoom");
	double dz = (parent_ == NULL ? -modelViewMatrix_.rows[3].z : -parent_->modelViewMatrix().rows[3].z);
	dz *= prefs.zoomThrottle();
	if (zoomin) dz = -dz;
	adjustCamera(0.0,0.0,dz);
	gui.mainWidget->doProjection();
	msg.exit("Model::adjustZoom");
}

// Reset View
void Model::resetView()
{
	// Reset the modelview matrix and the camera
	msg.enter("Model::resetView");
	Vec4<double> screenr;
	Atom *i, target;
	bool done = FALSE;
	double z, largest = 0.0;
	parent_ == NULL ? modelViewMatrix_.setIdentity() : parent_->modelViewMatrix().setIdentity();
	// Fit model to screen
	// Crude approach - find largest coordinate and zoom out so that {0,0,largest} is visible on screen
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		z = i->r().absMax();
		if (z > largest) largest = z;
	}
	target.r() = cell_.centre();
	target.r().add(0.0,0.0,cell_.lengths().z+largest);
	modelViewMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
	// Now, adjust camera matrix so that this atom is on-screen.
	// Need to do a check for the viability of the canvas first...
	if (gui.mainWidget->isValid() && (atoms_.nItems() != 0))
	{
		if (prefs.hasPerspective())
		{
			do
			{
				// Project our local atom and grab the z screen coordinate
				modelViewMatrix_.rows[3].z -= 1.0;
				gui.mainWidget->updateTransformation(modelViewMatrix(), cell_.centre());
				z = gui.mainWidget->modelToWorld(target.r()).z;
			} while (z > -5.0);
		}
		else
		{
			do
			{
				// Project our local atom and grab the z screen coordinate
				modelViewMatrix_.rows[3].z -= 1.0;
				gui.mainWidget->modelToWorld(target.r(), &screenr);
				done = TRUE;
				if ((screenr.x < 0.0) || (screenr.x > gui.mainWidget->width())) done = FALSE;
				if ((screenr.y < 0.0) || (screenr.y > gui.mainWidget->height())) done = FALSE;
				if (screenr.z < 0.0) done = FALSE;
			} while (!done);
		}
	}
	else modelViewMatrix_.rows[3].set(0.0, 0.0, -10.0, 1.0);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::resetView");
}

// Rotate free
void Model::axisRotateView(Vec3<double> vec, double angle)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::axisRotateView");
	static double theta, camrot;
	static Mat4<double> newrotmat, oldrotmat;
	if (parent_ == NULL)
	{
		// Generate quaternion
		newrotmat.createRotationAxis(vec.x, vec.y, vec.z, angle);
		oldrotmat = modelViewMatrix_;
		// Now, multiply our matrices together...
		modelViewMatrix_ = newrotmat * oldrotmat;
	}
	else parent_->axisRotateView(vec, angle);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::axisRotateView");
}

// Set exact rotation of model (angles passed in degrees)
void Model::setRotation(double rotx, double roty)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::setRotation");
	double sinx, cosx, siny, cosy;
	// Calculate cos/sin terms for needless speedup!
	if (parent_ == NULL)
	{
		rotx /= DEGRAD;
		roty /= DEGRAD;
		cosx = cos(rotx);
		cosy = cos(roty);
		sinx = sin(rotx);
		siny = sin(roty);
		modelViewMatrix_.rows[0].set(cosy,0.0,siny,0.0);
		modelViewMatrix_.rows[1].set((-sinx)*(-siny),cosx,(-sinx)*cosy,0.0);
		modelViewMatrix_.rows[2].set(cosx*(-siny),sinx,cosx*cosy,0.0);
		modelViewMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
	}
	else parent_->setRotation(rotx, roty);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::setRotation");
}

// Rotate free
void Model::rotateView(double dx, double dy)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::rotateView");
	double rotx, roty, sinx, cosx, siny, cosy;
	static Mat4<double> newrotmat, oldrotmat;
	if (parent_ == NULL)
	{
		rotx = -dy / 50.0;
		roty = -dx / 50.0;
		// Calculate cos/sin terms for needless speedup!
		cosx = cos(rotx);
		cosy = cos(roty);
		sinx = sin(rotx);
		siny = sin(roty);

		// Construct muptiplier matrix - will contain old translation / scaling values
		newrotmat.rows[0].set(cosy,0.0,siny,modelViewMatrix_.rows[0].w);
		newrotmat.rows[1].set((-sinx)*(-siny),cosx,(-sinx)*cosy,modelViewMatrix_.rows[1].w);
		newrotmat.rows[2].set(cosx*(-siny),sinx,cosx*cosy,modelViewMatrix_.rows[2].w);
		newrotmat.rows[3] = modelViewMatrix_.rows[3];

		// Reset translation and scaling on original matrix, and multiply
		modelViewMatrix_.rows[0].w = 0.0;
		modelViewMatrix_.rows[1].w = 0.0;
		modelViewMatrix_.rows[2].w = 0.0;
		modelViewMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
		modelViewMatrix_ *= newrotmat;
	}
	else parent_->rotateView(dx, dy);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::rotateView");
}

// Calculate and return inverse of current view matrix
Mat4<double> &Model::modelViewMatrixInverse()
{
	// Grab current modelview matrix
	modelViewMatrixInverse_ = (parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix());
	// Calculate inverse
	modelViewMatrixInverse_.invert();
	return modelViewMatrixInverse_;
}

// Set view to be along the specified cartesian axis
void Model::viewAlong(double x, double y, double z)
{
	msg.enter("Model::viewAlong");
	// Set model rotation matrix to be along the specified axis
	Vec3<double> v;
	v.set(x,y,z);
	v.toSpherical();
	// setRotation() expects the degrees of rotation about the x and y axes respectively
	setRotation(-v.y,fabs(v.z-180.0));
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::viewAlong");
}

// Set view to be along the specified cell axis
void Model::viewAlongCell(double x, double y, double z)
{
	msg.enter("Model::viewAlongCell");
	// Set model rotation matrix to be along the specified cell axis
	Vec3<double> v;
	v.set(x,y,z);
	v *= cell()->transpose();
	v.toSpherical();
	// setRotation() expects the degrees of rotation about the x and y axes respectively
	setRotation(-v.y,fabs(v.z-180.0));
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::viewAlongCell");
}

// Calculate and return drawing pixel width
double Model::drawPixelWidth(double drawdepth)
{
	// Get the Angstrom width of a single pixel at the current draw depth in the current view
	static Vec3<double> r;
	r = guideToModel(gui.mainWidget->width()/2+1, gui.mainWidget->height()/2, drawdepth) - guideToModel(gui.mainWidget->width()/2, gui.mainWidget->height()/2, drawdepth);
	return r.magnitude();
}

// Convert screen coordinates into modelspace coordinates
Vec3<double> Model::guideToModel(double sx, double sy, double drawdepth)
{
	// Convert the screen coordinates passed to a position on the drawing guide, and then into model coordinates
	msg.enter("Model::guideToModel");
	static Vec4<double> guidepoint;
	static Vec3<double> newpoint;
	static Mat4<double> rotmat;
	double radius;
	drawdepth = -modelViewMatrix_.rows[3].z + drawdepth;
//      printf("DEPTH = %f, cameraZ= %f\n",drawdepth, camera_.z);
	// First, project a point at the guide z-position into screen coordinates to get the guide 'yardstick'
	newpoint.set(modelViewMatrix_.rows[3].x, modelViewMatrix_.rows[3].y, drawdepth);
	//printf("newpoint1 "); newpoint.print();
	rotmat = modelViewMatrix_;
	rotmat.invert();
	newpoint *= rotmat;
	//printf("newpoint2 "); newpoint.print();
	gui.mainWidget->modelToWorld(newpoint, &guidepoint);
	//printf("guidepoint "); guidepoint.print();
	radius = guidepoint.w;
	// Now, calculate the position of the clicked point on the guide
	newpoint.x = sx - (gui.mainWidget->width() / 2.0 );
	newpoint.y = (gui.mainWidget->height() - sy) - (gui.mainWidget->height() / 2.0 );
	newpoint /= radius;
	newpoint.z = drawdepth + modelViewMatrix_.rows[3].z;
	// Convert this world coordinate into model coordinates by multiplying by the inverse of the PM matrix.
	newpoint *= modelViewMatrixInverse();
	// Also need to account for periodic systems (which are translated so the cell midpoint is centred in the screen) by adding the cell's centre coordinate
	newpoint += cell_.centre();
//      newpoint.print();
	msg.exit("Model::guideToModel");
	return newpoint;
}
