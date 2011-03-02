/*
	*** Model view functions
	*** src/model/view.cpp
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

#include "model/model.h"
#include "gui/tcanvas.uih"
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
void Model::setModelViewMatrix(Matrix &rmat)
{
	if (parent_ == NULL) modelViewMatrix_ = rmat;
	else parent_->setModelViewMatrix(rmat);
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the current modelview matrix
Matrix &Model::modelViewMatrix()
{
	return (parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix());
}

// Return the viewportMatrix
GLint *Model::viewportMatrix()
{
	return (parent_ == NULL ? viewportMatrix_ : parent_->viewportMatrix());
}

// Return current projection matrix
Matrix &Model::modelProjectionMatrix()
{
	return (parent_ == NULL ? modelProjectionMatrix_ : parent_->modelProjectionMatrix());
}

// Return current globe projection matrix
Matrix &Model::globeProjectionMatrix()
{
	return (parent_ == NULL ? globeProjectionMatrix_ : parent_->globeProjectionMatrix());
}

// Spin the model about the z axis
void Model::zRotateView(double dz)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::zRotateView");
	Matrix newrotmat, oldrotmat;
	if (parent_ == NULL)
	{
		newrotmat.createRotationZ(dz);
		newrotmat.copyTranslationAndScaling(modelViewMatrix_);

		// Reset translation and scaling on original matrix, and multiply
		modelViewMatrix_.removeTranslationAndScaling();
		modelViewMatrix_ = newrotmat * modelViewMatrix_;
	}
	else parent_->zRotateView(dz);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::zRotateView");
}

// Adjust Camera
void Model::adjustCamera(double dx, double dy, double dz)
{
	// Adjust the models camera variables
	msg.enter("Model::adjustCamera");
	if (parent_ == NULL)
	{
		modelViewMatrix_.adjustColumn(3, dx, -dy, dz, 0.0);
		// Never let camera z go below -1.0...
		if (modelViewMatrix_[14] > -1.0) modelViewMatrix_[14] = -1.0;
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
	double dz = (parent_ == NULL ? -modelViewMatrix_[14] : -parent_->modelViewMatrix()[14]);
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
	Vec3<double> extremes, rabs, target;
	Vec4<double> screenr;
	bool done = FALSE;
	double rad;
	Matrix &mview = (parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix_);
	mview.setIdentity();
	mview.setColumn(3,0.0,0.0,0.0,1.0);
	// Fit model to screen
	// Crude approach - find largest coordinate and zoom out so that {0,0,largest} is visible on screen
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		rad = prefs.styleRadius(i);
		rabs = i->r().abs();
		if (rabs.x > extremes.x) extremes.x = rabs.x + rad;
		if (rabs.y > extremes.y) extremes.y = rabs.y + rad;
		if (i->r().z > extremes.z) extremes.z = rabs.z;
	}
	target = (cell_.lengths() * 0.5) + extremes;

	// Now, adjust camera matrix so that this atom is on-screen.
	// Need to do a check for the viability of the canvas first...
	if (gui.mainWidget->isValid() && (atoms_.nItems() != 0))
	{
		// TODO Resetting orthographic view is broken, since modelProjectionMatrix_ (in engine_) is not updated
		// after changing zoom factor (relevant call to glOrtho)
		do
		{
			// Adjust z-translation by 1 Angstrom
			mview[14] -= 1.0;
			// Project our local atom and grab the z screen coordinate
			if (!prefs.hasPerspective()) gui.mainWidget->doProjection();// TGAY Necessary?
			setupView(0, 0, gui.mainWidget->contextWidth(), gui.mainWidget->contextHeight());
// 			gui.mainWidget->updateTransformation(mview, cell_.centre());
			modelToWorld(target, &screenr);
			done = TRUE;
			if ((screenr.x < 0.0) || (screenr.x > gui.mainWidget->width())) done = FALSE;
			if ((screenr.y < 0.0) || (screenr.y > gui.mainWidget->height())) done = FALSE;
			if (screenr.z < 0.0) done = FALSE;
		} while (!done);
	}
	else mview.setColumn(3, 0.0, 0.0, -10.0, 1.0);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::resetView");
}

// Rotate free
void Model::axisRotateView(Vec3<double> vec, double angle)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::axisRotateView");
	Matrix newrotmat, oldrotmat;
	if (parent_ == NULL)
	{
		// Generate quaternion
		newrotmat.createRotationAxis(vec.x, vec.y, vec.z, angle, TRUE);
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
	msg.enter("Model::setRotation");
	Matrix temp;
	if (parent_ == NULL)
	{
		// Store old translation and scaling values
		temp.copyTranslationAndScaling(modelViewMatrix_);
		modelViewMatrix_.createRotationXY(rotx, roty);
		modelViewMatrix_.copyTranslationAndScaling(temp);
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
	double rotx, roty;
	Matrix newrotmat, oldrotmat;
	if (parent_ == NULL)
	{
		// Create rotation matrix
		rotx = dy;
		roty = dx;
		newrotmat.createRotationXY(rotx, roty);
		newrotmat.copyTranslationAndScaling(modelViewMatrix_);

		// Reset translation and scaling on original matrix, and multiply
		modelViewMatrix_.removeTranslationAndScaling();
		modelViewMatrix_ = newrotmat * modelViewMatrix_;
	}
	else parent_->rotateView(dx, dy);
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::rotateView");
}

// Calculate and return inverse of current view matrix
Matrix &Model::modelViewMatrixInverse()
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
	v = cell()->axes() * v;
	v.toSpherical();
	// setRotation() expects the degrees of rotation about the x and y axes respectively
	setRotation(-v.y,fabs(v.z-180.0));
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::viewAlongCell");
}


// Set-up viewport and projection matrices
void Model::setupView(GLint x, GLint y, GLint w, GLint h)
{
	// Setup and store viewport matrix
	viewportMatrix_[0] = x;
	viewportMatrix_[1] = y;
	viewportMatrix_[2] = w;
	viewportMatrix_[3] = h;
	glViewport( x, y, w, h );
	glMatrixMode(GL_PROJECTION);
	
	// Create projection matrix for rotation globe
	glLoadIdentity();
	glOrtho(-1.0, 1.0, -1.0, 1.0, -10.0, 10.0);
	glGetDoublev(GL_PROJECTION_MATRIX, globeProjectionMatrix_.matrix());
	
	// Create projection matrix for model
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
		top = tan(prefs.perspectiveFov() / DEGRAD) * modelViewMatrix_[14];
		bottom = -top;
		glOrtho(aspect*top, aspect*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
	}
	glGetDoublev(GL_PROJECTION_MATRIX, modelProjectionMatrix_.matrix());
	glMatrixMode(GL_MODELVIEW);
}

// Project given model coordinates into world coordinates (and screen coordinates if requested)
Vec3<double> &Model::modelToWorld(Vec3<double> &modelr, Vec4<double> *screenr, double screenradius)
{
	msg.enter("Model::modelToWorld");
	static Vec3<double> worldr;
	Vec4<double> pos, temp, tempscreen;
	// Projection formula is : worldr = P x M x modelr
	pos.set(modelr, 1.0);
	// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
	temp = modelViewMatrix() * pos;
	worldr.set(temp.x, temp.y, temp.z);
	// Calculate 2D screen coordinates - Multiply world coordinates by P
	if (screenr != NULL)
	{
		*screenr = modelProjectionMatrix_ * temp;
		screenr->x /= screenr->w;
		screenr->y /= screenr->w;
		screenr->x = viewportMatrix_[0] + viewportMatrix_[2]*(screenr->x+1)*0.5;
		screenr->y = viewportMatrix_[1] + viewportMatrix_[3]*(screenr->y+1)*0.5;
		screenr->z = screenr->z / screenr->w;
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		if (screenradius > 0.0)
		{
			temp.x += screenradius;
			tempscreen = modelProjectionMatrix_ * temp;
			tempscreen.x /= tempscreen.w;
			screenr->w = fabs( (viewportMatrix_[0] + viewportMatrix_[2]*(tempscreen.x+1)*0.5) - screenr->x);
		}
	}
	msg.exit("Model::modelToWorld");
	return worldr;
}

// Convert screen coordinates into modelspace coordinates
Vec3<double> &Model::screenToModel(int x, int y, double z)
{
	msg.enter("Model::screenToModel");
	static Vec3<double> modelr;
	Vec4<double> temp, worldr;
	int newx, newy;
	double dx, dy;
	
	// Grab transformation matrix, apply cell centre correction, and invert
	Matrix itransform = modelViewMatrix_;
	itransform.applyTranslation(-cell_.centre().x, -cell_.centre().y, -cell_.centre().z);
	itransform.invert();
	
	// Mirror y-coordinate
	y = viewportMatrix()[3] - y;

	// Project points at guide z-position and two other points along literal x and y to get scaling factors for screen coordinates
	worldr.set(0.0,0.0,z, 1.0);
	temp = modelProjectionMatrix() * worldr;
	newx = viewportMatrix()[0] + viewportMatrix()[2]*(temp.x / temp.w + 1.0)*0.5;
	newy = viewportMatrix()[1] + viewportMatrix()[3]*(temp.y / temp.w + 1.0)*0.5;
		
	for (int n=0; n<10; ++n)
	{
		// Determine new (better) coordinate from a yardstick centred at current world coordinates
		temp = modelProjectionMatrix() * Vec4<double>(worldr.x+1.0, worldr.y+1.0, worldr.z, worldr.w);
		dx = viewportMatrix()[0] + viewportMatrix()[2]*(temp.x / temp.w + 1.0)*0.5 - newx;
		dy = viewportMatrix()[1] + viewportMatrix()[3]*(temp.y / temp.w + 1.0)*0.5 - newy;

		worldr.add((x-newx)/dx, (y-newy)/dy, 0.0, 0.0);
// 		printf ("N=%i", n); worldr.print();
		temp = modelProjectionMatrix() * worldr;
		newx = viewportMatrix()[0] + viewportMatrix()[2]*(temp.x / temp.w + 1.0)*0.5;
		newy = viewportMatrix()[1] + viewportMatrix()[3]*(temp.y / temp.w + 1.0)*0.5;
// 		printf("NEW dx = %f, dy = %f, wantedxy = %f, %f\n", newx, newy, x, y);
		if ((x == newx) && (y == newy)) break;
	}
	
	// Finally, invert to model coordinates
	modelr = modelViewMatrixInverse() * Vec3<double>(worldr.x, worldr.y, worldr.z);
	
	msg.exit("Model::screenToModel");
	return modelr;
}
