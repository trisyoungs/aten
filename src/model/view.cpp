/*
	*** Model view functions
	*** src/model/view.cpp
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

#include "model/model.h"

ATEN_USING_NAMESPACE

// Calculate and return inverse of current view matrix
Matrix& Model::modelViewMatrixInverse()
{
	// Grab current modelview matrix
	modelViewMatrixInverse_ = (parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix());

	// Calculate inverse
	modelViewMatrixInverse_.invert();
	return modelViewMatrixInverse_;
}

// Return the current modelview matrix (local, parent, or common)
const Matrix Model::modelViewMatrix()
{
	if (prefs.viewLock() == Prefs::FullLock) return prefs.commonViewMatrix();
	else return (parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix());
}

// Set the current modelview matrix
void Model::setModelViewMatrix(Matrix matrix)
{
	if (prefs.viewLock() == Prefs::FullLock) prefs.setCommonViewMatrix(matrix);
	else if (parent_ == NULL) modelViewMatrix_ = matrix;
	else parent_->setModelViewMatrix(matrix);
}

// Return the viewportMatrix
GLint *Model::viewportMatrix()
{
	return (parent_ == NULL ? viewportMatrix_ : parent_->viewportMatrix());
}

// Return current projection matrix
Matrix& Model::modelProjectionMatrix()
{
	return (parent_ == NULL ? modelProjectionMatrix_ : parent_->modelProjectionMatrix());
}

// Set view to be along the specified cartesian axis
void Model::viewAlong(double x, double y, double z)
{
	Messenger::enter("Model::viewAlong");
	// Set model rotation matrix to be along the specified axis
	Vec3<double> v;
	v.set(x,y,z);
	v.toSpherical();
	// setRotation() expects the degrees of rotation about the x and y axes respectively
	setRotation(-v.y,fabs(v.z-180.0));
	Messenger::exit("Model::viewAlong");
}

// Set view to be along the specified cell axis
void Model::viewAlongCell(double x, double y, double z)
{
	Messenger::enter("Model::viewAlongCell");
	// Set model rotation matrix to be along the specified cell axis
	Vec3<double> v;
	v.set(x,y,z);
	v = cell().axes() * v;
	v.toSpherical();
	// setRotation() expects the degrees of rotation about the x and y axes respectively
	setRotation(-v.y,fabs(v.z-180.0));
	Messenger::exit("Model::viewAlongCell");
}

// Rotate free
void Model::axisRotateView(Vec3<double> vec, double angle)
{
	// Rotate the whole system by the amounts specified.
	Messenger::enter("Model::axisRotateView");
	if (parent_ == NULL)
	{
		Matrix newrotmat, oldrotmat = modelViewMatrix();

		// Generate quaternion
		newrotmat.createRotationAxis(vec.x, vec.y, vec.z, angle, true);

		// Now, multiply our matrices together...
		setModelViewMatrix(newrotmat * oldrotmat);
	}
	else parent_->axisRotateView(vec, angle);
	Messenger::exit("Model::axisRotateView");
}

// Set exact rotation of model (angles passed in degrees)
void Model::setRotation(double rotx, double roty)
{
	Messenger::enter("Model::setRotation");
	if (parent_ == NULL)
	{
		// Store old translation and scaling values
		Matrix temp;
		temp.createRotationXY(rotx, roty);
		temp.copyTranslationAndScaling(modelViewMatrix());
		setModelViewMatrix(temp);
	}
	else parent_->setRotation(rotx, roty);
	Messenger::exit("Model::setRotation");
}

// Rotate free
void Model::rotateView(double dx, double dy)
{
	// Rotate the whole system by the amounts specified.
	Messenger::enter("Model::rotateView");
	double rotx, roty;
	if (parent_ == NULL)
	{
		Matrix newrotmat, oldrotmat = modelViewMatrix();

		// Create rotation matrix
		rotx = dy;
		roty = dx;
		newrotmat.createRotationXY(rotx, roty);
		newrotmat.copyTranslationAndScaling(oldrotmat);

		// Reset translation and scaling on original matrix, and multiply
		oldrotmat.removeTranslationAndScaling();
		setModelViewMatrix(newrotmat * oldrotmat);   // TEST Create preMultiply(Matrix&) method.
	}
	else parent_->rotateView(dx, dy);
	Messenger::exit("Model::rotateView");
}

// Spin the model about the z axis
void Model::zRotateView(double dz)
{
	// Rotate the whole system by the amounts specified.
	Messenger::enter("Model::zRotateView");
	if (parent_ == NULL)
	{
		Matrix newrotmat, oldrotmat = modelViewMatrix();
		newrotmat.createRotationZ(dz);
		newrotmat.copyTranslationAndScaling(oldrotmat);

		// Reset translation and scaling on original matrix, and multiply
		oldrotmat.removeTranslationAndScaling();
		setModelViewMatrix(newrotmat * oldrotmat);
	}
	else parent_->zRotateView(dz);
	Messenger::exit("Model::zRotateView");
}

// Adjust Camera
void Model::adjustCamera(double dx, double dy, double dz)
{
	// Adjust the models camera variables
	Messenger::enter("Model::adjustCamera");
	if (parent_ == NULL)
	{
		Matrix newmatrix = modelViewMatrix();
		newmatrix.adjustColumn(3, dx, -dy, dz, 0.0);

		// Never let camera z go below -1.0...
		if (newmatrix[14] > -1.0) newmatrix[14] = -1.0;

		setModelViewMatrix(newmatrix);
	}
	else parent_->adjustCamera(dx, dy, dz);
	Messenger::exit("Model::adjustCamera");
}

// Adjust camera zoom
void Model::adjustZoom(bool zoomin)
{
	Messenger::enter("Model::adjustZoom");

	double dz = (parent_ == NULL ? -modelViewMatrix().element(14) : -parent_->modelViewMatrix().element(14));
	dz *= prefs.zoomThrottle();
	if (zoomin) dz = -dz;
	adjustCamera(0.0,0.0,dz);
	Messenger::exit("Model::adjustZoom");
}

// Reset View
void Model::resetView(int contextWidth, int contextHeight)
{
	// Reset the modelview matrix and the camera
	Messenger::enter("Model::resetView");
	Vec3<double> extremes, rabs, target;
	Vec4<double> screenr;
	bool done = false;
	double rad;
	Matrix mview = modelViewMatrix();
	mview.setIdentity();
	mview.setColumn(3,0.0,0.0,0.0,1.0);

	// Fit model to screen
	// Crude approach - find largest coordinate and zoom out so that {0,0,largest} is visible on screen
	for (Atom* i = atoms_.first(); i != NULL; i = i->next)
	{
		rad = styleRadius(i->style(), i->element());
		rabs = i->r().abs();
		if (rabs.x > extremes.x) extremes.x = rabs.x + rad;
		if (rabs.y > extremes.y) extremes.y = rabs.y + rad;
		if (i->r().z > extremes.z) extremes.z = rabs.z;
	}
	target = (cell_.lengths() * 0.5) + extremes;

	// Now, adjust camera matrix so that this atom is on-screen.
	setModelViewMatrix(mview);
	setupView(0, 0, contextWidth, contextHeight);

	// Need to do a check for the viability of the canvas first...
	if (atoms_.nItems() > 0)
	{
		do
		{
			// Adjust z-translation by 1 Angstrom. Orthographic view needs the projection matrix to be recreated every time
			mview[14] -= 1.0;
			setModelViewMatrix(mview);
			if (!prefs.hasPerspective()) setupView(0, 0, contextWidth, contextHeight);

			// Project our local atom and grab the z screen coordinate
			modelToWorld(target, &screenr);
			done = true;
			if ((screenr.x < 0.0) || (screenr.x > contextWidth)) done = false;
			if ((screenr.y < 0.0) || (screenr.y > contextHeight)) done = false;
			if (screenr.z < 0.0) done = false;
		} while (!done);
	}
	else mview.setColumn(3, 0.0, 0.0, -10.0, 1.0);

	setModelViewMatrix(mview);

	Messenger::exit("Model::resetView");
}

// Set-up viewport and projection matrices
void Model::setupView(GLint x, GLint y, GLint w, GLint h)
{
	if (parent_ == NULL)
	{
		// Setup and store viewport matrix
		viewportMatrix_[0] = x;
		viewportMatrix_[1] = y;
		viewportMatrix_[2] = w;
		viewportMatrix_[3] = h;
		
		// Create projection matrix for model
		GLdouble top, bottom, right, left, aspect = (GLdouble) w / (GLdouble) h;
		if (prefs.hasPerspective())
		{
			// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
			top = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
			bottom = -top;
			left = -aspect*top;
			right = aspect*top;
			modelProjectionMatrix_.setColumn(0, (prefs.clipNear()*2.0) / (right-left), 0.0, 0.0, 0.0);
			modelProjectionMatrix_.setColumn(1, 0.0, (prefs.clipNear()*2.0) / (top-bottom), 0.0, 0.0);
			modelProjectionMatrix_.setColumn(2, (right+left)/(right-left), (top+bottom)/(top-bottom), -(prefs.clipFar()+prefs.clipNear())/(prefs.clipFar()-prefs.clipNear()), -1.0);
			modelProjectionMatrix_.setColumn(3, 0.0, 0.0, -(2.0*prefs.clipNear()*prefs.clipFar()) / (prefs.clipFar()-prefs.clipNear()), 0.0);
			// Equivalent to the following code:
			// glMatrixMode(GL_PROJECTION);
			// glLoadIdentity();
			// top = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
			// bottom = -top;
			// glFrustum(aspect*bottom, aspect*top, bottom, top, prefs.clipNear(), prefs.clipFar());
			// glGetDoublev(GL_PROJECTION_MATRIX, modelProjectionMatrix_.matrix());
		}
		else
		{
			top = -tan(prefs.perspectiveFov() / DEGRAD) * modelViewMatrix_[14];
			bottom = -top;
			left = -aspect*top;
			right = aspect*top;

			modelProjectionMatrix_.setColumn(0, 2.0 / (right-left), 0.0, 0.0, (right+left)/(right-left));
			modelProjectionMatrix_.setColumn(1, 0.0, 2.0 / (top-bottom), 0.0, (top+bottom)/(top-bottom));
			modelProjectionMatrix_.setColumn(2, 0.0, 0.0, -2.0/(prefs.clipFar()+prefs.clipFar()), 0.0);
			modelProjectionMatrix_.setColumn(3, 0.0, 0.0, 0.0, 1.0);
			// Equivalent to the following code:
			// glMatrixMode(GL_PROJECTION);
			// glLoadIdentity();
			// top = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
			// bottom = -top;
			// glOrtho(aspect*top, aspect*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
			// glGetDoublev(GL_PROJECTION_MATRIX, modelProjectionMatrix_.matrix());
		}
	}
	else parent_->setupView(x, y, w, h);
}

// Project given model coordinates into world coordinates (and screen coordinates if requested)
Vec3<double>& Model::modelToWorld(Vec3<double>& modelr, Vec4<double>* screenr, double screenradius)
{
	Messenger::enter("Model::modelToWorld");
	static Vec3<double> worldr;
	static Matrix vmat;
	Vec4<double> pos, temp, tempscreen;

	// Projection formula is : worldr = P x M x modelr
	pos.set(modelr, 1.0);

	// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
	vmat = modelViewMatrix();
	vmat.applyTranslation(-cell_.centre().x, -cell_.centre().y, -cell_.centre().z);
	temp = vmat * pos;
	worldr.set(temp.x, temp.y, temp.z);

	// Calculate 2D screen coordinates - Multiply world coordinates by P
	if (screenr != NULL)
	{
		*screenr = modelProjectionMatrix() * temp;
		screenr->x /= screenr->w;
		screenr->y /= screenr->w;
		screenr->x = viewportMatrix()[0] + viewportMatrix()[2]*(screenr->x+1)*0.5;
		screenr->y = viewportMatrix()[1] + viewportMatrix()[3]*(screenr->y+1)*0.5;
		screenr->z = screenr->z / screenr->w;
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		if (screenradius > 0.0)
		{
			temp.x += screenradius;
			tempscreen = modelProjectionMatrix() * temp;
			tempscreen.x /= tempscreen.w;
			screenr->w = fabs( (viewportMatrix()[0] + viewportMatrix()[2]*(tempscreen.x+1)*0.5) - screenr->x);
		}
	}

	Messenger::exit("Model::modelToWorld");
	return worldr;
}

// Convert screen coordinates into modelspace coordinates
Vec3<double>& Model::screenToModel(int x, int y, double z)
{
	Messenger::enter("Model::screenToModel");
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
	modelr = itransform * Vec3<double>(worldr.x, worldr.y, worldr.z);
	
	Messenger::exit("Model::screenToModel");
	return modelr;
}

// Set positive repeat cell value
void Model::setRepeatCellsPositive(int i, int r)
{
	repeatCellsPositive_.set(i,r);
}

// Get positive repeat cell value
Vec3<int> Model::repeatCellsPositive() const
{
	return repeatCellsPositive_;
}

// Set negative repeat cell value
void Model::setRepeatCellsNegative(int i, int r)
{
	repeatCellsNegative_.set(i,r);
}

// Get negative repeat cell value
Vec3<int> Model::repeatCellsNegative() const
{
	return repeatCellsNegative_;
}

void Model::setCommonViewMatrixFromLocal()
{
	prefs.setCommonViewMatrix(parent_ == NULL ? modelViewMatrix_ : parent_->modelViewMatrix());
}

