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

// Set the current rotation matrix
void Model::setRotationMatrix(Mat4<double> &rmat)
{
	if (parent_ == NULL) rotationMatrix_ = rmat;
	else parent_->setRotationMatrix(rmat);
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the current rotation matrix
Mat4<double> Model::rotationMatrix() const
{
	return (parent_ == NULL ? rotationMatrix_ : parent_->rotationMatrix());
}

// Return the GL-compatible array from the ModelMAT structure
void Model::copyRotationMatrix(double *m)
{
	// If a trajectory frame, return the parent's matrix
	if (parent_ == NULL) rotationMatrix_.copyColumnMajor(m);
	else parent_->copyRotationMatrix(m);
}

// Return the current camera matrix
Mat4<double> Model::cameraMatrix() const
{
	return (parent_ == NULL ? cameraMatrix_ : parent_->cameraMatrix());
}

// Return the GL-compatible array from the ModelMAT structure
void Model::copyCameraMatrix(double *m)
{
	// If a trajectory frame, return the parent's matrix
	if (parent_ == NULL) cameraMatrix_.copyColumnMajor(m);
	else parent_->copyCameraMatrix(m);
}

// Set the camera z-rotation
void Model::setCameraRotation(double r)
{
	cameraRotation_ = r;
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the current camera z-rotation
double Model::cameraRotation() const
{
	return (parent_ == NULL ? cameraRotation_ : parent_->cameraRotation());
}

// Spin the model about the z axis
void Model::zRotateView(double angle)
{
	adjustCamera(0.0, 0.0, 0.0, angle / DEGRAD);
}

// Adjust the position of the camera
void Model::adjustCamera(const Vec3<double> &v, double r)
{
	adjustCamera(v.x,v.y,v.z,r);
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
		rotationMatrix_.rows[0].set(cosy,0.0,siny,0.0);
		rotationMatrix_.rows[1].set((-sinx)*(-siny),cosx,(-sinx)*cosy,0.0);
		rotationMatrix_.rows[2].set(cosx*(-siny),sinx,cosx*cosy,0.0);
		rotationMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
	}
	else parent_->setRotation(rotx, roty);
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::setRotation");
}

// Adjust Camera
void Model::adjustCamera(double dx, double dy, double dz, double angle)
{
	// Adjust the models camera variables
	msg.enter("Model::adjustCamera");
	double sincam, coscam;
	if (parent_ == NULL)
	{
		camera_.add(dx, -dy, dz);
		// Never let camera z go below -1.0...
		if (camera_.z > -1.0) camera_.z = -1.0;
		cameraRotation_ = cameraRotation_ + angle;
		if (cameraRotation_ > 2.0*PI) cameraRotation_ -= 2.0*PI;
		coscam = cos(cameraRotation_);
		sincam = sin(cameraRotation_);
		// Now create the new matrix
		cameraMatrix_.rows[0].set(coscam,-sincam,0.0,camera_.x*coscam-camera_.y*sincam);
		cameraMatrix_.rows[1].set(sincam,coscam,0.0,camera_.x*sincam-camera_.y*coscam);
		cameraMatrix_.rows[2].set(0.0,0.0,1.0,camera_.z);
		cameraMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
	}
	else parent_->adjustCamera(dx, dy, dz, angle);
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::adjustCamera");
}

// Adjust camera zoom
void Model::adjustZoom(bool zoomin)
{
	msg.enter("Model::adjustZoom");
	double dz = (parent_ == NULL ? -camera_.z : -parent_->camera().z);
	dz *= prefs.zoomThrottle();
	if (zoomin) dz = -dz;
	adjustCamera(0.0,0.0,dz,0.0);
	gui.mainView.doProjection();
	msg.exit("Model::adjustZoom");
}

// Reset Camera
void Model::resetCamera(const Vec3<double> &newr)
{
	// Adjust the models camera variables
	msg.enter("Model::resetCamera");
	// Reset current camera position
	camera_.zero();
	cameraRotation_ = 0.0;
	// Set specified position
	adjustCamera(newr.x, -newr.y, newr.z, 0.0);
	// Recalculate viewing matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::resetCamera");
}

// Reset View
void Model::resetView()
{
	// Reset the modelview matrix and the camera
	msg.enter("Model::resetView");
	static Vec3<double> newcam, newscreen;
	Atom *i, target;
	bool done = FALSE;
	double z, largest = 0.0;
	Mat4<double> dummy;
	parent_ == NULL ? rotationMatrix_.setIdentity() : parent_->setRotationMatrix( dummy );
	// Fit model to screen
	// Crude approach - find largest coordinate and zoom out so that {0,0,largest} is visible on screen
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		z = i->r().absMax();
		if (z > largest) largest = z;
	}
	target.r() = cell_.centre();
	target.r().add(0.0,0.0,cell_.lengths().z+largest);
	newcam.set(0.0,0.0,0.0);
	resetCamera(newcam);
	// Now, adjust camera matrix so that this atom is on-screen.
	// Need to do a check for the viability of the canvas first...
	if (gui.mainView.isValid() && (atoms_.nItems() != 0))
	{
		if (prefs.hasPerspective())
		{
			do
			{
				// Project our local atom and grab the z screen coordinate
				adjustCamera(0.0,0.0,-1.0,0.0);
				calculateViewMatrix();
				projectAtom(&target);
				z = target.rWorld().z;
			} while (z > -5.0);
		}
		else
		{
			do
			{
				// Project our local atom and grab the z screen coordinate
				adjustCamera(0.0,0.0,-1.0,0.0);
				calculateViewMatrix();
				projectAtom(&target);
				done = TRUE;
				if ((target.rScreen().x < 0.0) || (target.rScreen().x > gui.mainView.width())) done = FALSE;
				if ((target.rScreen().y < 0.0) || (target.rScreen().y > gui.mainView.height())) done = FALSE;
				if (target.rScreen().z < 0.0) done = FALSE;
			} while (!done);
		}
	}
	else
	{
		newcam.set(0.0,0.0,-10.0);
		resetCamera(newcam);
	}
	// Recalculate viewing matrix
	calculateViewMatrix();
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
		camrot = cameraRotation_;
		camrot > PI ? theta = camrot-2.0*PI : theta = camrot;
		// Account for the orientation of the current camera up vector.
	// 	rotx = (dx*sin(theta) + dy*cos(theta) ) / DEGRAD;
	// 	roty = (dx*cos(theta) - dy*sin(theta) ) / DEGRAD;
		// Generate quaternion
		newrotmat.createRotationAxis(vec.x, vec.y, vec.z, angle);
		oldrotmat = rotationMatrix_;
		// Now, multiply our matrices together...
		rotationMatrix_ = newrotmat * oldrotmat;
	}
	else parent_->axisRotateView(vec, angle);
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::axisRotateView");
}

// Rotate free
void Model::rotateView(double dx, double dy)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::rotateView");
	static double rotx, roty, theta, sinx, cosx, siny, cosy, camrot;
	static Mat4<double> newrotmat, oldrotmat;
	if (parent_ == NULL)
	{
		camrot = cameraRotation_;
		camrot > PI ? theta = camrot-2.0*PI : theta = camrot;
		// Account for the orientation of the current camera up vector.
		rotx = (dx*sin(theta) + dy*cos(theta) ) / DEGRAD;
		roty = (dx*cos(theta) - dy*sin(theta) ) / DEGRAD;
		// Calculate cos/sin terms for needless speedup!
		cosx = cos(rotx);
		cosy = cos(roty);
		sinx = sin(rotx);
		siny = sin(roty);
		newrotmat.rows[0].set(cosy,0.0,siny,0.0);
		newrotmat.rows[1].set((-sinx)*(-siny),cosx,(-sinx)*cosy,0.0);
		newrotmat.rows[2].set(cosx*(-siny),sinx,cosx*cosy,0.0);
		newrotmat.rows[3].set(0.0,0.0,0.0,1.0);
		oldrotmat = rotationMatrix_;
		// Now, multiply our matrices together...
		rotationMatrix_ = newrotmat * oldrotmat;
	}
	else parent_->rotateView(dx, dy);
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::rotateView");
}

// Calculate View Matrix
void Model::calculateViewMatrix()
{
	// Calculate full viewing matrix
	if (parent_ == NULL) viewMatrix_ = cameraMatrix_ * rotationMatrix_;
	else viewMatrix_ = parent_->cameraMatrix() * parent_->rotationMatrix();
	// Calculate inverse
	viewMatrixInverse_ = viewMatrix_;
	viewMatrixInverse_.invert();
}

// Project the coordinates of all atoms in the model
void Model::projectAll()
{
	// Transform the model coordinates of all atoms into world GL and 2D screen coordinates
	msg.enter("Model::projectAll");
	if (gui.mainView.isValid())
	{
		if (projectionPoint_ != (changeLog.log(Log::Coordinates) + changeLog.log(Log::Camera) + changeLog.log(Log::Visual)))
		{
			if (gui.mainView.isValid()) for (Atom *i = atoms_.first(); i != NULL; i = i->next) projectAtom(i);
			projectionPoint_ = changeLog.log(Log::Coordinates) + changeLog.log(Log::Camera) + changeLog.log(Log::Visual);
		}
	}
	msg.exit("Model::projectAll");
}

// Project the coordinates of all selected atoms in the model
void Model::projectSelection()
{
	msg.enter("Model::projectSelection");
	if (gui.mainView.isValid()) for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) projectAtom(i);
	msg.exit("Model::projectSelection");
}

// Project the coordinates of a single atom in the model
void Model::projectAtom(Atom *i)
{
	// Transform the model coordinates of specified atom into world GL and 2D screen coordinates
	if (!gui.mainView.isValid()) return;
	static Vec4<double> modelr, screenr, worldr;
	static double srx, sry, srz;
	static GLint *vmat;
	// Projection formula is : worldr = P x M x modelr 
	modelr.set(i->r(), 1.0);
	// We also need to subtract the cell centre coordinate
	modelr -= cell_.centre();
	// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
	worldr = viewMatrix_ * modelr;
	i->rWorld().set(worldr.x, worldr.y, worldr.z);
	// Calculate 2D screen coordinates - Multiply world coordinates by P
	screenr = gui.mainView.PMAT * worldr;
	screenr.x /= screenr.w;
	screenr.y /= screenr.w;
	srz = screenr.z / screenr.w;
	vmat = gui.mainView.VMAT;
	srx = vmat[0] + vmat[2]*(screenr.x+1)*0.5;
	sry = vmat[1] + vmat[3]*(screenr.y+1)*0.5;
	i->rScreen().set(srx,sry,srz);
	// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
	worldr.x += prefs.screenRadius(i);
	screenr = gui.mainView.PMAT * worldr;
	screenr.x /= screenr.w;
	i->setScreenRadius(fabs( (vmat[0] + vmat[2]*(screenr.x+1)*0.5) - srx));
}

// Project given model coordinates into screen coordinates
Vec3<double> &Model::modelToScreen(Vec3<double> &pos)
{
	msg.enter("Model::modelToScreen");
	static Vec4<double> modelr, screenr, worldr;
	static Vec3<double> result;
	if (!gui.mainView.isValid())
	{
		msg.exit("Model::modelToScreen");
		result.zero();
		return result;
	}
	static GLint *vmat;
	// Projection formula is : worldr = P x M x modelr 
	modelr.set(pos, 1.0);
	// We also need to subtract the cell centre coordinate
	modelr -= cell_.centre();
	// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
	worldr = viewMatrix_ * modelr;
	// Calculate 2D screen coordinates - Multiply world coordinates by P
	screenr = gui.mainView.PMAT * worldr;
	screenr.x /= screenr.w;
	screenr.y /= screenr.w;
	vmat = gui.mainView.VMAT;
	result.set( vmat[0] + vmat[2]*(screenr.x+1)*0.5, vmat[1] + vmat[3]*(screenr.y+1)*0.5, screenr.z / screenr.w);
	msg.exit("Model::modelToScreen");
	return result;
}

// Project the supplied world coordinates into screen coordinates.
Vec4<double> &Model::worldToScreen(const Vec3<double> &v)
{
	// The returned vec4's 'w' component is the unit 'radius' at that point.
	msg.enter("Model::worldToScreen");
	static Vec4<double> modelr, screenr, worldr, result;
	static double x1,x2,radius;
	static GLint *vmat;
	screenr.zero();
	if (!gui.mainView.isValid() )
	{
		msg.exit("Model::worldToScreen");
		return screenr;
	}
	calculateViewMatrix();
	// Projection formula is : worldr = P x M x modelr
	// Get the 3D coordinates of the atom - Multiply by modelview matrix 'view'
	modelr.set(v.x, v.y, v.z, 1.0);
	worldr = viewMatrix_ * modelr;
	//viewMatrix_.print();
	// Calculate 2D 'radius' of the atom - Multiply worldr[x+delta] coordinates by P
	screenr = gui.mainView.PMAT * worldr;
	screenr.x /= screenr.w;
	screenr.y /= screenr.w;
	result = screenr;
	vmat = gui.mainView.VMAT;
	x1 = vmat[0] + vmat[2]*(screenr.x+1)/2.0;
	worldr.x += 1.0;
	screenr = gui.mainView.PMAT * worldr;
	screenr.x /= screenr.w;
	x2 = vmat[0] + vmat[2]*(screenr.x+1)/2.0;
	radius = fabs(x2 - x1);
	// Store info and return
	result.w = radius;
	msg.exit("Model::worldToScreen");
	return result;
}

// Convert screen coordinates into modelspace coordinates
Vec3<double> Model::guideToModel(const Vec3<double> &v, double drawdepth)
{
	return guideToModel(v.x, v.y, drawdepth);
}
Vec3<double> Model::guideToModel(double sx, double sy, double drawdepth)
{
	// Convert the screen coordinates passed to a position on the drawing guide, and then into model coordinates
	msg.enter("Model::guideToModel");
	static Vec4<double> guidepoint;
	static Vec3<double> newpoint;
	static Mat4<double> rotmat;
	double radius;
	drawdepth = -camera_.z + drawdepth;
// 	printf("DEPTH = %f, cameraZ= %f\n",drawdepth, camera_.z);
	// First, project a point at the guide z-position into screen coordinates to get the guide 'yardstick'
	newpoint.set(camera_.x, camera_.y, drawdepth);
	//printf("newpoint1 "); newpoint.print();
	rotmat = rotationMatrix_;
	rotmat.invert();
	newpoint *= rotmat;
	//printf("newpoint2 "); newpoint.print();
	guidepoint = worldToScreen(newpoint);
	//printf("guidepoint "); guidepoint.print();
	radius = guidepoint.w;
	// Now, calculate the position of the clicked point on the guide
	newpoint.x = sx - (gui.mainView.width() / 2.0 );
	newpoint.y = (gui.mainView.height() - sy) - (gui.mainView.height() / 2.0 );
	newpoint /= radius;
	newpoint.z = drawdepth + camera_.z;
	// Convert this world coordinate into model coordinates by multiplying by the inverse of the PM matrix.
	newpoint *= viewMatrixInverse_;
	// Also need to account for periodic systems (which are translated so the cell midpoint is centred in the screen) by adding the cell's centre coordinate
	newpoint += cell_.centre();
// 	newpoint.print();
	msg.exit("Model::guideToModel");
	return newpoint;
}

// Return the camera position vector
Vec3<double> Model::camera() const
{
	return (parent_ == NULL ? camera_ : parent_->camera());
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
	r = guideToModel(gui.mainView.width()/2+1, gui.mainView.height()/2, drawdepth) - guideToModel(gui.mainView.width()/2, gui.mainView.height()/2, drawdepth);
	return r.magnitude();
}
