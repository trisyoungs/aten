/*
	*** Model view functions
	*** src/model/view.cpp
	Copyright T. Youngs 2007-2009

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

// Render from self
void Model::setRenderFromSelf()
{
	renderFromSelf_ = TRUE;
}

// Render from trajectory
void Model::setRenderFromFrames()
{
	renderFromSelf_ = FALSE;
}

// Return the current rendering source for the model
Model *Model::renderSource()
{
	return (renderFromSelf_ ? this : currentFrame_);
}

/*
// View Adjustment
*/

// Calculate View Matrix
void Model::calculateViewMatrix(bool recalcmatrix)
{
	if (recalcmatrix)
	{
		Vec3<double> f = cameraPosition_ - cameraTarget_;
		f.normalise();
		cameraUp_.normalise();
		Vec3<double> s, u;
		s = f * cameraUp_;
		u = s * f;
		viewMatrix_.set(0, -s.x, -s.y, -s.z, 0.0);
		viewMatrix_.set(1, u.x, u.y, u.z, 0.0);
		viewMatrix_.set(2, f.x, f.y, f.z, 0.0);
		viewMatrix_.set(3, 0.0, 0.0, 0.0, 1.0);
	}
	viewMatrixInverse_ = viewMatrix_;
	viewMatrixInverse_.invert();
}

// Return the GL-compatible array from the ModelMAT structure
void Model::copyViewMatrix(double *m)
{
	// If a trajectory frame, return the parent's matrix
	if (trajectoryParent_ == NULL) viewMatrix_.copyColumnMajor(m);
	else trajectoryParent_->viewMatrix_.copyColumnMajor(m);
}

// Set the current rotation matrix
void Model::setViewMatrix(Mat4<double> &rmat)
{
	if (trajectoryParent_ == NULL) viewMatrix_ = rmat;
	else trajectoryParent_->viewMatrix_ = rmat;
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
}

// Reset View
void Model::resetView()
{
	// Reset the modelview matrix and the camera
	msg.enter("Model::resetView");
	Vec3<double> newscreen;
	Atom *i, target;
	bool done = FALSE;
	double z, largest = 0.0;
	// Fit model to screen
	// Crude approach - find largest coordinate and zoom out so that {0,0,largest} is visible on screen
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		z = i->r().absMax();
		if (z > largest) largest = z;
	}
	target.r() = cell_.centre();
	target.r().add(0.0,0.0,cell_.lengths().z+largest);
	if (trajectoryParent_ == NULL)
	{
		cameraPosition_.set(0.0,0.0,10.0);
		cameraUp_.set(0.0,1.0,0.0);
		cameraTarget_.set(0.0,0.0,0.0);
	}
	else
	{
		trajectoryParent_->cameraPosition_.set(0.0,0.0,10.0);
		trajectoryParent_->cameraUp_.set(0.0,1.0,0.0);
		trajectoryParent_->cameraTarget_.set(0.0,0.0,0.0);
	}
	// Now, adjust camera matrix so that this atom is on-screen.
	// Need to do a check for the viability of the canvas first...
	if (gui.mainView.isValid() && (atoms_.nItems() != 0))
	{
		if (prefs.hasPerspective())
		{
			do
			{
				// Project our local atom and grab the z screen coordinate
				zoomCamera(1.0);
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
				zoomCamera(1.0);
				calculateViewMatrix();
				projectAtom(&target);
				done = TRUE;
				if ((target.rScreen().x < 0.0) || (target.rScreen().x > gui.mainView.width())) done = FALSE;
				if ((target.rScreen().y < 0.0) || (target.rScreen().y > gui.mainView.height())) done = FALSE;
				if (target.rScreen().z < 0.0) done = FALSE;
			} while (!done);
		}
	}
	// Recalculate viewing matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::resetView");
}

// Translate Camera
void Model::translateCamera(double dx, double dy)
{
	// Reposition model camera
	msg.enter("Model::translateCamera");
	Vec3<double> tvec(dx, dy, 0.0);
	if (trajectoryParent_ == NULL)
	{
		tvec *= viewMatrix_;
		cameraPosition_ += tvec;
		cameraTarget_ += tvec;
	}
	else
	{
		tvec *= trajectoryParent_->viewMatrix_;
		trajectoryParent_->cameraPosition_ += tvec;
		trajectoryParent_->cameraTarget_ += tvec;
	}
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::translateCamera");
}

// Rotate free
void Model::axisRotateView(Vec3<double> vec, double angle)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::axisRotateView");
	Mat4<double> newrotmat;
	// Generate quaternion
	newrotmat.createRotationAxis(vec.x, vec.y, vec.z, angle);
	// Rotate!
	if (trajectoryParent_ == NULL)
	{
		cameraPosition_ *= newrotmat;
		cameraUp_ *= newrotmat;
	}
	else
	{
		trajectoryParent_->cameraPosition_ *= newrotmat;
		trajectoryParent_->cameraUp_ *= newrotmat;
	}
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::axisRotateView");
}

// Rotate view using screen displacement as guide
void Model::rotateView(double screendx, double screendy)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::rotateView");
	if ((fabs(screendx)+fabs(screendy)) < 0.5)
	{
		msg.exit("Model::rotateView");
		return;
	}
	Mat4<double> newrotmat, othermat;
	if (trajectoryParent_ == NULL)
	{
		newrotmat.createRotationAxis(viewMatrix_.rows[1], -screendx);
		othermat.createRotationAxis(viewMatrix_.rows[0], -screendy);
		newrotmat *= othermat;
		cameraPosition_ *= newrotmat;
		cameraUp_ *= newrotmat;
	}
	else
	{
		newrotmat.createRotationAxis(trajectoryParent_->viewMatrix_.rows[1], screendx);
		othermat.createRotationAxis(trajectoryParent_->viewMatrix_.rows[0], screendy);
		newrotmat *= othermat;
		trajectoryParent_->cameraPosition_ *= newrotmat;
		trajectoryParent_->cameraUp_ *= newrotmat;
	}
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::rotateView");
}

// Spin the model about the z axis
void Model::zRotateView(double angle)
{
	// Rotate the whole system by the amounts specified.
	msg.enter("Model::zRotateView");
	if (angle < 0.001)
	{
		msg.exit("Model::zRotateView");
		return;
	}
	Mat4<double> newrotmat;
	if (trajectoryParent_ == NULL)
	{
		newrotmat.createRotationAxis(viewMatrix_.rows[2], angle);
// 		cameraPosition_ *= newrotmat;
		cameraUp_ *= newrotmat;
	}
	else
	{
		newrotmat.createRotationAxis(trajectoryParent_->viewMatrix_.rows[2], angle);
// 		trajectoryParent_->cameraPosition_ *= newrotmat;
		trajectoryParent_->cameraUp_ *= newrotmat;
	}
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::zRotateView");
}

// Adjust the camera zoom with Prefs-defined zoom throttle
void Model::zoomCameraThrottled(bool zoomin)
{
	msg.enter("Model::zoomCameraThrottled");
	if (trajectoryParent_ == NULL)
	{
		Vec3<double> viewvec = cameraTarget_ - cameraPosition_;
		viewvec *= prefs.zoomThrottle();
		setCameraPosition(cameraPosition_ + (zoomin ? viewvec : -viewvec));
	}
	else
	{
		Vec3<double> viewvec = trajectoryParent_->cameraTarget_ - trajectoryParent_->cameraPosition_;
		viewvec *= prefs.zoomThrottle();
		setCameraPosition(trajectoryParent_->cameraPosition_ + (zoomin ? viewvec : -viewvec));
	}
	gui.mainView.doProjection();
	msg.exit("Model::zoomCameraThrottled");
}

// Adjust the camera zoom by specified model-space distance
void Model::zoomCamera(double dz)
{
	msg.enter("Model::zoomCamera");
	if (trajectoryParent_ == NULL)
	{
		Vec3<double> viewvec = cameraTarget_ - cameraPosition_;
		double mag = viewvec.magAndNormalise();
		mag += dz;
		if (mag < 1.0) mag = 1.0;
	printf("zoom\n");
	cameraPosition_.print();
		setCameraPosition(cameraTarget_ - viewvec*mag);
	cameraPosition_.print();
	}
	else
	{
		Vec3<double> viewvec = trajectoryParent_->cameraTarget_ - trajectoryParent_->cameraPosition_;
		double mag = viewvec.magAndNormalise();
		mag += dz;
		if (mag < 1.0) mag = 1.0;
		setCameraPosition(cameraTarget_ - viewvec*mag);
	}
	gui.mainView.doProjection();
	msg.exit("Model::zoomCamera");
}

// Set view to be along the specified cartesian axis
void Model::viewAlong(double x, double y, double z)
{
	msg.enter("Model::viewAlong");
	// Set model rotation matrix to be along the specified axis
	cameraTarget_.zero();
	cameraPosition_.set(x,y,z);
	cameraUp_.set(0.0,1.0,0.0);
	calculateViewMatrix();
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
	cameraTarget_.zero();
	cameraPosition_ = v;
	cameraUp_.set(0.0,1.0,0.0);
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
	msg.exit("Model::viewAlongCell");
}

// Return the camera position
Vec3<double> Model::cameraPosition() const
{
	return (trajectoryParent_ == NULL ? cameraPosition_ : trajectoryParent_->cameraPosition_);
}

// Set the camera position
void Model::setCameraPosition(Vec3<double> v)
{
	if (trajectoryParent_ == NULL)
	{
		cameraPosition_ = v;
		// Never let camera get too close to target
		Vec3<double> viewvec = cameraPosition_ - cameraTarget_;
		if (viewvec.magAndNormalise() < 1.0) cameraPosition_ = cameraTarget_ + viewvec;
	}
	else
	{
		trajectoryParent_->cameraPosition_ = v;
		// Never let camera get too close to target
		Vec3<double> viewvec = trajectoryParent_->cameraPosition_ - trajectoryParent_->cameraTarget_;
		if (viewvec.magAndNormalise() < 1.0) trajectoryParent_->cameraPosition_ = trajectoryParent_->cameraTarget_ + viewvec;
	}
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the camera target
Vec3<double> Model::cameraTarget() const
{
	return (trajectoryParent_ == NULL ? cameraTarget_ : trajectoryParent_->cameraTarget_);
}

// Set the camera target
void Model::setCameraTarget(Vec3<double> v)
{
	trajectoryParent_ == NULL ? cameraTarget_ = v : trajectoryParent_->cameraTarget_ = v;
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the camera up
Vec3<double> Model::cameraUp() const
{
	return (trajectoryParent_ == NULL ? cameraUp_ : trajectoryParent_->cameraUp_);
}

// Set the camera up
void Model::setCameraUp(Vec3<double> v)
{
	if (trajectoryParent_ == NULL)
	{
		cameraUp_ = v;
		cameraUp_.normalise();
	}
	else
	{
		trajectoryParent_->cameraUp_ = v;
		trajectoryParent_->cameraUp_.normalise();
	}
	calculateViewMatrix();
	// Log camera change
	changeLog.add(Log::Camera);
}

// Return the camera view vector (position->target)
Vec3<double> Model::viewVector() const
{
	return (trajectoryParent_ == NULL ? cameraTarget_ - cameraPosition_ : trajectoryParent_->cameraTarget_ - trajectoryParent_->cameraPosition_);
}

/*
// Point Projection
*/

// Project the coordinates of all atoms in the model
void Model::projectAll()
{
	// Transform the model coordinates of all atoms into world GL and 2D screen coordinates
	msg.enter("Model::projectAll");
	if (projectionPoint_ != (changeLog.log(Log::Coordinates) + changeLog.log(Log::Camera) + changeLog.log(Log::Visual)))
	{
		if (gui.mainView.isValid()) for (Atom *i = atoms_.first(); i != NULL; i = i->next) projectAtom(i);
		projectionPoint_ = changeLog.log(Log::Coordinates) + changeLog.log(Log::Camera) + changeLog.log(Log::Visual);
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
	if (trajectoryParent_ == NULL) worldr = viewMatrix_ * modelr - cameraPosition_;
	else worldr = trajectoryParent_->viewMatrix_ * modelr - trajectoryParent_->cameraPosition_;
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

Vec4<double> &Model::worldToScreen(const Vec3<double> &v)
{
	// Project the supplied world coordinates into screen coordinates.
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
Vec3<double> Model::guideToModel(const Vec3<double> &v)
{
	return guideToModel(v.x, v.y);
}

Vec3<double> Model::guideToModel(double sx, double sy)
{
	// Convert the screen coordinates passed to a position on the drawing guide, and then into model coordinates
	msg.enter("Model::guideToModel");
	static Vec4<double> guidepoint;
	static Vec3<double> newpoint;
	static Mat4<double> rotmat;
	double radius, depth;
	depth = -cameraPosition_.z - prefs.drawDepth();
	printf("GUIDE TO MODEL IS BROKEN.\n");
	return newpoint;
	//printf("DEPTH = %f, DRAWDEPTH = %f\n",depth, prefs.drawDepth());
	// First, project a point at the guide z-position into screen coordinates to get the guide 'yardstick'
	newpoint.set(cameraPosition_.x, cameraPosition_.y, depth);
	//printf("newpoint1 "); newpoint.print();
// 	rotmat = rotationMatrix_;
// 	rotmat.invert();
// 	newpoint *= rotmat;
// 	//printf("newpoint2 "); newpoint.print();
// 	guidepoint = worldToScreen(newpoint);
// 	//printf("guidepoint "); guidepoint.print();
// 	radius = guidepoint.w;
// 	// Now, calculate the position of the clicked point on the guide
// 	newpoint.x = sx - (gui.mainView.width() / 2.0 );
// 	newpoint.y = (gui.mainView.height() - sy) - (gui.mainView.height() / 2.0 );
// 	newpoint /= radius;
// 	newpoint.z = -prefs.drawDepth();
// 	// Convert this world coordinate into model coordinates by multiplying by the inverse of the PM matrix.
// 	newpoint *= viewMatrixInverse_;
// 	// Also need to account for periodic systems (which are translated so the cell midpoint is centred in the screen) by adding the cell's centre coordinate
// 	newpoint += cell_.centre();
	msg.exit("Model::guideToModel");
	return newpoint;
}

// Calculate and return drawing pixel width
double Model::drawPixelWidth()
{
	// Get the Angstrom width of a single pixel at the current draw depth in the current view
	static Vec3<double> r;
	r = guideToModel(gui.mainView.width()/2+1, gui.mainView.height()/2) - guideToModel(gui.mainView.width()/2, gui.mainView.height()/2);
	return r.magnitude();
}
