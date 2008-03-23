/*
	*** Model view functions
	*** src/model/view.cpp
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

// Convert screen coordinates into modelspace coordinates
Vec3<double> Model::guideToModel(const Vec3<double> &v)
{
	return guideToModel(v.x, v.y);
}

// Return the GL-compatible array from the ModelMAT structure
void Model::copyRotationMatrix(double *m)
{
	rotationMatrix_.copyColumnMajor(m);
}

// Return the GL-compatible array from the ModelMAT structure
void Model::copyCameraMatrix(double *m)
{
	cameraMatrix_.copyColumnMajor(m);
}

// Return the current camera z-rotation
double Model::cameraRotation()
{
	return cameraRotation_;
}

// Spin the model about the z axis
void Model::zRotate(double angle)
{
	adjustCamera(0.0, 0.0, 0.0, (angle / DEGRAD ) * 2.0);
}

// Adjust the position of the camera
void Model::adjustCamera(const Vec3<double> &v, double r)
{
	adjustCamera(v.x,v.y,v.z,r);
}

// Return the size of the orthographic projection
double Model::orthoSize()
{
	return orthoSize_;
}

// Set exact rotation of model (angles passed in radians)
void Model::setRotation(double rotx, double roty)
{
	// Rotate the whole system by the amounts specified.
	dbgBegin(DM_CALLS,"Model::setRotation");
	static double sinx, cosx, siny, cosy;
	rotationMatrix_.setIdentity();
	// Calculate cos/sin terms for needless speedup!
	cosx = cos(rotx);
	cosy = cos(roty);
	sinx = sin(rotx);
	siny = sin(roty);
	rotationMatrix_.rows[0].set(cosy,0.0,siny,0.0);
	rotationMatrix_.rows[1].set((-sinx)*(-siny),cosx,(-sinx)*cosy,0.0);
	rotationMatrix_.rows[2].set(cosx*(-siny),sinx,cosx*cosy,0.0);
	//rot.rows[3].set(0.0,0.0,0.0,1.0);
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	logChange(LOG_CAMERA);
	dbgEnd(DM_CALLS,"Model::setRotation");
}

// Adjust Camera
void Model::adjustCamera(double dx, double dy, double dz, double angle)
{
	// Adjust the models camera variables
	dbgBegin(DM_CALLS,"Model::adjustCamera");
	double sincam, coscam;
	rCamera_.x += dx;
	rCamera_.y += dy;
	rCamera_.z += dz;
	cameraRotation_ = cameraRotation_ + angle;
	if (cameraRotation_ > 2.0*PI) cameraRotation_ -= 2.0*PI;
	coscam = cos(cameraRotation_);
	sincam = sin(cameraRotation_);
	// Now create the new matrix
	cameraMatrix_.rows[0].set(coscam,-sincam,0.0,rCamera_.x*coscam-rCamera_.y*sincam);
	cameraMatrix_.rows[1].set(sincam,coscam,0.0,rCamera_.x*sincam-rCamera_.y*coscam);
	cameraMatrix_.rows[2].set(0.0,0.0,1.0,rCamera_.z);
	cameraMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
	calculateViewMatrix();
	// Log camera change
	logChange(LOG_CAMERA);
	dbgEnd(DM_CALLS,"Model::adjustCamera");
}

// Adjust orthographic size
void Model::adjustOrthoSize(double delta)
{
	dbgBegin(DM_CALLS,"Model::adjustOrthoSize");
	#ifdef HAS_GUI
		orthoSize_ += delta;
		if (orthoSize_ < 1.0) orthoSize_ = 1.0;
		if (orthoSize_ > 50.0) orthoSize_ = 50.0;
		calculateViewMatrix();
		gui.mainView.doProjection();
		// Log camera change
		logChange(LOG_CAMERA);
	#endif
	dbgEnd(DM_CALLS,"Model::adjustOrthoSize");
}

// Reset Camera
void Model::resetCamera(const Vec3<double> &newr)
{
	// Adjust the models camera variables
	dbgBegin(DM_CALLS,"Model::resetCamera");
	#ifdef HAS_GUI
		rCamera_ = newr;
		cameraRotation_ = 0.0;
		// Now create the new matrix
		cameraMatrix_.rows[0].set(1.0,0.0,0.0,rCamera_.x);
		cameraMatrix_.rows[1].set(0.0,1.0,0.0,rCamera_.y);
		cameraMatrix_.rows[2].set(0.0,0.0,1.0,rCamera_.z);
		cameraMatrix_.rows[3].set(0.0,0.0,0.0,1.0);
		// Recalculate viewing matrix
		calculateViewMatrix();
		// Log camera change
		logChange(LOG_CAMERA);
	#endif
	dbgEnd(DM_CALLS,"Model::resetCamera");
}

// Reset View
void Model::resetView()
{
	// Reset the modelview matrix and the camera
	dbgBegin(DM_CALLS,"Model::resetView");
	#ifdef HAS_GUI
		static Vec3<double> newcam, newscreen;
		Atom *i, target;
		double z, largest = 0.0;
		rotationMatrix_.setIdentity();
		orthoSize_ = 20.0;
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
		if (gui.mainView.isValid())
			do
			{
				// Project our local atom and grab the z screen coordinate
				calculateViewMatrix();
				projectAtom(&target);
				z = target.rWorld().z;
				adjustCamera(0.0,0.0,-1.0,0.0);
			} while (z > -5.0);
		// Recalculate viewing matrix
		calculateViewMatrix();
		// Log camera change
		logChange(LOG_CAMERA);
	#endif
	dbgEnd(DM_CALLS,"Model::resetView");
}

// Rotate free
void Model::rotate(double dx, double dy)
{
	// Rotate the whole system by the amounts specified.
	dbgBegin(DM_CALLS,"Model::rotate");
	static double rotx, roty, theta, sinx, cosx, siny, cosy;
	static Mat4<double> newrotmat, oldrotmat;
	cameraRotation_ > PI ? theta = cameraRotation_-2.0*PI : theta = cameraRotation_;
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
	// Recalculate view matrix
	calculateViewMatrix();
	// Log camera change
	logChange(LOG_CAMERA);
	dbgEnd(DM_CALLS,"Model::rotate");
}

// Calculate View Matrix
void Model::calculateViewMatrix()
{
	// Calculate full viewing matrix
	viewMatrix_ = cameraMatrix_ * rotationMatrix_;
	// Calculate inverse
	viewMatrixInverse_ = viewMatrix_;
	viewMatrixInverse_.invert();
}

// Project the coordinates of all atoms in the model
void Model::projectAll()
{
	// Transform the model coordinates of all atoms into world GL and 2D screen coordinates
	dbgBegin(DM_CALLS,"Model::projectAll");
	#ifdef HAS_GUI
		if (projectionPoint_ != (logs_[LOG_COORDS] + logs_[LOG_CAMERA]))
		{
			if (gui.mainView.isValid()) for (Atom *i = atoms_.first(); i != NULL; i = i->next) projectAtom(i);
			projectionPoint_ = logs_[LOG_COORDS] + logs_[LOG_CAMERA];
		}
	#endif
	dbgEnd(DM_CALLS,"Model::projectAll");
}

// Project the coordinates of all selected atoms in the model
void Model::projectSelection()
{
	dbgBegin(DM_CALLS,"Model::projectSelection");
	#ifdef HAS_GUI
		if (gui.mainView.isValid()) for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) projectAtom(i);
	#endif
	dbgEnd(DM_CALLS,"Model::projectSelection");
}

// Project the coordinates of a single atom in the model
void Model::projectAtom(Atom *i)
{
	// Transform the model coordinates of specified atom into world GL and 2D screen coordinates
	dbgBegin(DM_MORECALLS,"Model::projectAtom");
	#ifdef HAS_GUI
		if (!gui.mainView.isValid())
		{
			dbgEnd(DM_MORECALLS,"Model::projectAtom");
			return;
		}
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
		srx = vmat[0] + vmat[2]*(screenr.x+1)/2.0;
		sry = vmat[1] + vmat[3]*(screenr.y+1)/2.0;
		i->rScreen().set(srx,sry,srz);
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		worldr.x += prefs.screenRadius(i);
		screenr = gui.mainView.PMAT * worldr;
		screenr.x /= screenr.w;
		screenr.y /= screenr.w;
		i->setScreenRadius(fabs( (vmat[0] + vmat[2]*(screenr.x+1)/2.0) - srx));
	#endif
	dbgEnd(DM_MORECALLS,"Model::projectAtom");
}

Vec4<double> &Model::worldToScreen(const Vec3<double> &v)
{
	// Project the supplied world coordinates into screen coordinates.
	// The returned vec4's 'w' component is the unit 'radius' at that point.
	dbgBegin(DM_CALLS,"Model::worldToScreen");
	static Vec4<double> modelr, screenr, worldr, result;
	static double x1,x2,radius;
	static GLint *vmat;
	#ifdef HAS_GUI
		screenr.zero();
		if (!gui.mainView.isValid())
		{
			dbgEnd(DM_CALLS,"Model::worldToScreen");
			return screenr;
		}
		// Projection formula is : worldr = P x M x modelr
		// Get the 3D coordinates of the atom - Multiply by modelview matrix 'view'
		modelr.set(v.x, v.y, v.z, 1.0);
		worldr = viewMatrix_ * modelr;
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
	#endif
	dbgEnd(DM_CALLS,"Model::worldToScreen");
	return result;
}

Vec3<double> Model::guideToModel(double sx, double sy)
{
	// Convert the screen coordinates passed to a position on the drawing guide, and then into model coordinates
	dbgBegin(DM_CALLS,"Model::guideToModel");
	static Vec4<double> guidepoint;
	static Vec3<double> newpoint;
	double radius, depth;
	#ifdef HAS_GUI
		depth = prefs.drawDepth();
		// First, project a point at the guide z-position into screen coordinates to get the guide 'yardstick'
		newpoint.set(0.0,0.0,depth);
		guidepoint = worldToScreen(newpoint);
		radius = guidepoint.w;
		// Now, calculate the position of the clicked point on the guide
		newpoint.x = sx - (gui.mainView.width() / 2.0 );
		newpoint.y = (gui.mainView.height() - sy) - (gui.mainView.height() / 2.0 );
		newpoint /= radius;
		newpoint.z = depth + rCamera_.z;
		// Convert this world coordinate into model coordinates by multiplying by the inverse of the PM matrix.
		newpoint *= viewMatrixInverse_;
		// Also need to account for periodic systems (which are translated so the cell midpoint is centred in the screen) by adding the cell's centre coordinate
		newpoint += cell_.centre();
	#endif
	dbgEnd(DM_CALLS,"Model::guideToModel");
	return newpoint;
}


