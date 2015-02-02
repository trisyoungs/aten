/*
	*** Model Extras
	*** src/model/modelextras.cpp
	Copyright T. Youngs 2007-2015

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
#include "model/clipboard.h"

// Add new basis function to the list
BasisShell *Model::addBasisShell()
{
	return basisShells_.add();
}

// Return the first basis function in the list
BasisShell *Model::basisShells()
{
	return basisShells_.first();
}

// Return total number of defined basis functions
int Model::nBasisShells()
{
	return basisShells_.nItems();
}

// Return total number of cartesian basis functions
int Model::nCartesianBasisFunctions()
{
	int result = 0;
	for (BasisShell *basis = basisShells_.first(); basis != NULL; basis = basis->next) result += BasisShell::nCartesianFunctions(basis->type());
	return result;
}

// Add new eigenvevtor to the list
Eigenvector *Model::addEigenvector()
{
	return eigenvectors_.add();
}

// Return the first eigenvevtor in the list
Eigenvector *Model::eigenvectors()
{
	return eigenvectors_.first();
}

// Return the n'th eigenvector in the list
Eigenvector *Model::eigenvector(int n)
{
	return eigenvectors_[n];
}

// Return total number of defined eigenvectors
int Model::nEigenvectors()
{
	return eigenvectors_.nItems();
}

// Return density of nth eigenvalue at given coordinates
double Model::eigenvectorDensityAt(int id, Vec3<double> v)
{
	msg.enter("Model::eigenvectorDensityAt");
	int n, i = 0;
	double result = 0.0;
	// Grab eigenvector pointer
	if ((id < 0) || (id >= eigenvectors_.nItems()))
	{
		msg.print("Illegal eigenvector ID '%i' given to Model::eigenvectorDensityAt.\n", id);
		msg.exit("Model::eigenvectorDensityAt");
		return 0.0;
	}
	Eigenvector *evec = eigenvectors_[id];
	double *eigenvec = evec->eigenvector();
	for (BasisShell *bas = basisShells_.first(); bas != NULL; bas = bas->next)
	{
		// Cycle over primitives
// 		for (n = 0; n < bas->nCartesianFunctions(); ++n)
// 		{
// 			text.print("%i (%s)\n", bas->atomId()+1, m->atom(bas->atomId()) != NULL ? Elements().symbol(m->atom(bas->atomId())) : "NULL");
// 			i++;
// 		}
	}
	msg.exit("Model::eigenvectorDensityAt");
	return result;
}

// Add a new vibration to the model
Vibration *Model::addVibration(int size)
{
	Vibration *vib = vibrations_.add();
	vib->initialise(this, size);
	return vib;
}

// Return number of defined vibrations
int Model::nVibrations()
{
	return vibrations_.nItems();
}

// Return first vibration
Vibration *Model::vibrations()
{
	return vibrations_.first();
}

// Return n'th vibration
Vibration *Model::vibration(int n)
{
	return vibrations_[n];
}

 // Generate trajectory for n'th vibration
void Model::generateVibration(int index, int nsteps)
{
	msg.enter("Model::generateVibration");
	// Delete old vibrations
	vibrationCurrentFrame_ = NULL;
	vibrationFrames_.clear();
	vibrationFrameIndex_ = -1;

	// Check vibration index
	if ((index < 0) || (index >= vibrations_.nItems()))
	{
		printf("Internal Error : Vibration index %i given to Model::generateVibration is invalid.\n", index);
		msg.exit("Model::generateVibration");
		return;
	}

	// Grab necessary pointers
	Vibration *vib = vibrations_[index];
	double freq = vib->frequency();
	Vec3<double> *displacements = vib->displacements();

	// Check number of atoms against number of defined displacements
	if (vib->nDisplacements() != atoms_.nItems())
	{
		printf("Internal Error : Vibration given to Model::generateVibration contains %i displacements, but there are %i atoms.\n", vib->nDisplacements(), atoms_.nItems());
		msg.exit("Model::generateVibration");
		return;
	}

	// Ready to generate frames
	double delta, stepdelta = 1.0 / nsteps;
	int k;
	Clipboard clip;
	clip.copyAll(this);
	// LOOP!
	for(k=0; k<=nsteps; k++)
	{	
		delta=k*stepdelta;
		// To add a new frame to the list	
		Model *m = vibrationFrames_.add();
		m->setParent(this);
		m->setType(Model::VibrationFrameType);
		clip.pasteToModel(m, FALSE);
		
		// To loop over original atom coordinates
		int count = 0;
		for (Atom *i = m->atoms(); i != NULL; i = i->next)
		{
			i->r() += displacements[count] * delta;
			++count;
		}
	}
	
	// Reset variables
	vibrationForward_ = TRUE;
	vibrationFrameIndex_ = 0;
	vibrationCurrentFrame_ = vibrationFrames_.first();
	msg.exit("Model::generateVibration");
}

// Return current vibration frame
Model *Model::vibrationCurrentFrame()
{
	return vibrationCurrentFrame_;
}

// Set current frame index 
void Model::setVibrationFrameIndex(int index)
{
	// Check frame range
	if ((index < 0) || (index >= vibrationFrames_.nItems()))
	{
		msg.print("Internal Error: Vibration frame index %i is out of range (vibration contains %i frames).\n", index, vibrationFrames_.nItems());
		return;
	}
	vibrationCurrentFrame_ = vibrationFrames_[index];
	vibrationFrameIndex_ = index;
}

// Move on to next/prev frame (depending on current playback direction)
void Model::vibrationNextFrame()
{
	msg.enter("Model::vibrationNextFrame");
	// Check for presence of a vibration trajectory
	if (vibrationFrames_.nItems() == 0)
	{
		printf("Internal Error : Model '%s' has no vibration trajectory to display.\n", name_.get());
		msg.exit("Model::vibrationNextFrame");
		return;
	}
	if (vibrationForward_)
	{
		if (vibrationCurrentFrame_->next == NULL)
		{
			vibrationForward_ = FALSE;
			vibrationCurrentFrame_ = vibrationCurrentFrame_->prev;
		}
		else vibrationCurrentFrame_ = vibrationCurrentFrame_->next;
	}
	else
	{
		if (vibrationCurrentFrame_->prev == NULL)
		{
			vibrationForward_ = TRUE;
			vibrationCurrentFrame_ = vibrationCurrentFrame_->next;
		}
		else vibrationCurrentFrame_ = vibrationCurrentFrame_->prev;
	}
	if (vibrationForward_) ++vibrationFrameIndex_;
	else --vibrationFrameIndex_;
	msg.exit("Model::vibrationNextFrame");
}

// Return index of current vibration frame
int Model::vibrationFrameIndex()
{
	return vibrationFrameIndex_;
}
