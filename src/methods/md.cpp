/*
	*** Molecular Dynamics methods
	*** src/methods/md.cpp
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

#include "main/aten.h"
#include "methods/md.h"
#include "model/model.h"
#include "gui/gui.h"

// Static Singleton
MDEngine md;

// Constructor
MDEngine::MDEngine()
{
	// Private variables
	temperature_ = 298.15;
	pressure_ = 1.0;
	nSteps_ = 100;
	timeStepMantissa_ = 5.0;
	timeStepExponent_ = -4;
}

// Destructor
MDEngine::~MDEngine()
{
}

/*
// Run Parameters
*/

// Set starting MD temperature
void MDEngine::setTemperature(double t)
{
	temperature_ = t;
}

// Return starting MD temperature
double MDEngine::temperature()
{
	return temperature_;
}

// Set starting simulation pressure
void MDEngine::setPressure(double p)
{
	pressure_ = p;
}

// Return starting simulation pressure
double MDEngine::pressure()
{
	return pressure_;
}

// Set simulation timestep (mantissa/exponent)
void MDEngine::setTimeStep(double mantissa, int exponent)
{
	timeStepMantissa_ = mantissa;
	timeStepExponent_ = exponent;
}

// Return simulation timestep mantissa
double MDEngine::timeStepMantissa()
{
	return timeStepMantissa_;
}

// Return simulation timestep exponent
int MDEngine::timeStepExponent()
{
	return timeStepExponent_;
}

// Return simulation timestep as simple real number
double MDEngine::timeStep()
{
	return timeStepMantissa_ * pow(10, timeStepExponent_);
}

// Set number of simulation steps
void MDEngine::setNSteps(int n)
{
	nSteps_ = n;
}

// Return number of simulation steps
int MDEngine::nSteps()
{
	return nSteps_;
}

/*
// Methods
*/

// Perform MD simulation with current parameters
void MDEngine::run()
{
	msg.enter("MDEngine::run");

	// Perform some checks to make sure the model is ready...
	Model *m = aten.currentModel()->renderSource();
	if (m->nAtoms() == 0)
	{
		msg.print("No atoms in model - can't run MD.\n");
		msg.exit("MDEngine::run");
		return;
	}
	if (!m->createExpression())
	{
		msg.print("No valid energy expression for model - can't run MD.\n");
		msg.exit("MDEngine::run");
		return;
	}

	// Okay to proceed - print out summary of MD run parameters
	msg.print("Preparing MD run:\n");
	msg.print("Temperature     : %e K\n", temperature_);
	msg.print("Pressure        : %e atm\n", pressure_);
	msg.print("Timestep        : %e ps\n", timeStep());
	msg.print("Number of steps : %i\n", nSteps_);
	msg.print("Ready to go!\n");


	msg.exit("MDEngine::run");
}
