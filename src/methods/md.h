/*
	*** Molecular Dynamics methods
	*** src/methods/md.h
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

#ifndef ATEN_MOLECULARDYNAMICS_H
#define ATEN_MOLECULARDYNAMICS_H

// Forward declarations

// Molecular Dynamics
class MDEngine
{
	public:
	// Constructor / Destructor
	MDEngine();
	~MDEngine();

	/*
	// MD Parameters
	*/
	private:
	// Starting simulation temperature, in K
	double temperature_;
	// Starting simulation pressure, in atm
	double pressure_;
	// Simulation timestep , in ps
	double timeStepMantissa_;
	int timeStepExponent_;
	// Number of steps to run
	int nSteps_;
	// Frequency of trajectory dump

	public:
	// Set starting simulation temperature
	void setTemperature(double t);
	// Return starting simulation temperature
	double temperature();
	// Set starting simulation pressure
	void setPressure(double p);
	// Return starting simulation pressure
	double pressure();
	// Set simulation timestep (mantissa/exponent)
	void setTimeStep(double mantissa, int exponent);
	// Return simulation timestep mantissa
	double timeStepMantissa();
	// Return simulation timestep exponent
	int timeStepExponent();
	// Return simulation timestep as simple real number
	double timeStep();
	// Set number of simulation steps
	void setNSteps(int n);
	// Return number of simulation steps
	int nSteps();

	/*
	// Methods
	*/
	public:
	// Perform MD simulation with current parameters
	void run();
};

// Static Singleton
extern MDEngine md;

#endif
