/*
	*** Steepest descent minimiser
	*** src/methods/sd.h
	Copyright T. Youngs 2007

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

#ifndef H_SD_H
#define H_SD_H

#include "model/model.h"
#include "methods/linemin.h"

// Steepest Descent Minimiser
class sd_method : public linemin
{
	public:
	// Constructor
	sd_method();

	private:
	// Maximum number of iterations to perform
	int ncycles;

	public:
	// Set maximum number of cycles to perform
	void set_ncycles(int i) { ncycles = i; }
	// Get maximum number of  for MC move
	int get_ncycles() { return ncycles; }
	// Minimise the specified model
	void minimise(model *source, double econ, double fcon);
};

#endif
