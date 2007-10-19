/*
	*** Numerical constants
	*** src/base/constants.h
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

#ifndef H_CONSTANTS_H
#define H_CONSTANTS_H

// Richtig oder falsch
#define FALSE (0)
#define TRUE (!FALSE)

// Permittivity of free space (m-3 kg-1 s4 A2)
#define EPSILON0 8.85418782E-12

// Elementary Charge (C, Amp s)
#define ECHARGE 1.60217646E-19

// Avogadro's Number
#define AVOGADRO 6.0221415E23

// Pi
#define PI 3.141592653589793
#define TWOPI 6.283185307179586
#define SQRTPI 1.772453850905515

// Degrees per Radian
#define DEGRAD 57.295779578552

/*
// Conversion from atomic units to J/mol for Electrostatic Energy / Forces
//
// COULCONVERT =      q * q	 
//		 ----------------
//		 4 * pi * e0 * r2
*/
#define COULCONVERT 1389354.44426359172669289

// Fortran Datatype Sizes
#define FORT_INT 4
#define FORT_REAL 8

#endif
