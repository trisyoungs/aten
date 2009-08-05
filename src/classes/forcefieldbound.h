/*
	*** Forcefield bound term
	*** src/classes/forcefieldbound.h
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

#ifndef ATEN_FORCEFIELDBOUND_H
#define ATEN_FORCEFIELDBOUND_H

#include "base/dnchar.h"
#include "base/forms.h"

// Forcefield bound interaction type
class ForcefieldBound
{
	public:
	// List pointers
	ForcefieldBound *prev, *next;
	// Constructor
	ForcefieldBound();
	// Forcefield Bound Interaction Type
	enum BoundType { NoInteraction, BondInteraction, AngleInteraction, TorsionInteraction };
	static const char *boundType(ForcefieldBound::BoundType bt);
	static int boundTypeNAtoms(ForcefieldBound::BoundType bt);

	private:
	// Type of bound interaction
	BoundType type_;
	// Form of bound interaction type
	int functionalForm_;
	// Forcefield types involved in this term
	Dnchar typeNames_[MAXFFBOUNDTYPES];
	// Interaction parameter data
	double params_[MAXFFPARAMDATA];

	public:
	// Set the type of bound interaction
	void setType(BoundType fc);
	// Return the type of bound interaction
	BoundType type();
	// Return functional form text
	const char *formText();
	// Return the functional form (cast as a bond style)
	BondFunctions::BondFunction bondStyle();
	// Return the functional form (cast as a angle style)
	AngleFunctions::AngleFunction angleStyle();
	// Return the functional form (cast as a torsion style)
	TorsionFunctions::TorsionFunction torsionStyle();
	// Set the bond functional form
	void setBondStyle(BondFunctions::BondFunction bf);
	// Set the angle functional form
	void setAngleStyle(AngleFunctions::AngleFunction af);
	// Set the torsion functional form
	void setTorsionStyle(TorsionFunctions::TorsionFunction tf);
	// Set the parameter data specified
	void setParameter(int i, double d);
	// Return parameter data specified
	double parameter(int i);
	// Return pointer to parameter array
	double *parameters();
	// Return the atom type 'n'
	const char *typeName(int n);
	// Return the atom type array
	Dnchar *typeNames();
	// Set the atom type 'n'
	void setTypeName(int n, const char *s);
};

#endif

