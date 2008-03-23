/*
	*** Forcefield term functional forms
	*** src/energy/forms.cpp
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

#include "energy/forms.h"

// Generation rules (for rule-based FFs)
const char *FFR_strings[FFR_NITEMS] = { "No rules defined.", "UniversalFF (Rappe et al.)" };
const char *FFR_keywords[FFR_NITEMS] = { "norules", "uff" };
const char *text_from_FFR(ForcefieldRules i)
	{ return FFR_strings[i]; }
ForcefieldRules FFR_from_text(const char *s)
	{ return (ForcefieldRules) enumSearch("forcefield rules set",FFR_NITEMS,FFR_keywords,s); }

// Electrostatic model
const char *EM_keywords[EM_NITEMS] = { "off", "coulomb", "ewald", "ewaldauto" };
const char *text_from_EM(ElecMethod i)
	{ return EM_keywords[i]; }
ElecMethod EM_from_text(const char *s)
	{ return (ElecMethod) enumSearch("electrostatics method",EM_NITEMS,EM_keywords,s); }

// VDW potential forms
const char *VF_strings[VF_NITEMS] = { "Unspecified", "Lennard-Jones 12-6", "Buckingham exp6" };
const char *VF_keywords[VF_NITEMS] = { "_NULL_", "lj", "buck" };
const char *text_from_VF(VdwFunction i)
	{ return VF_strings[i]; }
const char *keyword_from_VF(VdwFunction i)
	{ return VF_keywords[i]; }
VdwFunction VF_from_text(const char *s)
	{ return (VdwFunction) enumSearch("VDW style",VF_NITEMS,VF_keywords,s); }

// Bond potential forms
const char *BF_strings[BF_NITEMS] = { "Unspecified", "Harmonic", "Morse" };
const char *BF_keywords[BF_NITEMS] = { "_NULL_", "harmonic", "morse" };
const char *text_from_BF(BondFunction i)
	{ return BF_keywords[i]; }
BondFunction BF_from_text(const char *s)
	{ return (BondFunction) enumSearch("bond style",BF_NITEMS,BF_keywords,s); }

// Angle potential forms
const char *AF_strings[AF_NITEMS] = { "Unspecified", "Harmonic", "Cosine", "UFF Cosine" };
const char *AF_keywords[AF_NITEMS] = { "_NULL_", "harmonic", "cos", "uff" };
const char *text_from_AF(AngleFunction i)
	{ return AF_keywords[i]; }
AngleFunction AF_from_text(const char *s)
	{ return (AngleFunction) enumSearch("angle style",AF_NITEMS,AF_keywords,s); }

// Torsion potential forms
const char *TF_strings[TF_NITEMS] = { "Unspecified", "Cosine", "Triple Cosine", "Quadruple Cosine", "Constant + Triple Cosine" };
const char *TF_keywords[TF_NITEMS] = { "_NULL_", "cosine", "cos3", "cos4", "cosc" };
const char *text_from_TF(TorsionFunction i)
	{ return TF_keywords[i]; }
TorsionFunction TF_from_text(const char *s)
	{ return (TorsionFunction) enumSearch("torsion style",TF_NITEMS,TF_keywords,s); }
