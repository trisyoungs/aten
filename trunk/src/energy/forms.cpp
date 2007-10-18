/*
	*** Forcefield term functional forms
	*** src/energy/forms.cpp

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
const char *text_from_FFR(ff_rules i)
	{ return FFR_strings[i]; }
ff_rules FFR_from_text(const char *s)
	{ return (ff_rules) enum_search("forcefield rules set",FFR_NITEMS,FFR_keywords,s); }

// Electrostatic model
const char *EM_keywords[EM_NITEMS] = { "off", "coulomb", "ewald", "ewaldauto" };
const char *text_from_EM(elec_type i)
	{ return EM_keywords[i]; }
elec_type EM_from_text(const char *s)
	{ return (elec_type) enum_search("electrostatics method",EM_NITEMS,EM_keywords,s); }

// VDW potential forms
const char *VF_strings[VF_NITEMS] = { "Unspecified", "Lennard-Jones 12-6", "Buckingham exp6" };
const char *VF_keywords[VF_NITEMS] = { "_NULL_", "lj", "buck" };
const char *text_from_VF(vdw_func i)
	{ return VF_strings[i]; }
vdw_func VF_from_text(const char *s)
	{ return (vdw_func) enum_search("VDW style",VF_NITEMS,VF_keywords,s); }

// Bond potential forms
const char *BF_strings[BF_NITEMS] = { "Unspecified", "Harmonic", "Morse" };
const char *BF_keywords[BF_NITEMS] = { "_NULL_", "harmonic", "morse" };
const char *text_from_BF(bond_func i)
	{ return BF_keywords[i]; }
bond_func BF_from_text(const char *s)
	{ return (bond_func) enum_search("bond style",BF_NITEMS,BF_keywords,s); }

// Angle potential forms
const char *AF_strings[AF_NITEMS] = { "Unspecified", "Harmonic", "Cosine", "UFF Cosine" };
const char *AF_keywords[AF_NITEMS] = { "_NULL_", "harmonic", "cos", "uff" };
const char *text_from_AF(angle_func i)
	{ return AF_keywords[i]; }
angle_func AF_from_text(const char *s)
	{ return (angle_func) enum_search("angle style",AF_NITEMS,AF_keywords,s); }

// Torsion potential forms
const char *TF_strings[TF_NITEMS] = { "Unspecified", "Cosine", "Triple Cosine" };
const char *TF_keywords[TF_NITEMS] = { "_NULL_", "cosine", "cos3" };
const char *text_from_TF(torsion_func i)
	{ return TF_keywords[i]; }
torsion_func TF_from_text(const char *s)
	{ return (torsion_func) enum_search("torsion style",TF_NITEMS,TF_keywords,s); }
