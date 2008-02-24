/*
	*** Forcefield term functional forms
	*** src/energy/forms.h
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

#ifndef H_FORMS_H
#define H_FORMS_H

#include "base/sysfunc.h"

// Generation rules (for rule-based FFs)
enum ff_rules { FFR_NORULES, FFR_UFF, FFR_NITEMS };
const char *text_from_FFR(ff_rules);
ff_rules FFR_from_text(const char*);

// Electrostatic model
enum elec_method { EM_OFF, EM_COULOMB, EM_EWALD, EM_EWALDAUTO, EM_NITEMS };
const char *text_from_EM(elec_method);
elec_method EM_from_text(const char*);

// VDW potential forms
enum vdw_func { VF_UNSPECIFIED, VF_LJ, VF_BUCK, VF_NITEMS };
const char *text_from_VF(vdw_func);
const char *keyword_from_VF(vdw_func);
vdw_func VF_from_text(const char*);
enum vdw_lj { VF_LJ_EPS, VF_LJ_SIGMA };
enum vdw_buck { VF_BUCK_A, VF_BUCK_B, VF_BUCK_C };

// Bond potential forms
enum bond_func { BF_UNSPECIFIED, BF_HARMONIC, BF_MORSE, BF_NITEMS };
const char *text_from_BF(bond_func);
bond_func BF_from_text(const char*);
enum bond_harmonic { BF_HARMONIC_K, BF_HARMONIC_EQ };

// Angle potential forms
enum angle_func { AF_UNSPECIFIED, AF_HARMONIC, AF_COSINE, AF_UFFCOSINE1, AF_UFFCOSINE2, AF_NITEMS };
const char *text_from_AF(angle_func);
angle_func AF_from_text(const char*);
enum angle_harmonic { AF_HARMONIC_K, AF_HARMONIC_EQ };
enum angle_cosine { AF_COSINE_K, AF_COSINE_S, AF_COSINE_EQ };
enum angle_uffcosine { AF_UFFCOSINE_K, AF_UFFCOSINE_N, AF_UFFCOSINE_EQ };

// Torsion potential forms
enum torsion_func { TF_UNSPECIFIED, TF_COSINE, TF_COS3, TF_COS4, TF_COS3C, TF_NITEMS };
const char *text_from_TF(torsion_func);
torsion_func TF_from_text(const char*);
enum torsion_cosine { TF_COSINE_K, TF_COSINE_EQ, TF_COSINE_P };
enum torsion_cos3 { TF_COS3_K1, TF_COS3_K2, TF_COS3_K3 };
enum torsion_cos4 { TF_COS4_K1, TF_COS4_K2, TF_COS4_K3, TF_COS4_K4 };
enum torsion_cosc { TF_COS3C_K0, TF_COS3C_K1, TF_COS3C_K2, TF_COS3C_K3 };

#endif
