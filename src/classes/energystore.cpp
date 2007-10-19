/*
	*** Energy store
	*** src/classes/energystore.cpp
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

#include "base/prefs.h"
#include "classes/energystore.h"
#include "classes/pattern.h"
#include "model/model.h"

// Constructor
energystore::energystore()
{
	bond = NULL;
	angle = NULL;
	torsion = NULL;
	vdw_intra = NULL;
	coulomb_intra = NULL;
	ewald_real_intra = NULL;
	ewald_recip_intra = NULL;
	vdw_inter = NULL;
	vdw_tail = 0.0;
	coulomb_inter = NULL;
	ewald_real_inter = NULL;
	ewald_recip_inter = NULL;
	ewald_self_correct = NULL;
	ewald_mol_correct = NULL;
	size = 0;
	calculated = FALSE;
	#ifdef MEMDEBUG
	memdbg.create[MD_ENERGYSTORE] ++;
	#endif
}

// Destructor
energystore::~energystore()
{
	deallocate();
	#ifdef MEMDEBUG
	memdbg.destroy[MD_ENERGYSTORE] ++;
	#endif
}

// Add
void energystore::add(energy_type et, double energy, int id1, int id2)
{
	dbg_begin(DM_CALLS,"energystore::add");
	if ((id1 >= size) || (id2 >= size))
	{
		printf("energystore::add <<<< Array element out of range - %i %i - Ignored >>>>\n", id1, id2);
		dbg_end(DM_CALLS,"energystore::add");
		return;
	}
	switch (et)
	{
		case (ET_BOND):
			bond[id1] += energy;
			break;
		case (ET_ANGLE):
			angle[id1] += energy;
			break;
		case (ET_TORSION):
			torsion[id1] += energy;
			break;
		case (ET_VDWINTRA):
			vdw_intra[id1] += energy;
			break;
		case (ET_VDWINTER):
			vdw_inter[id1][id2] += energy;
			break;
		case (ET_VDWTAIL):
			vdw_tail += energy;
			break;
		case (ET_COULOMBINTRA):
			coulomb_intra[id1] += energy;
			break;
		case (ET_COULOMBINTER):
			coulomb_inter[id1][id2] += energy;
			break;
		case (ET_EWALDREALINTRA):
			ewald_real_intra[id1] += energy;
			break;
		case (ET_EWALDREALINTER):
			ewald_real_inter[id1][id2] += energy;
			break;
		case (ET_EWALDRECIPINTRA):
			ewald_recip_intra[id1] += energy;
			break;
		case (ET_EWALDRECIPINTER):
			ewald_recip_inter[id1][id2] += energy;
			break;
		case (ET_EWALDSELF):
			ewald_self_correct[id1] += energy;
			break;
		case (ET_EWALDMOL):
			ewald_mol_correct[id1] += energy;
			break;
	}
	dbg_end(DM_CALLS,"energystore::add");
}

// Deallocate arrays
void energystore::deallocate()
{
	dbg_begin(DM_CALLS,"energystore::deallocate");
	if (bond != NULL) delete[] bond;
	if (angle != NULL) delete[] angle;
	if (torsion != NULL) delete[] torsion;
	if (vdw_intra != NULL) delete[] vdw_intra;
	if (coulomb_intra != NULL) delete[] coulomb_intra;
	if (ewald_real_intra != NULL) delete[] ewald_real_intra;
	if (ewald_recip_intra != NULL) delete[] ewald_recip_intra;
	for (int n=0; n<size; n++)
	{
		delete[] vdw_inter[n];
		delete[] coulomb_inter[n];
		delete[] ewald_real_inter[n];
		delete[] ewald_recip_inter[n];
	}
	if (vdw_inter != NULL) delete[] vdw_inter;
	if (coulomb_inter != NULL) delete[] coulomb_inter;
	if (ewald_real_inter != NULL) delete[] ewald_real_inter;
	if (ewald_recip_inter != NULL) delete[] ewald_recip_inter;
	if (ewald_self_correct != NULL) delete[] ewald_self_correct;
	if (ewald_mol_correct != NULL) delete[] ewald_mol_correct;
	size = 0;
	dbg_end(DM_CALLS,"energystore::deallocate");
}

// Resize
void energystore::resize(int newsize)
{
	dbg_begin(DM_CALLS,"energystore::resize");
	// Delete old data first
	deallocate();
	// Now create new arrays...
	size = newsize;
	bond = new double[size];
	angle = new double[size];
	torsion = new double[size];
	vdw_intra = new double[size];
	vdw_inter = new double*[size];
	coulomb_intra = new double[size];
	ewald_real_intra = new double[size];
	ewald_recip_intra = new double[size];
	coulomb_inter = new double*[size];
	ewald_real_inter = new double*[size];
	ewald_recip_inter = new double*[size];
	for (int n=0; n<size; n++)
	{
		vdw_inter[n] = new double[size];
		coulomb_inter[n] = new double[size];
		ewald_real_inter[n] = new double[size];
		ewald_recip_inter[n] = new double[size];
	}
	ewald_self_correct = new double[size];
	ewald_mol_correct = new double[size];
	clear();
	msg(DM_VERBOSE,"Energy store resized to %i\n",size);
	dbg_end(DM_CALLS,"energystore::resize");
}

// CLear values
void energystore::clear()
{
	// Clear all the values in the energy store
	dbg_begin(DM_CALLS,"energystore::clear");
	for (int n=0; n<size; n++)
	{
		bond[n] = 0.0;
		angle[n] = 0.0;
		torsion[n] = 0.0;
		vdw_intra[n] = 0.0;
		coulomb_intra[n] = 0.0;
		ewald_real_intra[n] = 0.0;
		ewald_recip_intra[n] = 0.0;
		for (int m=0; m<size; m++)
		{
			vdw_inter[n][m] = 0.0;
			coulomb_inter[n][m] = 0.0;
			ewald_real_inter[n][m] = 0.0;
			ewald_recip_inter[n][m] = 0.0;
		}
		ewald_self_correct[n] = 0.0;
		ewald_mol_correct[n] = 0.0;
	}
	vdw_tail = 0.0;
	tot_bond = 0.0;
	tot_angle = 0.0;
	tot_torsion = 0.0;
	tot_vdw = 0.0;
	tot_elec = 0.0;
	tot_intra = 0.0;
	tot_inter = 0.0;
	tot_ereal = 0.0;
	tot_erecip = 0.0;
	tot_eself = 0.0;
	tot_emol = 0.0;
	total = 0.0;
	calculated = FALSE;
	dbg_end(DM_CALLS,"energystore::clear");
}

// Create totals
void energystore::totalise()
{
	// Sum up the energies to get totals for individual aspects and the total overall energy.
	dbg_begin(DM_CALLS,"energystore::totalise");
	tot_bond = 0.0;
	tot_angle = 0.0;
	tot_torsion = 0.0;
	tot_vdw = 0.0;
	tot_elec = 0.0;
	tot_intra = 0.0;
	tot_inter = 0.0;
	tot_ereal = 0.0;
	tot_erecip = 0.0;
	tot_eself = 0.0;
	tot_emol = 0.0;
	total = 0.0;
	for (int n=0; n<size; n++)
	{
		tot_bond += bond[n];
		tot_angle += angle[n];
		tot_torsion += torsion[n];
		tot_vdw += vdw_intra[n];
		tot_elec += coulomb_intra[n];
		tot_elec += ewald_real_intra[n];
		tot_elec += ewald_recip_intra[n];
		for (int m=n; m<size; m++)
		{
			// Symmetrise matrices here (for printed output)
			vdw_inter[m][n] = vdw_inter[n][m];
			coulomb_inter[m][n] = coulomb_inter[n][m];
			ewald_real_inter[m][n] = ewald_real_inter[n][m];
			ewald_recip_inter[m][n] = ewald_recip_inter[n][m];
			// Sum intermolecular contributions
			tot_vdw += vdw_inter[n][m];
			tot_elec += coulomb_inter[n][m];
			tot_ereal += ewald_real_inter[n][m];
			// Ewald off-diagonal terms must be added twice
			tot_erecip += (n == m ? ewald_recip_inter[n][m] : 2.0*ewald_recip_inter[n][m]);
		}
		tot_eself += ewald_self_correct[n];
		tot_emol += ewald_mol_correct[n];
	}
	tot_vdw += vdw_tail;
	tot_elec += tot_ereal + tot_erecip - tot_eself - tot_emol;
	tot_intra = tot_bond + tot_angle + tot_torsion;
	tot_inter = tot_vdw + tot_elec;
	total = tot_intra + tot_inter;
	calculated = TRUE;
	dbg_end(DM_CALLS,"energystore::totalise");
}

// Print out all energy terms
void energystore::print()
{
	dbg_begin(DM_CALLS,"energystore::print");
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print");
		return;
	}
	msg(DM_NONE,"Energy (%s):\n",text_from_EU(prefs.get_internal_units()));
	msg(DM_NONE,"   Bond : %13.6f\n",tot_bond);
	msg(DM_NONE,"  Angle : %13.6f\n",tot_angle);
	msg(DM_NONE,"Torsion : %13.6f\n",tot_torsion);
	msg(DM_NONE,"    VDW : %13.6f (tail contribution : %13.6f)\n",tot_vdw,vdw_tail);
	msg(DM_NONE,"   Elec : %13.6f\n",tot_elec);
	msg(DM_NONE,"  TOTAL : %13.6f\n",total);
	dbg_end(DM_CALLS,"energystore::print");
}

// Print energy summary
void energystore::print_summary()
{
	dbg_begin(DM_CALLS,"energystore::print_summary");
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print_summary - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print_summary");
		return;
	}
	msg(DM_NONE,"Etot = %13.6e %s, b a t = %13.6e %13.6e %13.6e v = %13.6e e = %13.6e\n",total, text_from_EU(prefs.get_internal_units()),tot_bond,tot_angle,tot_torsion,tot_vdw,tot_elec);
	dbg_end(DM_CALLS,"energystore::print_summary");
}

// Print out Ewald energy terms
void energystore::print_ewald()
{
	dbg_begin(DM_CALLS,"energystore::print_ewald");
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print_ewald - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print_ewald");
		return;
	}
	msg(DM_NONE,"Ewald Energy (%s):\n",text_from_EU(prefs.get_internal_units()));
	msg(DM_NONE," Real : %13.6f\n",tot_ereal);
	msg(DM_NONE,"Recip : %13.6f\n",tot_erecip);
	msg(DM_NONE," Self : %13.6f\n",tot_eself);
	msg(DM_NONE,"  Mol : %13.6f\n",tot_emol);
	msg(DM_NONE,"TOTAL : %13.6f\n",tot_elec);
	dbg_end(DM_CALLS,"energystore::print_ewald");
}

// Print out VDW energy decomposition matrix
void energystore::print_vdwmatrix(model *m)
{
	dbg_begin(DM_CALLS,"energystore::print_vdwmatrix");
	int i, count1, count2;
	pattern *p1, *p2;
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print_vdwmatrix - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print_vdwmatrix");
		return;
	}
	// Print out VDW energy decomposition
	printf("VDW Interaction Energy:\n     Pattern         Intra  ");
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next) printf("%13s  ",p1->get_name());
	printf("\n");
	count1 = 0;
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next)
	{
		count2 = 0;
		printf("%13s  %13.6e  ",p1->get_name(),vdw_intra[count1]);
		for (p2 = m->get_patterns(); p2 != NULL; p2 = p2->next)
		{
			printf("%13.6e  ",vdw_inter[count1][count2]);
			count2 ++;
		}
		printf("\n");
		count1 ++;
	}
	dbg_end(DM_CALLS,"energystore::print_vdwmatrix");
}

// Print out electrostatic energy decomposition matrix
void energystore::print_elecmatrix(model *m)
{
	dbg_begin(DM_CALLS,"energystore::print_elecmatrix");
	int count1, count2;
	pattern *p1, *p2;
	double energy;
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print_elecmatrix - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print_elecmatrix");
		return;
	}
	elec_type et = prefs.get_electrostatics();
	count1 = 0;
	// Print out electrostatic energy decomposition
	printf("Electrostatic Interaction Energy:\n      Pattern          Intra  ");
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next) printf("%13s  ",p1->get_name());
	printf("\n");
	count1 = 0;
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next)
	{
		count2 = 0;
		switch (et)
		{
			case (EM_COULOMB):
				printf("%13s  %13.6e  ",p1->get_name(),coulomb_intra[count1]);
				for (p2 = m->get_patterns(); p2 != NULL; p2 = p2->next)
				{
					printf("%13.6e  ",coulomb_inter[count1][count2]);
					count2 ++;
				}
				break;
			default:	 // Ewald
				energy = ewald_real_intra[count1] + ewald_recip_intra[count1] - ewald_mol_correct[count1];
				printf("%13s  %13.6e  ",p1->get_name(),energy);
				for (p2 = m->get_patterns(); p2 != NULL; p2 = p2->next)
				{
					energy = ewald_real_inter[count1][count2] + ewald_recip_inter[count1][count2];
					if (count1 == count2) energy -= ewald_self_correct[count1];
					else energy += ewald_recip_inter[count1][count2];
					printf("%13.6e  ",energy);
					count2 ++;
				}
				break;
		}
		printf("\n");
		count1 ++;
	}
	dbg_end(DM_CALLS,"energystore::print_elecmatrix");
}

// Print out interpattern energy decomposition matrix
void energystore::print_intermatrix(model *m)
{
	dbg_begin(DM_CALLS,"energystore::print_intermatrix");
	int count1, count2;
	pattern *p1, *p2;
	double energy_inter, energy_intra;
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print_intermatrix - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print_intermatrix");
		return;
	}
	elec_type et = prefs.get_electrostatics();
	// Print out total interpattern energy decomposition
	printf("Total Interaction Energy:\n      Pattern          Intra  ");
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next) printf("%13s  ",p1->get_name());
	printf("\n");
	count1 = 0;
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next)
	{
		count2 = 0;
		// Calculate total intrapattern contribution
		energy_intra = vdw_intra[count1];
		switch (et)
		{
			case (EM_OFF):
				break;
			case (EM_COULOMB):
				energy_intra += coulomb_intra[count1];
				break;
			default: // Ewald
				energy_intra += ewald_real_intra[count1] + ewald_recip_intra[count1] - ewald_mol_correct[count1];
				break;
		}
		printf("%13s  %13.6e  ",p1->get_name(),energy_intra);
		// Calculate total interpattern contributions
		for (p2 = m->get_patterns(); p2 != NULL; p2 = p2->next)
		{
			energy_inter = vdw_inter[count1][count2];
			switch (et)
			{
				case (EM_OFF):
					break;
				case (EM_COULOMB):
					energy_inter += coulomb_inter[count1][count2];
					break;
				default:	 // Ewald
					energy_inter += ewald_real_inter[count1][count2] + ewald_recip_inter[count1][count2];
					if (count1 == count2) energy_inter -= ewald_self_correct[count1];
					else energy_inter += ewald_recip_inter[count1][count2];
					break;
			}
			printf("%13.6e  ",energy_inter);
			count2 ++;
		}
		printf("\n");
		count1 ++;
	}
	dbg_end(DM_CALLS,"energystore::print_intermatrix");
}

// Print out intramolecular energy decomposition matrix
void energystore::print_intramatrix(model *m)
{
	dbg_begin(DM_CALLS,"energystore::print_intramatrix");
	int count1;
	pattern *p1;
	double energy;
	if (!calculated)
	{
		msg(DM_NONE,"energystore::print_intramatrix - Total energy has not yet been calculated.\n");
		dbg_end(DM_CALLS,"energystore::print_intramatrix");
		return;
	}
	// Print out VDW energy decomposition
	printf("Intramolecular Energy:\n     Pattern         Total       Per Mol         Bond         Angle       Torsion \n");
	count1 = 0;
	for (p1 = m->get_patterns(); p1 != NULL; p1 = p1->next)
	{
		energy = bond[count1] + angle[count1] + torsion[count1];
		printf("%13s  %13.6e  %13.6e  %13.6e  %13.6e  %13.6e\n",p1->get_name(),energy, energy/p1->get_nmols(), bond[count1], angle[count1], torsion[count1]);
		count1 ++;
	}
	dbg_end(DM_CALLS,"energystore::print_intramatrix");
}
