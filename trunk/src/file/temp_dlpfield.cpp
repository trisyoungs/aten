/*
	*** Temporary routine to export DL_POLY field file
	*** src/file/temp_dlpfield.cpp
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

#include "model/model.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"
#include "base/elements.h"
#include "file/parse.h"
#include "file/format.h"
#include "base/prefs.h"
#include "base/sysfunc.h"

bool savedlpfield(const char *filename, model *sourcemodel)
{
	// Write out a DL_POLY FIELD file...
	dbg_begin(DM_CALLS,"fileio::save_dlpfield");
	atom *i;
	int keytrj, n;
	char s[80], temps[10];
	pattern *p;
	reflist<ffatom> vdwtypes;
	refitem<ffatom> *ri, *rj;
	patbound *pb;
	ffparams params;
	forcefield *pff;
	ffatom *ffa;
	// To write a FIELD file we need both a correct pattern definition and energy expression...
	if (!sourcemodel->autocreate_patterns())
	{
		msg(DM_NONE,"Can't write FIELD file without a valid pattern definition!\n");
		dbg_end(DM_CALLS,"save_dlpfield");
		return FALSE;
	}
	if (!sourcemodel->create_expression())
	{
		msg(DM_NONE,"Can't write FIELD file without a valid energy expression!\n");
		dbg_end(DM_CALLS,"save_dlpfield");
		return FALSE;
	}
	ofstream fieldfile(filename,ios::out);
	// First line is header - this will be the current model name
	//sprintf(s,"%s\n",name.c_str());
	fieldfile << sourcemodel->get_name() << "\n";
	// Energy units used in forcefield
	fieldfile << "UNITS  kj\n";
	// Number of molecules (pattern nodes)
	fieldfile << "MOLECULES  " << sourcemodel->get_npatterns() << "\n";
	// Loop over molecules (pattern nodes) and write out forcefield terms
	for (p = sourcemodel->get_patterns(); p != NULL; p = p->next)
	{
		// Grab forcefield for individual pattern, if there is one
		pff = p->get_ff();
		if (pff == NULL) pff = sourcemodel->get_ff();
		// Molecule name
		fieldfile << p->get_name() << "\n";
		// Number of molecules
		fieldfile << "NUMMOLS  " << p->get_nmols() << "\n";
		// Atoms in molecule
		fieldfile << "ATOMS  " << p->get_natoms() << "\n";
		i = p->get_firstatom();
		for (n=0; n<p->get_natoms(); n++)
		{
			ffa = i->get_type();
			if (ffa == NULL)
			{
				msg(DM_NONE,"Warning : No forcefield type associated with atom '%i' of pattern '%s'. Using element...\n",n,p->get_name());
				strcpy(temps,elements.symbol(i));
			}
			else
			{
				strcpy(temps,ffa->get_name());
				// Add this type (uniquely) to the list of vdw types
				vdwtypes.add_unique(ffa,0,0);
			}
			sprintf(s,"%8s    %10.4f  %10.6f   1    0    1\n",temps,elements.mass(i),i->get_charge());
			fieldfile << s;
			i = i->next;
		}
		// Bond definitions
		fieldfile << "BONDS  " << p->bonds.size() << "\n";
		for (pb = p->bonds.first(); pb != NULL; pb = pb->next)
		{
			params = pb->get_data()->get_params();
			switch (pb->get_data()->get_funcform().bondfunc)
			{
				case (BF_HARMONIC):
					sprintf(s,"harm %4i %4i   %9.4f  %9.6f\n", pb->get_atomid(0)+1, pb->get_atomid(1)+1, params.data[BF_HARMONIC_K], params.data[BF_HARMONIC_EQ]);
					break;
			}
			fieldfile << s;
		}
		// Angle definitions
		fieldfile << "ANGLES  " << p->angles.size() << "\n";
		for (pb = p->angles.first(); pb != NULL; pb = pb->next)
		{
			params = pb->get_data()->get_params();
			switch (pb->get_data()->get_funcform().anglefunc)
			{
				case (AF_HARMONIC):
					sprintf(s,"harm %4i %4i %4i   %9.4f  %9.6f\n", pb->get_atomid(0)+1, pb->get_atomid(1)+1, pb->get_atomid(2)+1, params.data[AF_HARMONIC_K], params.data[AF_HARMONIC_EQ]);
					break;
			}
			fieldfile << s;
		}
		// Torsion definitions
		fieldfile << "DIHEDRALS  " << p->torsions.size() << "\n";
		for (pb = p->torsions.first(); pb != NULL; pb = pb->next)
		{
			params = pb->get_data()->get_params();
			switch (pb->get_data()->get_funcform().torsionfunc)
			{
				case (TF_COSINE):
					sprintf(s,"cos %4i %4i %4i %4i   %9.5f  %7.2f  %4.1f  %7.4f  %7.4f\n", pb->get_atomid(0)+1, pb->get_atomid(1)+1, pb->get_atomid(2)+1, pb->get_atomid(3)+1, params.data[TF_COSINE_K], params.data[TF_COSINE_EQ], params.data[TF_COSINE_P], params.data[TF_ESCALE], params.data[TF_ESCALE]);
					break;
				case (TF_COS3):
					sprintf(s,"cos3 %4i %4i %4i %4i   %9.5f  %9.5f  %9.5f  %7.4f  %7.4f\n", pb->get_atomid(0)+1, pb->get_atomid(1)+1, pb->get_atomid(2)+1, pb->get_atomid(3)+1, params.data[TF_COS3_K1], params.data[TF_COS3_K2], params.data[TF_COS3_K3], params.data[TF_ESCALE], params.data[TF_ESCALE]);
					break;
			}
			fieldfile << s;
		}
		fieldfile << "FINISH\n";
	}
	// Now write VDW interaction pairs
	int nvdw = 0;
	n = 0;
	// Get total number...
	for (ri = vdwtypes.first(); ri != NULL; ri = ri->next)
	{
		n ++;
		nvdw += n;
	}
	fieldfile << "VDW  " << nvdw << "\n";
	// TODO Assume that VDW form of first pattern/forcefield is representative of all.
	pff = sourcemodel->get_patterns()->get_ff();
	double v1, v2, v3;
	ffparams paramsi, paramsj;
	if (pff == NULL) pff = sourcemodel->get_ff();
	for (ri = vdwtypes.first(); ri != NULL; ri = ri->next)
		for (rj = ri; rj != NULL; rj = rj->next)
		{
			paramsi = ri->item->get_params();
			paramsj = rj->item->get_params();
			switch (ri->item->get_style())
			{
				case (VF_LJ):
					v1 = sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
					v2 = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]);
					sprintf(s,"%8s %8s lj         %10.6f  %10.6f\n", ri->item->get_name(), rj->item->get_name(), v1, v2);
					fieldfile << s;
					break;
			}
		}
	fieldfile << "CLOSE";
	fieldfile.close();
	dbg_end(DM_CALLS,"fileio::save_dlpfield");
	return TRUE;
}

