/*
// Gamess US Import
*/

#include "model/model.h"
#include "classes/config.h"
#include "file/format.h"
#include "file/parse.h"
#include "base/prefs.h"
#include "base/sysfunc.h"

bool fileio::read_gamessus(const char *filename)
{
	dbg_begin(DM_CALLS,"fileio::read_gamessus");
	// Read in data from a Gamess US output file
	atom *i;
	int n, success, nsets, atomsinfile;
	bool done;
	// Create formats
	format gamessus_natoms, gamessus_atom;
	gamessus_natoms.create("* * * * * i");
	gamessus_atom.create("e * x y z");
	msg(DM_NONE,"Type   : GAMESS-US output file\n");
	ifstream gamessfile(filename,ios::in);
	// Number of atoms is given in line " TOTAL NUMBER OF ATOMS = xx"
	// Each set of coordinates is preceeded by " COORDINATES OF ALL ATOMS" and then two skippable lines
	// First, find number of atoms in file...
	done = FALSE;
	do
	{
		if (parser.get_line(&gamessfile,PO_SKIPBLANKS) != 0)
		{
			msg(DM_NONE,"Error reading line from GAMESS file.\n");
			dbg_end(DM_CALLS,"fileio::read_gamessus");
			return FALSE;
		}
		if (parser.line_contains(" TOTAL NUMBER OF ATOMS"))
		{
			// Found the line containing the number of atoms, so parse it (formatted) and get natoms
			gamessus_natoms.parse_line(PO_DEFAULTS);
			atomsinfile = gamessus_natoms.get_i();
			done = TRUE;
		}
	}
	while (!done);
	// Search through file, looking for coordinate sets.
	// Load each into the modelspace (after clearing it) and then copy to a configuration node if we're caching
	// the trajectory as well.
	nsets = 0;
	done = FALSE;
	do
	{
		if (parser.get_line(&gamessfile,TRUE) != 0) done = TRUE;
		else
		{
			if (parser.line_contains(" COORDINATES OF ALL ATOMS"))
			{
				// Found a configuration, so increase counter and read into model...
				nsets ++;
				targetmodel->clear_atoms();
				// Skip two lines to get to atomic data
				parser.skip_lines(&gamessfile,2);
				for (n=0; n<atomsinfile; n++)
				{
					if (gamessus_atom.parse_file(&gamessfile,PO_DEFAULTS) != 0)
					{
						msg(DM_NONE,"Error reading atom %i of configuration %i.\n",n+1,nsets);
						dbg_end(DM_CALLS,"fileio::read_gamessus");
						return FALSE;
					}
					i = targetmodel->add_atom(gamessus_atom.get_el());
					i->set_coords(gamessus_atom.get_r());
				}
				// Cache geometry optimisation steps
				if (prefs.load_all_coords())
				{
					targetmodel->traj.set_cached(TRUE);
					config *newframe = targetmodel->traj.add_frame();
					targetmodel->copy_to_config(newframe,FALSE,CFG_R);
				}
			}
		}
	}
	while (!done);
	msg(DM_NONE,"      : %i geometries in file.\n",nsets);
	targetmodel->traj.set_totalframes(nsets);
	finalise_model(MF_GAMESSUS,filename);
	dbg_end(DM_CALLS,"fileio::read_gamessus");
	return TRUE;
}
