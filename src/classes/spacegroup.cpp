/*
	*** Crystal spacegroups / generators
	*** src/classes/spacegroup.cpp
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

#include "classes/spacegroup.h"
#include "classes/cell.h"
#include "parse/format.h"
#include "parse/parser.h"

spacegroup_list spacegroups;

// Constructors
symmop::symmop()
{
	next = NULL;
	prev = NULL;
	identity = TRUE;
	#ifdef MEMDEBUG
		memdbg.create[MD_SYMMOP] ++;
	#endif
}

spacegroup::spacegroup()
{
	#ifdef MEMDEBUG
		memdbg.create[MD_SPACEGROUP] ++;
	#endif
}

spacegroup_list::spacegroup_list()
{
	#ifdef MEMDEBUG
		printf("Constructor : spacegroup_list\n");
	#endif
}

// Destructors
symmop::~symmop()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_SYMMOP] ++;
	#endif
}

spacegroup::~spacegroup()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_SPACEGROUP] ++;
	#endif
}

spacegroup_list::~spacegroup_list()
{
	#ifdef MEMDEBUG
		printf(" Destructor : spacegroup_list\n");
	#endif
}

/*
// Generators
*/

// Set from custom string
void symmop::set(const char *xyzstr)
{
	// Sets the rotation matrix and translation vector.
	// String format is, for example, "-y,x,z,0.0,0.0,0.5", corresponding to a matrix whose rows are (0,-1,0), (1,0,0), and (0,0,1) respectively,
	// and a translation vector of (0,0,0.5)
	dbg_begin(DM_CALLS,"symmop::set");
	static int n,m,i;
	static dnchar temps, s;
	static char dat[32];
	s = xyzstr;
	vec3<double> v;
	char *c;
	// First three args are x, y, and z vectors in the rotation matrix
	for (n=0; n<3; n++)
	{
		strcpy(dat,parser.get_next_delim(s,PO_DEFAULTS));
		i = 1;
		v.zero();
		//for (m=0, c=dat[m]; c != '\0'; m++, c = dat[m])
		for (c = dat; *c != '\0'; c++)
		{
			switch (*c)
			{
				case ('-'):
					i = -1;
					break;
				case ('x'):	// ascii = 120
				case ('y'):
				case ('z'):
					// Construct unit vector, and set in the matrix
					v.set(*c-120,i);
					rotation.set(n,v.x,v.y,v.z);
					break;
				default:
					printf("Unrecognised character '%c' found in symmop spec.\n",*c);
					break;
			}
		}
	}
	// Store the translation vector (if one was given), otherwise assume (0,0,0)
	translation.set(0.0,0.0,0.0);
	for (n=3; n<6; n++)
	{
		temps = parser.get_next_delim(s,PO_DEFAULTS);
		if (temps.empty()) break;
		translation.set(n-3,atof(temps.get()));
	}
	identity = FALSE;
	dbg_end(DM_CALLS,"symmop::set");
}

// Set from xyz string
void symmop::set_from_xyz(const char *xyzstr)
{
	// Sets the rotation matrix and translation vector.
	// String format is, for example, "-y,x,z+1/2", corresponding to a matrix whose rows are (0,-1,0), (1,0,0), and (0,0,1) respectively,
	// and a translation vector of (0,0,0.5). Detect if an identity transformation (x,y,z,0,0,0) is passed and flag 'identity' variable.	dbg_begin(DM_CALLS,"symmop::set_from_xyz");
	int n,m,i,pos;
	dnchar temps, s, dat, temp2;
	s = xyzstr;
	vec3<double> v;
	char c;
	// Slightly different approach. First  find the x/y/z and any + or - coming before it. Remove these from the string,
	// and whatever remains should be the translation component.
	for (n=0; n<3; n++)
	{
		temps = parser.get_next_delim(s,PO_DEFAULTS);
		dat = temps;
		i = 1;
		v.zero();
		pos = -1;
		// Find x, y, or z in the string. Get the sign (i) as well.
		for (m=0, c=dat[m]; c != '\0'; m++, c = dat[m])
		{
			switch (c)
			{
				case ('+'):
					i = 1;
					break;
				case ('-'):
					i = -1;
					break;
				case ('x'):
				case ('y'):
				case ('z'):
					// Identity check - if (c-120 != n) then identity == FALSE
					if (((c-120) != n) || (i == -1)) identity = FALSE;
					v.set(c-120,i);
					rotation.set(n,v.x,v.y,v.z);
					pos = m;
					break;
			}
			if (pos != -1) break;
		}
		// Have the position of the character in 'pos'. Remove this (and any sign before it) from the string
		temps.erasestart(1);
		if (pos != 0)
		{
			if ((temps[pos-1] == '-') || (temps[pos-1] == '+')) temps.erase(pos-1,pos-1);
		}
		if (!temps.empty())
		{
			// There is a translation component left over in the string.
			// Either a decimal value, or a fraction. Search for '/' to find out which.
			identity = FALSE;
			pos = temps.find('/');
			if (pos == -1) translation.set(n,temps.as_double());
			else
			{
				printf("PANIC - this isn't finished....\n");
				// Put first value in temp
				//temps.cutstart(pos,temp2);


				// translation.set(n,atof(temps.substr(0,pos)) /
				// atof(temps.substr(pos+1,temps.length()-pos)));
			}
		}
	}
	// Print out a message if we detected this was the identity operator
	if (identity) msg(DM_NONE,"Added an identity operator - will be ignored in symmetry transformation.\n");
	dbg_end(DM_CALLS,"symmop::set_from_xyz");
}

/*
// Spacegroups
*/

// Load
void spacegroup_list::load(const char *filename)
{
	dbg_begin(DM_CALLS,"spacegroup_list::load");
	int success,n,i;
	symmop *so;
	ifstream spgrpfile(filename,ios::in);
	if (!spgrpfile.good())
	{
		printf("Couldn't open spacegroup definitions. Have you set $ATENDATA?\n");
		spgrpfile.close();
		dbg_end(DM_CALLS,"spacegroup_list::load");
		return;
	}
	// Attempt to read in the spacegroup definitions...
	for (n=0; n<NSPACEGROUPS; n++)
	{
		success = parser.get_args_delim(&spgrpfile,PO_SKIPBLANKS+PO_USEQUOTES);
		if (success != 0)
		{
			if (success == 1) printf("Error reading spacegroup %i.\n",n);
			else if (success == -1) printf("Premature end of spacegroup definitions at number %i.\n",n);
			spgrpfile.close();
			dbg_end(DM_CALLS,"spacegroup_list::load");
			return;
		}
		// Store the name of the spacegroup
		strcpy(groups[n].name,parser.argc(1));
		// Store symmetry operators
		for (i=2; i<parser.get_nargs(); i++)
		{
			so = groups[n].symmops.add();
			so->set(parser.argc(i));
		}
	}
	spgrpfile.close();
	dbg_end(DM_CALLS,"spacegroup_list::load");
}

// Name search
int spacegroup_list::find_by_name(const char *name)
{
	dbg_begin(DM_CALLS,"spacegroup_list::find_by_name");
	int result = 0;
	for (int n=1; n<NSPACEGROUPS; n++)
		if (groups[n].name == name)
		{
			result = n;
			break;
		}
	dbg_end(DM_CALLS,"spacegroup_list::find_by_name");
	return result;
}

// Cell type from spacegrgoup
cell_type spacegroup_list::get_cell_type(int sg)
{
	dbg_begin(DM_CALLS,"spacegroup_list::get_cell_type");
	cell_type result = CT_NONE;
	// None
	if (sg == 0) result = CT_NONE;
	// Triclinic and monoclinic
	else if (sg < 16) result = CT_PARALLELEPIPED;
	// Orthorhombic and tetragonal
	else if (sg < 143) result = CT_ORTHORHOMBIC;
	// Trigonal
	else if (sg < 168) result = CT_PARALLELEPIPED;
	// Hexagonal
	else if (sg < 195) result = CT_NONE;
	// Cubic
	else result = CT_CUBIC;
	dbg_begin(DM_CALLS,"spacegroup_list::get_cell_type");
	return result;
}
