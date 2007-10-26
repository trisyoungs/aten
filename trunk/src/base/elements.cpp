/*
	*** Element definitions
	*** src/base/elements.cpp
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

#include "base/elements.h"
#include "base/prefs.h"
#include "base/master.h"
#include "base/debug.h"
#include "base/sysfunc.h"
#include "file/parse.h"
#include "classes/forcefield.h"

element_map elements;

/*
// Default Element Data
*/

// Format: ID   Mass    Name            Symbol  Radius  R       G       B       Valency 
const char *elementdata[] = {
	"0	0.000	Dummy		XX	0.500	0	0.5,0.5,0.5		0.375,0.375,0.375",
	"1	1.008	Hydrogen	H	0.320	1	0.87,0.87,0.87		0.78,0.78,0.78",
	"2	4.003	Helium		He	0.310	1	1.0,0.784,0.784		0.750,0.588,0.588",
	"3	6.941	Lithium		Li	1.630	1	0.647,0.165,0.165	0.485,0.124,0.124",
	"4	9.012	Beryllium	Be	0.900	1	0.0,0.0,0.0		0.000,0.000,0.000",
	"5	10.811	Boron		B	0.820	0	0.0,1.0,0.0		0.000,0.750,0.000",
	"6	12.011	Carbon		C	0.770	4	0.0,1.0,0.2		0.000,0.750,0.150",
	"7	14.007	Nitrogen	N	0.750	3	0.561,0.561,1.0		0.421,0.421,0.750",
	"8	15.999	Oxygen		O	0.730	2	1.0,0.0,0.0		0.750,0.000,0.000",
	"9	18.998	Fluorine	F	0.720	1	0.784,0.647,0.094	0.588,0.485,0.071",
	"10	20.180	Neon		Ne	0.710	0	0.0,0.0,0.0		0.000,0.000,0.000",
	"11	22.990	Sodium		Na	1.540	1	0.0,0.0,1.0		0.000,0.000,0.750",
	"12	24.305	Magnesium	Mg	1.360	0	0.0,0.0,0.0		0.000,0.000,0.000",
	"13	26.982	Aluminium	Al	1.180	3	0.561,0.561,1.0		0.421,0.421,0.750",
	"14	28.085	Silicon		Si	1.110	4	0.784,0.647,0.094	0.588,0.485,0.071",
	"15	30.974	Phosphorous	P	1.060	1	1.0,0.647,0.0		0.750,0.485,0.000",
	"16	32.066	Sulfur		S	1.020	2	1.0,0.784,0.196		0.750,0.588,0.147",
	"17	35.453	Chlorine	Cl	0.990	1	0.0,1.0,0.0		0.000,0.750,0.000",
	"18	39.948	Argon		Ar	0.980	0	0.0,1.0,0.0		0.000,0.750,0.000",
	"19	39.098	Potassium	K	2.030	1	0.0,1.0,0.0		0.000,0.750,0.000",
	"20	40.078	Calcium		Ca	1.740	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"21	44.956	Scandium	Sc	1.440	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"22	47.880	Titanium	Ti	1.320	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"23	50.941	Vanadium	V	1.220	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"24	51.996	Chromium	Cr	1.180	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"25	54.938	Manganese	Mn	1.170	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"26	55.847	Iron		Fe	1.170	0	1.0,0.647,0.0		0.750,0.485,0.000",
	"27	58.933	Cobalt		Co	1.160	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"28	58.693	Nickel		Ni	1.150	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"29	63.546	Copper		Cu	1.170	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"30	65.390	Zinc		Zn	1.250	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"31	69.723	Gallium		Ga	1.260	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"32	72.610	Germanium	Ge	1.220	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"33	74.922	Arsenic		As	1.200	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"34	78.960	Selenium	Se	1.160	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"35	79.904	Bromine		Br	1.140	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"36	83.800	Krypton		Kr	1.120	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"37	85.468	Rubidium	Rb	2.160	0	0.0,1.0,0.0		0.000,0.750,0.000",
	"38	87.620	Strontium	Sr	1.910	0	0.0,1.0,0.0		0.000,0.750,0.000",
	"39	88.906	Yttrium		Y	1.620	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"40	91.224	Zirconium	Zr	1.450	0	0.784,0.647,0.094	0.588,0.485,0.071",
	"41	92.906	Niobium		Nb	1.340	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"42	95.940	Molybdenum	Mo	1.300	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"43	97.907	Technetium	Tc	1.270	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"44	101.070	Ruthenium	Ru	1.250	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"45	102.906	Rhodium		Rh	1.250	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"46	106.420	Palladium	Pd	1.280	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"47	107.870	Silver		Ag	1.340	0	0.6,0.6,0.6		0.450,0.450,0.450",
	"48	112.411	Cadmium		Cd	1.480	0	0.0,1.0,0.0		0.000,0.750,0.000",
	"49	114.818	Indium		In	1.440	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"50	118.710	Tin		Sn	1.410	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"51	121.760	Antimony	Sb	1.400	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"52	127.600	Tellurium	Te	1.360	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"53	129.905	Iodine		I	1.330	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"54	131.290	Xenon		Xe	1.310	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"55	132.905	Caesium		Cs	2.350	0	0.647,0.165,0.165	0.485,0.124,0.124",
	"56	137.327	Barium		Ba	1.980	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"57	138.905	Lanthanum	La	1.690	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"58	140.115	Cerium		Ce	1.650	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"59	140.908	Praesodymium	Pr	1.650	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"60	144.240	Neodymium	Ns	1.840	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"61	144.913	Prometheum	Pm	1.630	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"62	150.360	Samarium	Sm	1.620	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"63	151.965	Europium	Eu	1.850	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"64	157.250	Gadolinium	Gd	1.610	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"65	158.925	Terbium		Tb	1.590	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"66	162.500	Dysprosium	Dy	1.590	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"67	164.930	Holmium		Ho	1.580	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"68	167.260	Erbium		Er	1.570	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"69	168.934	Thulium		Tm	1.560	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"70	173.040	Ytterbium	Yb	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"71	174.967	Lutetium	Lu	1.560	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"72	178.490	Hafnium		Hf	1.440	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"73	180.948	Tantalum	Ta	1.340	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"74	183.840	Tungsten	W	1.300	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"75	186.207	Rhenium		Re	1.280	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"76	190.230	Osmium		Os	1.260	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"77	192.220	Iridium		Ir	1.270	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"78	195.080	Platinum	Pt	1.300	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"79	196.967	Gold		Au	1.340	0	0.784,0.647,0.094	0.588,0.485,0.071",
	"80	200.590	Mercury		Hg	1.490	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"81	204.383	Thallium	Tl	1.480	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"82	207.200	Lead		Pb	1.470	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"83	208.980	Bismuth		Bi	1.460	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"84	208.982	Polonium	Po	1.460	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"85	209.987	Astatine	At	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"86	222.018	Radon		Rn	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"87	223.020	Francium	Fr	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"88	226.025	Radon		Ra	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"89	227.028	Actinium	Ac	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"90	232.038	Thorium		Th	1.650	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"91	231.036	Protactinium	Pa	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"92	238.029	Uranium		U	1.420	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"93	237.048	Neptunium	Np	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"94	244.064	Plutonium	Pu	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"95	243.061	Americium	Am	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"96	247.070	Curium		Cm	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"97	247.070	Berkelium	Bk	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"98	251.080	Californium	Cf	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"99	252.083	Einsteinium	Es	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"100	257.095	Fermium		Fm	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"101	258.100	Mendelevium	Md	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"102	259.101	Nobelium	No	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"103	262.110	Lawrencium	Lr	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"104	261	Rutherfordium	Rf	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"105	262	Dubnium		Db	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"106	266	Seaborgium	Sg	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"107	264	Bohrium		Bh	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"108	269	Hassium		Hs	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"109	268	Meitnerium	Mt	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"110	281	Darmstadtium	Ds	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"111	272	Roentgenium	Rg	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"112	285	Ununium		Uub	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"113	284	Ununtrium	Uut	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"114	289	Ununquadium	Uuq	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"115	288	Ununpentium	Uup	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"116	292	Ununhexium	Uuh	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"117	0.0	Ununseptium	Uus	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"118	0.0	Ununoctium	Uuo	2.000	0	1.0,1.0,1.0		0.750,0.750,0.750",
	"119	0.0	Ellipsoid	ELP	0.0	0	0.0,0.0,1.0		0.000,0.000,0.750" };

// Constructors
element::element()
{
	#ifdef MEMDEBUG
		memdbg.create[MD_ELEMENT] ++;
	#endif
}

element_map::element_map()
{
	#ifdef MEMDEBUG
		printf("Constructor : element_map\n");
	#endif
}

// Destructors
element::~element()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_ELEMENT] ++;
	#endif
}

element_map::~element_map()
{
	#ifdef MEMDEBUG
		printf(" Destructor : element_map\n");
	#endif
}

// Initialise the element map
void element_map::initialise()
{
	// Load in the elemental data from the file 'elements.dat'
	dbg_begin(DM_CALLS,"element_map::initialise");
	int err,n,m;
	// Go through each string held in elementdata[], and parse it into values
	for (n=0; n<NELEMENTS; n++)
	{
		parser.get_args_delim(elementdata[n],PO_SKIPBLANKS);
		el[n].mass = parser.argd(1);
		strcpy(el[n].name,parser.argc(2));
		strcpy(el[n].symbol,parser.argc(3));
		strcpy(el[n].ucsymbol,upper_case(parser.argc(3)));
		el[n].radius = parser.argd(4);
		el[n].valency = parser.argi(5);
		// Colours are stored as floating-point values - convert these to span an integer of 0 - INT_MAX.
		el[n].ambient[0] = (GLint) (parser.argd(6) * INT_MAX);
		el[n].ambient[1] = (GLint) (parser.argd(7) * INT_MAX);
		el[n].ambient[2] = (GLint) (parser.argd(8) * INT_MAX);
		el[n].ambient[3] = INT_MAX;
		el[n].diffuse[0] = (GLint) (parser.argd(9) * INT_MAX);
		el[n].diffuse[1] = (GLint) (parser.argd(10) * INT_MAX);
		el[n].diffuse[2] = (GLint) (parser.argd(11) * INT_MAX);
		el[n].diffuse[3] = INT_MAX;
	}
	dbg_end(DM_CALLS,"element_map::initialise");
}

// Return ambient colour in supplied vector
void element_map::ambient(int i, GLint *v)
{
	v[0] = el[i].ambient[0];
	v[1] = el[i].ambient[1];
	v[2] = el[i].ambient[2];
	v[3] = el[i].ambient[3];
}

// Return diffuse colour in supplied vector
void element_map::diffuse(int i, GLint *v)
{
	v[0] = el[i].diffuse[0];
	v[1] = el[i].diffuse[1];
	v[2] = el[i].diffuse[2];
	v[3] = el[i].diffuse[3];
}

// Convert string from Z to element number
int element_map::number_to_z(const char *s)
{
	// Check that the string is entirely numerical
	bool isnumber = TRUE;
	for (int n=0; s[n] != '\0'; n++)
		if ((s[n] < 48) || (s[n] > 57))
		{
			isnumber = FALSE;
			break;
		}
	if (isnumber) return atoi(s);
	else return -1;
}

// Convert string from alpha to element number
int element_map::alpha_to_z(const char *s)
{
	// Ignore numbers. Convert up to non-alpha character.
	static char cleaned[32];
	int n, len = 0, result = -1;
	for (n=0; s[n] != '\0'; n++)
		if (s[n] > 64 && s[n] < 91) { cleaned[len] = s[n]; len++; }
		else if (s[n] > 96 && s[n] < 123) { cleaned[len] = toupper(s[n]); len++; }
		else if (s[n] == '_') break;
	cleaned[len] = '\0';
	for (n=0; n<NELEMENTS; n++)
		if (strcmp(el[n].ucsymbol,cleaned) == 0) 
		{
			result = n;
			break;
		}
	return result;
}

// Convert string from fftype to element number
int element_map::ff_to_z(const char *s)
{
	ffatom *ffa;
	int result = -1;
	for (forcefield *ff = master.get_ffs(); ff != NULL; ff = ff->next)
	{
		ffa = ff->find_type(s);
		if (ffa != NULL)
		{
			// Found a match, so find out what element it is...
			result = ffa->get_atomtype()->el;
			break;
		}
		if (result != -1) break;
	}
	return result;
}

// Search for element named 'query' in the list of known elements
int element_map::find(const char *query)
{
	// Get the element number from the element name provided.
	dbg_begin(DM_CALLS,"element_map::find");
	int result = -1;
	if (query[0] == '\0')
	{
		printf("Warning: find_el was given a zero-length string.\n");
		dbg_end(DM_CALLS,"element_map::find");
		return 0;
	}
	// Convert the query string according to the specified rule
	switch (prefs.get_zmapping())
	{
		// Automatic determination
		case (ZM_AUTO):
			// First, try pure numeric conversion
			result = number_to_z(query);
			if (result != -1) break;
			// Then, try alpha conversion
			result = alpha_to_z(query);
			if (result != -1) break;
			// Finally, try FF conversion
			result = ff_to_z(query);
			break;
		// Search loaded forcefields for atom names
		case (ZM_FORCEFIELD):
			result = ff_to_z(query);
			// Attempt an alpha conversion if the FF conversion failed
			if (result == -1) result = alpha_to_z(query);
			break;
		// Convert based on alpha-part of atom name only
		case (ZM_ALPHA):
			result = alpha_to_z(query);
			break;
		// Convert based on numeric part only
		case (ZM_NUMERIC):
			result = number_to_z(query);
			break;
	}
	dbg_end(DM_CALLS,"element_map::find");
	return ((result == -1) ? 0 : result);
}

// Search for element named 'query' in the list of known elements, using the specified algorithm
int element_map::find(const char *query, zmap_type zmt)
{
	// Store the old zmapping type, and temporarily set a new one
	zmap_type last = prefs.get_zmapping();
	prefs.set_zmapping(zmt);
	int result = find(query);
	prefs.set_zmapping(last);
	return result;
}

