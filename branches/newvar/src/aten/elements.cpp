/*
	*** Element definitions
	*** src/base/elements.cpp
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

#include "base/elements.h"
#include "base/prefs.h"
#include "base/aten.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include "parse/parser.h"
#include "classes/forcefield.h"

// Singleton declaration
ElementMap elements;

// Definitions
#define LANTHANIDES 98
#define ACTINIDES 99

// ZMapping types
const char *ZmapTypeKeywords[Prefs::nZmapTypes] = { "alpha", "firstalpha", "name", "numeric", "ff", "auto" };
Prefs::ZmapType Prefs::zmapType(const char *s)
{
	return (Prefs::ZmapType) enumSearch("element mapping style", Prefs::nZmapTypes, ZmapTypeKeywords, s);
}

/*
// Default Element Data
// Element radii taken from:
//	"Covalent radii revisited", 
//	B. Cordero, V. Gómez, A. E. Platero-Prats, M. Revés, J. Echeverría, E. Cremades, F. Barragán and S. Alvarez
//	Dalton Trans., 2008 (DOI: 10.1039/b801115j)
//	Notes: High-Spin radii taken for 1st transition elements, sp3 value taken for Carbon. Bk and beyond set to 1.50.
//
// Lutetium and Lawrencium are assigned to group 3, with Lanthanum and Actinium the first elements in the lanthanide and actinide series.
*/

//	  Mass  	Name         		Symbol			Group	Radius  Vlncy	AmbientRGBA		DiffuseRGBA
Element ElementMap::el_[] = {
	{ 0.000,	"Dummy","DUMMY",	"XX","XX",		0,	0.00,	0.5f,0.5f,0.5f,1.0f,		0.375f,0.375f,0.375f,1.0f,
		0,0,0,0,0,0,0,0,0,	0,0,0,0,0,0,0,0,0 },
	{ 1.008,	"Hydrogen","HYDROGEN",	"H","H",		1,	0.31,	0.87f,0.87f,0.87f,1.0f,		0.78f,0.78f,0.78f,1.0f,
		2,0,16,32,32,32,32,32,32,	1,0,-1,0,0,0,0,0,0 },
	{ 4.003,	"Helium","HELIUM",	"He","HE",		18,	0.28,	1.0f,0.784f,0.784f,1.0f,	0.75f,0.588f,0.588f,1.0f,
		8,0,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 6.941,	"Lithium","LITHIUM",	"Li","LI",		1,	1.28,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		2,0,32,32,32,32,32,32,32,	1,0,0,0,0,0,0,0,0 },
	{ 9.012,	"Beryllium","BERYLLIUM","Be","BE",		2,	0.96,	0.0f,0.0f,0.0f,1.0f,		0.0f,0.0f,0.0f,1.0f,
		32,8,4,0,32,32,32,32,32,	2,1,0,0,0,0,0,0,0 },
	{ 10.811,	"Boron","BORON",	"B","B",		13,	0.84,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		32,16,8,0,32,32,32,32,32,	3,2,1,0,0,0,0,0,0 },
	{ 12.011,	"Carbon","CARBON",	"C","C",		14,	0.76,	0.0f,1.0f,0.2f,1.0f,		0.0f,0.75f,0.150f,1.0f,
		32,32,32,16,0,32,32,32,32,	-4,-3,-2,-1,0,1,0,0,0 },
	{ 14.007,	"Nitrogen","NITROGEN",	"N","N",		15,	0.71,	0.561f,0.561f,1.0f,1.0f,	0.421f,0.421f,0.75f,1.0f,
		32,32,16,0,8,32,32,32,32,	-3,-2,-1,0,1,0,0,0,0 },
	{ 15.999,	"Oxygen","OXYGEN",	"O","O",		16,	0.66,	1.0f,0.0f,0.0f,1.0f,		0.75f,0.0f,0.0f,1.0f,
		4,2,0,2,32,32,32,32,32,		-2,-1,0,1,0,0,0,0,0 },
	{ 18.998,	"Fluorine","FLUORINE",	"F","F",		17,	0.57,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f,
		2,0,32,32,32,32,32,32,32,	-1,0,0,0,0,0,0,0,0 },
	{ 20.180,	"Neon","NEON",		"Ne","NE",		18,	0.58,	0.0f,0.0f,0.0f,1.0f,		0.0f,0.0f,0.0f,1.0f,
		8,0,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 22.990,	"Sodium","SODIUM",	"Na","NA",		1,	1.66,	0.0f,0.0f,1.0f,1.0f,		0.0f,0.0f,0.75f,1.0f,
		2,0,32,32,32,32,32,32,32,	1,0,0,0,0,0,0,0,0 },
	{ 24.305,	"Magnesium","MAGNESIUM","Mg","MG",		2,	1.41,	0.0f,0.0f,0.0f,1.0f,		0.0f,0.0f,0.0f,1.0f,
		8,4,0,32,32,32,32,32,32,	2,1,0,0,0,0,0,0,0 },
	{ 26.982,	"Aluminium","ALUMINIUM","Al","AL",		13,	1.21,	0.561f,0.561f,1.0f,1.0f,	0.421f,0.421f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 28.085,	"Silicon","SILICON",	"Si","SI",		14,	1.11,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f,
		32,32,32,32,0,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 30.974,	"Phosphorous","PHOSPHOROUS","P","P",		15,	1.07,	1.0f,0.647f,0.0f,1.0f,		0.75f,0.485f,0.0f,1.0f,
		32,32,32,0,32,0,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 32.066,	"Sulfur","SULFUR",	"S","S",		16,	1.05,	1.0f,0.784f,0.196f,1.0f,	0.75f,0.588f,0.147f,1.0f,
		32,32,0,32,0,32,0,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 35.453,	"Chlorine","CHLORINE",	"Cl","CL",		17,	1.02,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		0,0,32,32,32,32,32,32,32,	-1,0,0,0,0,0,0,0,0 },
	{ 39.948,	"Argon","ARGON",	"Ar","AR",		18,	1.06,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 39.098,	"Potassium","POTASSIUM","K","K",		1,	2.03,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 40.078,	"Calcium","CALCIUM",	"Ca","CA",		2,	1.76,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 44.956,	"Scandium","SCANDIUM",	"Sc","SC",		3,	1.70,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 47.880,	"Titanium","TITANIUM",	"Ti","TI",		4,	1.60,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 50.941,	"Vanadium","VANADIUM",	"V","V",		5,	1.53,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 51.996,	"Chromium","CHROMIUM",	"Cr","CR",		6,	1.39,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 54.938,	"Manganese","MANGANESE","Mn","MN",		7,	1.61,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 55.847,	"Iron","IRON",		"Fe","FE",		8,	1.52,	1.0f,0.647f,0.0f,1.0f,		0.75f,0.485f,0.0f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 58.933,	"Cobalt","COBALT",	"Co","CO",		9,	1.50,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 58.693,	"Nickel","NICKEL",	"Ni","NI",		10,	1.24,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 63.546,	"Copper","COPPER",	"Cu","CU",		11,	1.32,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 65.390,	"Zinc","ZINC",		"Zn","ZN",		12,	1.22,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 69.723,	"Gallium","GALLIUM",	"Ga","GA",		13,	1.22,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 72.610,	"Germanium","GERMANIUM","Ge","GE",		14,	1.20,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 74.922,	"Arsenic","ARSENIC",	"As","AS",		15,	1.19,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 78.960,	"Selenium","SELENIUM",	"Se","SE",		16,	1.20,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 79.904,	"Bromine","BROMINE",	"Br","BR",		17,	1.20,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		0,0,32,32,32,32,32,32,32,	-1,0,0,0,0,0,0,0,0 },
	{ 83.800,	"Krypton","KRYPTON",	"Kr","KR",		18,	1.16,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 85.468,	"Rubidium","RUBIDIUM",	"Rb","RB",		1,	2.20,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 87.620,	"Strontium","STRONTIUM","Sr","SR",		2,	1.95,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 88.906,	"Yttrium","YTTRIUM",	"Y","Y",		3,	1.90,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 91.224,	"Zirconium","ZIRCONIUM","Zr","ZR",		4,	1.75,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 92.906,	"Niobium","NIOBIUM",	"Nb","NB",		5,	1.64,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 95.940,	"Molybdenum","MOLYBDENUM","Mo","MO",		6,	1.54,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 97.907,	"Technetium","TECHNETIUM","Tc","TC",		7,	1.47,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 101.070,	"Ruthenium","RUTHENIUM","Ru","RU",		8,	1.46,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 102.906,	"Rhodium","RHODIUM",	"Rh","RH",		9,	1.42,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 106.420,	"Palladium","PALLADIUM","Pd","PD",		10,	1.39,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 107.870,	"Silver","SILVER",	"Ag","AG",		11,	1.45,	0.6f,0.6f,0.6f,1.0f,		0.45f,0.45f,0.45f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 112.411,	"Cadmium","CADMIUM",	"Cd","CD",		12,	1.44,	0.0f,1.0f,0.0f,1.0f,		0.0f,0.75f,0.0f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 114.818,	"Indium","INDIUM",	"In","IN",		13,	1.42,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 118.710,	"Tin","TIN",		"Sn","SN",		14,	1.39,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 121.760,	"Antimony","ANTIMONY",	"Sb","SB",		15,	1.39,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 127.600,	"Tellurium","TELLURIUM","Te","TE",		16,	1.38,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 129.905,	"Iodine","IODINE",	"I","I",		17,	1.39,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		0,0,32,32,32,32,32,32,32,	-1,0,0,0,0,0,0,0,0 },
	{ 131.290,	"Xenon","XENON",	"Xe","XE",		18,	1.40,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 132.905,	"Caesium","CAESIUM",	"Cs","CS",		1,	2.44,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 137.327,	"Barium","BARIUM",	"Ba","BA",		2,	2.15,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 138.905,	"Lanthanum","LANTHANUM","La","LA",		98,	2.07,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 140.115,	"Cerium","CERIUM",	"Ce","CE",		98,	2.04,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 140.908,	"Praesodymium","PRAESODYMIUM","Pr","PR",	98,	2.03,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 144.240,	"Neodymium","NEODYMIUM","Ns","NS",		98,	2.01,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 144.913,	"Prometheum","PROMETHEUM","Pm","PM",		98,	1.99,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 150.360,	"Samarium","SAMARIUM",	"Sm","SM",		98,	1.98,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 151.965,	"Europium","EUROPIUM",	"Eu","EU",		98,	1.98,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 157.250,	"Gadolinium","GADOLINIUM","Gd","GD",		98,	1.96,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 158.925,	"Terbium","TERBIUM",	"Tb","TB",		98,	1.94,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 162.500,	"Dysprosium","DYSPROSIUM","Dy","DY",		98,	1.92,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 164.930,	"Holmium","HOLMIUM",	"Ho","HO",		98,	1.92,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 167.260,	"Erbium","ERBIUM",	"Er","ER",		98,	1.89,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 168.934,	"Thulium","THULIUM",	"Tm","TM",		98,	1.90,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 173.040,	"Ytterbium","YTTERBIUM","Yb","YB",		98,	1.87,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 174.967,	"Lutetium","LUTETIUM",	"Lu","LU",		3,	1.87,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 178.490,	"Hafnium","HAFNIUM",	"Hf","HF",		4,	1.75,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 180.948,	"Tantalum","TANTALUM",	"Ta","TA",		5,	1.70,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 183.840,	"Tungsten","TUNGSTEN",	"W","W",		6,	1.62,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 186.207,	"Rhenium","RHENIUM",	"Re","RE",		7,	1.51,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 190.230,	"Osmium","OSMIUM",	"Os","OS",		8,	1.44,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 192.220,	"Iridium","IRIDIUM",	"Ir","IR",		9,	1.41,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 195.080,	"Platinum","PLATINUM",	"Pt","PT",		10,	1.36,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 196.967,	"Gold","GOLD",		"Au","AU",		11,	1.36,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 200.590,	"Mercury","MERCURY",	"Hg","HG",		12,	1.32,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 204.383,	"Thallium","THALLIUM",	"Tl","TL",		13,	1.45,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 207.200,	"Lead","LEAD",		"Pb","PB",		14,	1.46,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 208.980,	"Bismuth","BISMUTH",	"Bi","BI",		15,	1.48,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 208.982,	"Polonium","POLONIUM",	"Po","PO",		16,	1.40,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 209.987,	"Astatine","ASTATINE",	"At","AT",		17,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 222.018,	"Radon","RADON",	"Rn","RN",		18,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 223.020,	"Francium","FRANCIUM",	"Fr","FR",		1,	2.60,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 226.025,	"Radon","RADON",	"Ra","RA",		2,	2.21,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 227.028,	"Actinium","ACTINIUM",	"Ac","AC",		99,	2.15,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 232.038,	"Thorium","THORIUM",	"Th","TH",		99,	2.06,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 231.036,	"Protactinium","PROTACTINIUM","Pa","PA",	99,	2.00,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 238.029,	"Uranium","URANIUM",	"U","U",		99,	1.96,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 237.048,	"Neptunium","NEPTUNIUM","Np","NP",		99,	1.90,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 244.064,	"Plutonium","PLUTONIUM","Pu","PU",		99,	1.87,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 243.061,	"Americium","AMERICIUM","Am","AM",		99,	1.80,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 247.070,	"Curium","CURIUM",	"Cm","CM",		99,	1.69,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 247.070,	"Berkelium","BERKELIUM","Bk","BK",		99,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 251.080,	"Californium","CALIFORNIUM","Cf","CF",		99,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 252.083,	"Einsteinium","EINSTEINIUM","Es","ES",		99,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 257.095,	"Fermium","FERMIUM",	"Fm","FM",		99,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 258.100,	"Mendelevium","MENDELEVIUM","Md","MD",		99,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 259.101,	"Nobelium","NOBELIUM",	"No","NO",		99,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 262.110,	"Lawrencium","LAWRENCIUM","Lr","LR",		3,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 261.000,	"Rutherfordium","RUTHERFORDIUM","Rf","RF",	4,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 262.000,	"Dubnium","DUBNIUM",	"Db","DB",		5,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 266.000,	"Seaborgium","SEABORGIUM","Sg","SG",		6,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 264.000,	"Bohrium","BOHRIUM",	"Bh","BH",		7,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 269.000,	"Hassium","HASSIUM",	"Hs","HS",		8,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 268.000,	"Meitnerium","MEITNERIUM","Mt","MT",		9,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 281.000,	"Darmstadtium","DARMSTADTIUM","Ds","DS",	10,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 272.000,	"Roentgenium","ROENTGENIUM","Rg","RG",		11,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 285.000,	"Ununium","UNUNIUM",	"Uub","UUB",		12,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 284.000,	"Ununtrium","UNUNTRIUM","Uut","UUT",		13,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 289.000,	"Ununquadium","UNUNQUADIUM","Uuq","UUQ",	14,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 288.000,	"Ununpentium","UNUNPENTIUM","Uup","UUP",	15,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 292.000,	"Ununhexium","UNUNHEXIUM","Uuh","UUH",		16,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 0.000,	"Ununseptium","UNUNSEPTIUM","Uus","UUS",	17,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 },
	{ 0.000,	"Ununoctium","UNUNOCTIUM","Uuo","UUO",		18,	1.50,	1.0f,1.0f,1.0f,1.0f,		0.75f,0.75f,0.75f,1.0f,
		32,32,32,32,32,32,32,32,32,	0,0,0,0,0,0,0,0,0 }
};

// Constructor
ElementMap::ElementMap()
{
	// Determine number of defined elements
	nElements_ = sizeof(el_) / sizeof(el_[0]);
}

// Return group number of atomic number 'i'
int ElementMap::group(Atom *i)
{
	return group(i->element());
}

// Return atomic mass of atomic number 'i'
double ElementMap::atomicMass(Atom *i)
{
	return atomicMass(i->element());
}

// Return name of atomic number 'i'
const char *ElementMap::name(Atom *i)
{
	return name(i->element());
}

// Return symbol of atomic number 'i'
const char *ElementMap::symbol(Atom *i)
{
	return symbol(i->element());
}

// Return effective radius of atomic number 'i'
double ElementMap::atomicRadius(Atom *i)
{
	return atomicRadius(i->element());
}

// Return bond order penalty for TBO 'bo' of atomic number 'i'
int ElementMap::bondOrderPenalty(Atom *i, int bo)
{
	return bondOrderPenalty(i->element(), bo);
}

// Return the ambient colour of the element
GLfloat *ElementMap::ambientColour(Atom *i)
{
	return ambientColour(i->element());
}

// Return the diffuse colour of the element
GLfloat *ElementMap::diffuseColour(Atom *i)
{
	return diffuseColour(i->element());
}

// Return number of defined elements
int ElementMap::nElements()
{
	return nElements_;
}

// Return group number of atomic number 'i'
int ElementMap::group(int i)
{
	return el_[i].group;
}

// Return atomic mass of atomic number 'i'
double ElementMap::atomicMass(int i)
{
	return el_[i].atomicMass;
}

// Return name of atomic number 'i'
const char *ElementMap::name(int i)
{
	return el_[i].name;
}

// Return symbol of atomic number 'i'
const char *ElementMap::symbol(int i)
{
	return el_[i].symbol;
}

// Set radius of atomic number 'i'
void ElementMap::setAtomicRadius(int i, double r)
{
	el_[i].atomicRadius = r;
}

// Return effective radius of atomic number 'i'
double ElementMap::atomicRadius(int i)
{
	return el_[i].atomicRadius;
}

// Return bond order penalty for TBO 'bo' of atomic number 'i'
int ElementMap::bondOrderPenalty(int i, int bo)
{
	return el_[i].bondOrderPenalty[bo];
}

// Return the ambient colour of the element
GLfloat *ElementMap::ambientColour(int i)
{
	return el_[i].ambientColour;
}

// Set ambient colour component of element
void ElementMap::setAmbientColour(int i, int rgb, GLfloat value)
{
	el_[i].ambientColour[rgb] = value;
}

// Set ambient colour component
void ElementMap::setAmbientColour(int i, GLfloat r, GLfloat g, GLfloat b)
{
	el_[i].ambientColour[0] = r;
	el_[i].ambientColour[1] = g;
	el_[i].ambientColour[2] = b;
}

// Return the diffuse colour of the element
GLfloat *ElementMap::diffuseColour(int i)
{
	return el_[i].diffuseColour;
}

// Set diffuse colour component of element
void ElementMap::setDiffuseColour(int i, int rgb, GLfloat value)
{
	el_[i].diffuseColour[rgb] = value;
}

void ElementMap::setDiffuseColour(int i, GLfloat r, GLfloat g, GLfloat b)
{
	el_[i].diffuseColour[0] = r;
	el_[i].diffuseColour[1] = g;
	el_[i].diffuseColour[2] = b;
}

// Return ambient colour in supplied vector
void ElementMap::copyAmbientColour(int i, GLfloat *v)
{
	v[0] = el_[i].ambientColour[0];
	v[1] = el_[i].ambientColour[1];
	v[2] = el_[i].ambientColour[2];
	v[3] = el_[i].ambientColour[3];
}

// Return diffuse colour in supplied vector
void ElementMap::copyDiffuseColour(int i, GLfloat *v)
{
	v[0] = el_[i].diffuseColour[0];
	v[1] = el_[i].diffuseColour[1];
	v[2] = el_[i].diffuseColour[2];
	v[3] = el_[i].diffuseColour[3];
}

// Convert string from Z to element number
int ElementMap::numberToZ(const char *s)
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
int ElementMap::alphaToZ(const char *s)
{
	// Ignore numbers. Convert up to non-alpha character.
	static char cleaned[32];
	int n, len = 0, result = -1;
	for (n=0; s[n] != '\0'; n++)
		if (s[n] == ' ') continue;
		else if (s[n] > 64 && s[n] < 91)
		{
			cleaned[len] = s[n];
			len++;
		}
		else if (s[n] > 96 && s[n] < 123)
		{
			cleaned[len] = toupper(s[n]);
			len++;
		}
		else if (s[n] == '_') break;
	cleaned[len] = '\0';
	for (n=0; n<nElements_; n++)
		if (strcmp(el_[n].ucSymbol,cleaned) == 0) 
		{
			result = n;
			break;
		}
	return result;
}

// Convert string from first alpha part to element number
int ElementMap::firstAlphaToZ(const char *s)
{
	// Convert up to non-alpha character.
	static char cleaned[32];
	int n, len = 0, result = -1;
	for (n=0; s[n] != '\0'; n++)
		if (s[n] == ' ') continue;
		else if (s[n] > 64 && s[n] < 91)
		{
			cleaned[len] = s[n];
			len++;
		}
		else if (s[n] > 96 && s[n] < 123)
		{
			cleaned[len] = toupper(s[n]);
			len++;
		}
		else break;
	cleaned[len] = '\0';
	for (n=0; n<nElements_; n++)
		if (strcmp(el_[n].ucSymbol,cleaned) == 0) 
		{
			result = n;
			break;
		}
	return result;
}

// Convert string from name to element number
int ElementMap::nameToZ(const char *s)
{
	// Ignore numbers. Convert up to non-alpha character.
	static char cleaned[32];
	int n, len = 0, result = -1;
	for (n=0; s[n] != '\0'; n++)
		if (s[n] > 64 && s[n] < 91)
		{
			cleaned[len] = s[n];
			len++;
		}
		else if (s[n] > 96 && s[n] < 123)
		{
			cleaned[len] = toupper(s[n]);
			len++;
		}
		else if (s[n] == '_') break;
	cleaned[len] = '\0';
	for (n=0; n<nElements_; n++)
		if (strcmp(el_[n].ucName,cleaned) == 0) 
		{
			result = n;
			break;
		}
	return result;
}

// Convert string from fftype to element number
int ElementMap::ffToZ(const char *s)
{
	ForcefieldAtom *ffa;
	int result = -1;
	for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next)
	{
		ffa = ff->findType(s);
		// Found a match, so find out what element it is...
		if (ffa != NULL) result = ffa->atomtype()->characterElement();
		if (result != -1) break;
	}
	return result;
}

// Search for element named 'query' in the list of known elements
int ElementMap::find(const char *query)
{
	// Get the element number from the element name provided.
	msg.enter("ElementMap::find");
	int result = -1;
	if (query[0] == '\0')
	{
		printf("Warning: Element search requested on blank string.\n");
		msg.exit("ElementMap::find");
		return 0;
	}
	// Convert the query string according to the specified rule
	switch (prefs.zmapType())
	{
		// Automatic determination
		case (Prefs::AutoZmap):
			// First, try pure numeric conversion
			result = numberToZ(query);
			if (result != -1) break;
			// Then, try alpha conversion
			result = alphaToZ(query);
			if (result != -1) break;
			// Then, try name conversion
			result = nameToZ(query);
			if (result != -1) break;
			// Finally, try FF conversion
			result = ffToZ(query);
			break;
		// Name search
		case (Prefs::NameZmap):
			result = nameToZ(query);
			break;
		// Search loaded forcefields for atom names
		case (Prefs::ForcefieldZmap):
			result = ffToZ(query);
			// Attempt an alpha conversion if the FF conversion failed
			if (result == -1) result = alphaToZ(query);
			break;
		// Convert based on alpha-part of atom name only
		case (Prefs::AlphaZmap):
			result = alphaToZ(query);
			break;
		// Convert based on first alpha-part of atom name only
		case (Prefs::FirstAlphaZmap):
			result = firstAlphaToZ(query);
			break;
		// Convert based on numeric part only
		case (Prefs::NumericZmap):
			result = numberToZ(query);
			break;
	}
	msg.exit("ElementMap::find");
	return ((result == -1) ? 0 : result);
}

// Search for element named 'query' in the list of known elements, using the specified algorithm
int ElementMap::find(const char *query, Prefs::ZmapType zmt)
{
	// Store the old zmapping type, and temporarily set a new one
	Prefs::ZmapType last = prefs.zmapType();
	prefs.setZmapType(zmt);
	int result = find(query);
	prefs.setZmapType(last);
	return result;
}

