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
#include "base/master.h"
#include "base/debug.h"
#include "base/sysfunc.h"
#include "parse/parser.h"
#include "classes/forcefield.h"

element_map elements;

/*
// Default Element Data
*/

// Format: ID   Mass    Name            Symbol  Radius  Vlncy	AmbientRGBA		DiffuseRGBA
element element_map::el[] = {
	{ 0.000,	"Dummy","DUMMY",	"XX","XX",	0.500,	0,	0.5f,0.5f,0.5f,1.0f,	0.375f,0.375f,0.375f,1.0f },
	{ 1.008,	"Hydrogen","HYDROGEN",	"H","H",	0.320,	1,	0.87f,0.87f,0.87f,1.0f,	0.78f,0.78f,0.78f,1.0f },
	{ 4.003,	"Helium","HELIUM",	"He","HE",	0.310,	1,	1.0f,0.784f,0.784f,1.0f,	0.75f,0.588f,0.588f,1.0f },
	{ 6.941,	"Lithium","LITHIUM",	"Li","LI",	1.630,	1,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 9.012,	"Beryllium","BERYLLIUM","Be","BE",	0.900,	1,	0.0f,0.0f,0.0f,1.0f,	0.0f,0.0f,0.0f,1.0f },
	{ 10.811,	"Boron","BORON",	"B","B",	0.820,	0,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 12.011,	"Carbon","CARBON",	"C","C",	0.770,	4,	0.0f,1.0f,0.2f,1.0f,	0.0f,0.75f,0.150f,1.0f },
	{ 14.007,	"Nitrogen","NITROGEN",	"N","N",	0.750,	3,	0.561f,0.561f,1.0f,1.0f,	0.421f,0.421f,0.75f,1.0f },
	{ 15.999,	"Oxygen","OXYGEN",	"O","O",	0.730,	2,	1.0f,0.0f,0.0f,1.0f,	0.75f,0.0f,0.0f,1.0f },
	{ 18.998,	"Fluorine","FLUORINE",	"F","F",	0.720,	1,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f },
	{ 20.180,	"Neon","NEON",		"Ne","NE",	0.710,	0,	0.0f,0.0f,0.0f,1.0f,	0.0f,0.0f,0.0f,1.0f },
	{ 22.990,	"Sodium","SODIUM",	"Na","NA",	1.540,	1,	0.0f,0.0f,1.0f,1.0f,	0.0f,0.0f,0.75f,1.0f },
	{ 24.305,	"Magnesium","MAGNESIUM","Mg","MG",	1.360,	0,	0.0f,0.0f,0.0f,1.0f,	0.0f,0.0f,0.0f,1.0f },
	{ 26.982,	"Aluminium","ALUMINIUM","Al","AL",	1.180,	3,	0.561f,0.561f,1.0f,1.0f,	0.421f,0.421f,0.75f,1.0f },
	{ 28.085,	"Silicon","SILICON",	"Si","SI",	1.110,	4,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f },
	{ 30.974,	"Phosphorous","PHOSPHOROUS","P","P",	1.060,	1,	1.0f,0.647f,0.0f,1.0f,	0.75f,0.485f,0.0f,1.0f },
	{ 32.066,	"Sulfur","SULFUR",	"S","S",	1.020,	2,	1.0f,0.784f,0.196f,1.0f,	0.75f,0.588f,0.147f,1.0f },
	{ 35.453,	"Chlorine","CHLORINE",	"Cl","CL",	0.990,	1,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 39.948,	"Argon","ARGON",	"Ar","AR",	0.980,	0,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 39.098,	"Potassium","POTASSIUM","K","K",	2.030,	1,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 40.078,	"Calcium","CALCIUM",	"Ca","CA",	1.740,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 44.956,	"Scandium","SCANDIUM",	"Sc","SC",	1.440,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 47.880,	"Titanium","TITANIUM",	"Ti","TI",	1.320,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 50.941,	"Vanadium","VANADIUM",	"V","V",	1.220,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 51.996,	"Chromium","CHROMIUM",	"Cr","CR",	1.180,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 54.938,	"Manganese","MANGANESE","Mn","MN",	1.170,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 55.847,	"Iron","IRON",		"Fe","FE",	1.170,	0,	1.0f,0.647f,0.0f,1.0f,	0.75f,0.485f,0.0f,1.0f },
	{ 58.933,	"Cobalt","COBALT",	"Co","CO",	1.160,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 58.693,	"Nickel","NICKEL",	"Ni","NI",	1.150,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 63.546,	"Copper","COPPER",	"Cu","CU",	1.170,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 65.390,	"Zinc","ZINC",		"Zn","ZN",	1.250,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 69.723,	"Gallium","GALLIUM",	"Ga","GA",	1.260,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 72.610,	"Germanium","GERMANIUM","Ge","GE",	1.220,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 74.922,	"Arsenic","ARSENIC",	"As","AS",	1.200,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 78.960,	"Selenium","SELENIUM",	"Se","SE",	1.160,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 79.904,	"Bromine","BROMINE",	"Br","BR",	1.140,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 83.800,	"Krypton","KRYPTON",	"Kr","KR",	1.120,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 85.468,	"Rubidium","RUBIDIUM",	"Rb","RB",	2.160,	0,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 87.620,	"Strontium","STRONTIUM","Sr","SR",	1.910,	0,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 88.906,	"Yttrium","YTTRIUM",	"Y","Y",	1.620,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 91.224,	"Zirconium","ZIRCONIUM","Zr","ZR",	1.450,	0,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f },
	{ 92.906,	"Niobium","NIOBIUM",	"Nb","NB",	1.340,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 95.940,	"Molybdenum","MOLYBDENUM","Mo","MO",	1.300,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 97.907,	"Technetium","TECHNETIUM","Tc","TC",	1.270,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 101.070,	"Ruthenium","RUTHENIUM","Ru","RU",	1.250,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 102.906,	"Rhodium","RHODIUM",	"Rh","RH",	1.250,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 106.420,	"Palladium","PALLADIUM","Pd","PD",	1.280,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 107.870,	"Silver","SILVER",	"Ag","AG",	1.340,	0,	0.6f,0.6f,0.6f,1.0f,	0.45f,0.45f,0.45f,1.0f },
	{ 112.411,	"Cadmium","CADMIUM",	"Cd","CD",	1.480,	0,	0.0f,1.0f,0.0f,1.0f,	0.0f,0.75f,0.0f,1.0f },
	{ 114.818,	"Indium","INDIUM",	"In","IN",	1.440,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 118.710,	"Tin","TIN",		"Sn","SN",	1.410,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 121.760,	"Antimony","ANTIMONY",	"Sb","SB",	1.400,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 127.600,	"Tellurium","TELLURIUM","Te","TE",	1.360,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 129.905,	"Iodine","IODINE",	"I","I",	1.330,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 131.290,	"Xenon","XENON",	"Xe","XE",	1.310,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 132.905,	"Caesium","CAESIUM",	"Cs","CS",	2.350,	0,	0.647f,0.165f,0.165f,1.0f,	0.485f,0.124f,0.124f,1.0f },
	{ 137.327,	"Barium","BARIUM",	"Ba","BA",	1.980,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 138.905,	"Lanthanum","LANTHANUM","La","LA",	1.690,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 140.115,	"Cerium","CERIUM",	"Ce","CE",	1.650,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 140.908,	"Praesodymium","PRAESODYMIUM","Pr","PR",1.650,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 144.240,	"Neodymium","NEODYMIUM","Ns","NS",	1.840,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 144.913,	"Prometheum","PROMETHEUM","Pm","PM",	1.630,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 150.360,	"Samarium","SAMARIUM",	"Sm","SM",	1.620,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 151.965,	"Europium","EUROPIUM",	"Eu","EU",	1.850,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 157.250,	"Gadolinium","GADOLINIUM","Gd","GD",	1.610,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 158.925,	"Terbium","TERBIUM",	"Tb","TB",	1.590,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 162.500,	"Dysprosium","DYSPROSIUM","Dy","DY",	1.590,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 164.930,	"Holmium","HOLMIUM",	"Ho","HO",	1.580,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 167.260,	"Erbium","ERBIUM",	"Er","ER",	1.570,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 168.934,	"Thulium","THULIUM",	"Tm","TM",	1.560,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 173.040,	"Ytterbium","YTTERBIUM","Yb","YB",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 174.967,	"Lutetium","LUTETIUM",	"Lu","LU",	1.560,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 178.490,	"Hafnium","HAFNIUM",	"Hf","HF",	1.440,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 180.948,	"Tantalum","TANTALUM",	"Ta","TA",	1.340,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 183.840,	"Tungsten","TUNGSTEN",	"W","W",	1.300,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 186.207,	"Rhenium","RHENIUM",	"Re","RE",	1.280,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 190.230,	"Osmium","OSMIUM",	"Os","OS",	1.260,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 192.220,	"Iridium","IRIDIUM",	"Ir","IR",	1.270,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 195.080,	"Platinum","PLATINUM",	"Pt","PT",	1.300,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 196.967,	"Gold","GOLD",		"Au","AU",	1.340,	0,	0.784f,0.647f,0.094f,1.0f,	0.588f,0.485f,0.071f,1.0f },
	{ 200.590,	"Mercury","MERCURY",	"Hg","HG",	1.490,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 204.383,	"Thallium","THALLIUM",	"Tl","TL",	1.480,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 207.200,	"Lead","LEAD",		"Pb","PB",	1.470,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 208.980,	"Bismuth","BISMUTH",	"Bi","BI",	1.460,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 208.982,	"Polonium","POLONIUM",	"Po","PO",	1.460,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 209.987,	"Astatine","ASTATINE",	"At","AT",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 222.018,	"Radon","RADON",	"Rn","RN",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 223.020,	"Francium","FRANCIUM",	"Fr","FR",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 226.025,	"Radon","RADON",	"Ra","RA",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 227.028,	"Actinium","ACTINIUM",	"Ac","AC",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 232.038,	"Thorium","THORIUM",	"Th","TH",	1.650,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 231.036,	"Protactinium","PROTACTINIUM","Pa","PA",2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 238.029,	"Uranium","URANIUM",	"U","U",	1.420,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 237.048,	"Neptunium","NEPTUNIUM","Np","NP",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 244.064,	"Plutonium","PLUTONIUM","Pu","PU",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 243.061,	"Americium","AMERICIUM","Am","AM",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 247.070,	"Curium","CURIUM",	"Cm","CM",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 247.070,	"Berkelium","BERKELIUM","Bk","BK",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 251.080,	"Californium","CALIFORNIUM","Cf","CF",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 252.083,	"Einsteinium","EINSTEINIUM","Es","ES",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 257.095,	"Fermium","FERMIUM",	"Fm","FM",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 258.100,	"Mendelevium","MENDELEVIUM","Md","MD",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 259.101,	"Nobelium","NOBELIUM",	"No","NO",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 262.110,	"Lawrencium","LAWRENCIUM","Lr","LR",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 261.000,	"Rutherfordium","RUTHERFORDIUM","Rf","RF",2.000,0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 262.000,	"Dubnium","DUBNIUM",	"Db","DB",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 266.000,	"Seaborgium","SEABORGIUM","Sg","SG",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 264.000,	"Bohrium","BOHRIUM",	"Bh","BH",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 269.000,	"Hassium","HASSIUM",	"Hs","HS",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 268.000,	"Meitnerium","MEITNERIUM","Mt","MT",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 281.000,	"Darmstadtium","DARMSTADTIUM","Ds","DS",2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 272.000,	"Roentgenium","ROENTGENIUM","Rg","RG",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 285.000,	"Ununium","UNUNIUM",	"Uub","UUB",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 284.000,	"Ununtrium","UNUNTRIUM","Uut","UUT",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 289.000,	"Ununquadium","UNUNQUADIUM","Uuq","UUQ",2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 288.000,	"Ununpentium","UNUNPENTIUM","Uup","UUP",2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 292.000,	"Ununhexium","UNUNHEXIUM","Uuh","UUH",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 0.000,	"Ununseptium","UNUNSEPTIUM","Uus","UUS",2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 0.000,	"Ununoctium","UNUNOCTIUM","Uuo","UUO",	2.000,	0,	1.0f,1.0f,1.0f,1.0f,	0.75f,0.75f,0.75f,1.0f },
	{ 0.000,	"Ellipsoid","ELLIPSOID","ELP","ELP",	0.000,	0,	0.0f,0.0f,1.0f,1.0f,	0.0f,0.0f,0.75f,1.0f }
};

// Constructor
element_map::element_map()
{
	#ifdef MEMDEBUG
		printf("Constructor : element_map\n");
	#endif
}

// Destructor
element_map::~element_map()
{
	#ifdef MEMDEBUG
		printf(" Destructor : element_map\n");
	#endif
}

// Return ambient colour in supplied vector
void element_map::ambient(int i, GLfloat *v)
{
	v[0] = el[i].ambient[0];
	v[1] = el[i].ambient[1];
	v[2] = el[i].ambient[2];
	v[3] = el[i].ambient[3];
}

// Return diffuse colour in supplied vector
void element_map::diffuse(int i, GLfloat *v)
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

// Convert string from first alpha part to element number
int element_map::alphafirst_to_z(const char *s)
{
	// Ignore numbers. Convert up to non-alpha character.
	static char cleaned[32];
	int n, len = 0, result = -1;
	for (n=0; s[n] != '\0'; n++)
		if (s[n] > 64 && s[n] < 91) { cleaned[len] = s[n]; len++; }
		else if (s[n] > 96 && s[n] < 123) { cleaned[len] = toupper(s[n]); len++; }
		else break;
	cleaned[len] = '\0';
	for (n=0; n<NELEMENTS; n++)
		if (strcmp(el[n].ucsymbol,cleaned) == 0) 
		{
			result = n;
			break;
		}
	return result;
}

// Convert string from name to element number
int element_map::name_to_z(const char *s)
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
		if (strcmp(el[n].ucname,cleaned) == 0) 
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
			// Then, try name conversion
			result = name_to_z(query);
			if (result != -1) break;
			// Finally, try FF conversion
			result = ff_to_z(query);
			break;
		// Name search
		case (ZM_NAME):
			result = name_to_z(query);
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
		// Convert based on first alpha-part of atom name only
		case (ZM_FIRSTALPHA):
			result = alphafirst_to_z(query);
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

