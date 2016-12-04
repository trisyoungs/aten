/*
	*** Element Definitions
	*** src/base/elements.cpp
	Copyright T. Youngs 2007-2016

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

#include "base/element.h"
#include "base/sysfunc.h"
#include "ff/forcefield.h"
#include "base/forcefieldatom.h"
#include "base/atom.h"
#include "main/aten.h"
#include <QPainter>

ATEN_BEGIN_NAMESPACE

// Static Members
Aten* ElementMap::aten_ = NULL;
Element* ElementMap::elements_ = NULL;
int ElementMap::nElements_ = 0;
Element* ElementMap::backupElements_ = NULL;
NameMapList<int> ElementMap::mappings_;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Definitions
#define LANTHANIDES 98
#define ACTINIDES 99

// ZMapping types
const char* ZMapTypeKeywords[ElementMap::nZMapTypes] = { "Alpha", "FirstAlpha", "SingleAlpha", "Name", "Numeric", "FF", "Auto" };
ElementMap::ZMapType ElementMap::zMapType(QString s, bool reportError)
{
	ElementMap::ZMapType zm = (ElementMap::ZMapType) enumSearch("element mapping style", ElementMap::nZMapTypes, ZMapTypeKeywords, s);
	if ((zm == nZMapTypes) && reportError) enumPrintValid(ElementMap::nZMapTypes, ZMapTypeKeywords);
	return zm;
}
const char* ElementMap::zMapType(ElementMap::ZMapType zm)
{
	return ZMapTypeKeywords[zm];
}

/*
 * Default Element Data
 * Element radii taken from:
 *	"Covalent radii revisited", 
 *	B. Cordero, V. Gómez, A. E. Platero-Prats, M. Revés, J. Echeverría, E. Cremades, F. Barragán and S. Alvarez
 *	Dalton Trans., 2008 (DOI: 10.1039/b801115j)
 *	Notes: High-Spin radii taken for 1st transition elements, sp3 value taken for Carbon. Bk and beyond set to 1.50.
 *
 * Lutetium and Lawrencium are assigned to group 3, with Lanthanum and Actinium the first elements in the lanthanide and actinide series.
 */

//	  Z	Mass  		Name         		Symbol			Group	Radius  RGBA
const Element ElementMap::defaultElements_[] = {
	{ 0,	0.000,		"Dummy","DUMMY",	"XX","XX",		0,	0.00,	{ 0.5,0.5,0.5,1.0 },
		{ 0,0,0,0,0,0,0,0,0 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 1,	1.008,		"Hydrogen","HYDROGEN",	"H","H",		1,	0.31,	{ 0.87,0.87,0.87,1.0 },
		{ 2,0,32,32,32,32,32,32,32 },	{ 1,0,-1,0,0,0,0,0,0 } },
	{ 2,	4.003,		"Helium","HELIUM",	"He","HE",		18,	0.28,	{ 1.0,0.784,0.784,1.0 },
		{ 8,0,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 3,	6.941,		"Lithium","LITHIUM",	"Li","LI",		1,	1.28,	{ 0.647,0.165,0.165,1.0 },
		{ 2,0,32,32,32,32,32,32,32 },	{ 1,0,0,0,0,0,0,0,0 } },
	{ 4,	9.012,		"Beryllium","BERYLLIUM","Be","BE",		2,	0.96,	{ 0.0,0.0,0.0,1.0 },
		{ 32,8,4,0,32,32,32,32,32 },	{ 2,1,0,0,0,0,0,0,0 } },
	{ 5,	10.811,		"Boron","BORON",	"B","B",		13,	0.84,	{ 0.0,1.0,0.0,1.0 },
		{ 32,16,8,0,32,32,32,32,32 },	{ 3,2,1,0,0,0,0,0,0 } },
	{ 6,	12.011,		"Carbon","CARBON",	"C","C",		14,	0.76,	{ 0.0,1.0,0.2,1.0 },
		{ 32,32,32,16,0,32,32,32,32 },	{ -4,-3,-2,-1,0,1,0,0,0 } },
	{ 7,	14.007,		"Nitrogen","NITROGEN",	"N","N",		15,	0.71,	{ 0.561,0.561,1.0,1.0 },
		{ 32,32,16,0,8,64,32,32,32 },	{ -3,-2,-1,0,1,0,0,0,0 } },
	{ 8,	15.999,		"Oxygen","OXYGEN",	"O","O",		16,	0.66,	{ 1.0,0.0,0.0,1.0 },
		{ 32,32,0,32,32,32,32,32,32 },	{ -2,-1,0,1,0,0,0,0,0 } },
	{ 9,	18.998,		"Fluorine","FLUORINE",	"F","F",		17,	0.57,	{ 0.784,0.647,0.094,1.0 },
		{ 2,0,32,32,32,32,32,32,32 },	{ -1,0,0,0,0,0,0,0,0 } },
	{ 10,	20.180,		"Neon","NEON",		"Ne","NE",		18,	0.58,	{ 0.0,0.0,0.0,1.0 },
		{ 8,0,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 11,	22.990,		"Sodium","SODIUM",	"Na","NA",		1,	1.66,	{ 0.0,0.0,1.0,1.0 },
		{ 2,0,32,32,32,32,32,32,32 },	{ 1,0,0,0,0,0,0,0,0 } },
	{ 12,	24.305,		"Magnesium","MAGNESIUM","Mg","MG",		2,	1.41,	{ 0.0,0.0,0.0,1.0 },
		{ 8,4,0,32,32,32,32,32,32 },	{ 2,1,0,0,0,0,0,0,0 } },
	{ 13,	26.982,		"Aluminium","ALUMINIUM","Al","AL",		13,	1.21,	{ 0.561,0.561,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 14,	28.085,		"Silicon","SILICON",	"Si","SI",		14,	1.11,	{ 0.784,0.647,0.094,1.0 },
		{ 32,32,32,32,0,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 15,	30.974,		"Phosphorus","PHOSPHORUS","P","P",		15,	1.07,	{ 1.0,0.647,0.0,1.0 },
		{ 32,32,32,0,32,0,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 16,	32.066,		"Sulfur","SULFUR",	"S","S",		16,	1.05,	{ 1.0,0.784,0.196,1.0 },
		{ 32,32,0,16,0,16,0,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 17,	35.453,		"Chlorine","CHLORINE",	"Cl","CL",		17,	1.02,	{ 0.0,1.0,0.0,1.0 },
		{ 0,0,32,32,32,32,32,32,32 },	{ -1,0,0,0,0,0,0,0,0 } },
	{ 18,	39.948,		"Argon","ARGON",	"Ar","AR",		18,	1.06,	{ 0.0,1.0,0.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 19,	39.098,		"Potassium","POTASSIUM","K","K",		1,	2.03,	{ 0.0,1.0,0.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 20,	40.078,		"Calcium","CALCIUM",	"Ca","CA",		2,	1.76,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 21,	44.956,		"Scandium","SCANDIUM",	"Sc","SC",		3,	1.70,	{ 1.0,0.86,0.66,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 22,	47.880,		"Titanium","TITANIUM",	"Ti","TI",		4,	1.60,	{ 0.7,0.7,0.7,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 23,	50.941,		"Vanadium","VANADIUM",	"V","V",		5,	1.53,	{ 0.7,0.7,0.7,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 24,	51.996,		"Chromium","CHROMIUM",	"Cr","CR",		6,	1.39,	{ 0.7,0.7,0.7,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 25,	54.938,		"Manganese","MANGANESE","Mn","MN",		7,	1.61,	{ 0.7,0.7,0.7,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 26,	55.847,		"Iron","IRON",		"Fe","FE",		8,	1.52,	{ 1.0,0.647,0.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 27,	58.933,		"Cobalt","COBALT",	"Co","CO",		9,	1.50,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 28,	58.693,		"Nickel","NICKEL",	"Ni","NI",		10,	1.24,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 29,	63.546,		"Copper","COPPER",	"Cu","CU",		11,	1.32,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 30,	65.390,		"Zinc","ZINC",		"Zn","ZN",		12,	1.22,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 31,	69.723,		"Gallium","GALLIUM",	"Ga","GA",		13,	1.22,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 32,	72.610,		"Germanium","GERMANIUM","Ge","GE",		14,	1.20,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 33,	74.922,		"Arsenic","ARSENIC",	"As","AS",		15,	1.19,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 34,	78.960,		"Selenium","SELENIUM",	"Se","SE",		16,	1.20,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 35,	79.904,		"Bromine","BROMINE",	"Br","BR",		17,	1.20,	{ 0.647,0.165,0.165,1.0 },
		{ 0,0,32,32,32,32,32,32,32 },	{ -1,0,0,0,0,0,0,0,0 } },
	{ 36,	83.800,		"Krypton","KRYPTON",	"Kr","KR",		18,	1.16,	{ 0.6,0.6,0.6,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 37,	85.468,		"Rubidium","RUBIDIUM",	"Rb","RB",		1,	2.20,	{ 0.0,1.0,0.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 38,	87.620,		"Strontium","STRONTIUM","Sr","SR",		2,	1.95,	{ 0.0,1.0,0.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 39,	88.906,		"Yttrium","YTTRIUM",	"Y","Y",		3,	1.90,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 40,	91.224,		"Zirconium","ZIRCONIUM","Zr","ZR",		4,	1.75,	{ 0.784,0.647,0.094,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 41,	92.906,		"Niobium","NIOBIUM",	"Nb","NB",		5,	1.64,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 42,	95.940,		"Molybdenum","MOLYBDENUM","Mo","MO",		6,	1.54,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 43,	97.907,		"Technetium","TECHNETIUM","Tc","TC",		7,	1.47,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 44,	101.070,	"Ruthenium","RUTHENIUM","Ru","RU",		8,	1.46,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 45,	102.906,	"Rhodium","RHODIUM",	"Rh","RH",		9,	1.42,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 46,	106.420,	"Palladium","PALLADIUM","Pd","PD",		10,	1.39,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 47,	107.870,	"Silver","SILVER",	"Ag","AG",		11,	1.45,	{ 0.6,0.6,0.6,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 48,	112.411,	"Cadmium","CADMIUM",	"Cd","CD",		12,	1.44,	{ 0.0,1.0,0.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 49,	114.818,	"Indium","INDIUM",	"In","IN",		13,	1.42,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 50,	118.710,	"Tin","TIN",		"Sn","SN",		14,	1.39,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 51,	121.760,	"Antimony","ANTIMONY",	"Sb","SB",		15,	1.39,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 52,	127.600,	"Tellurium","TELLURIUM","Te","TE",		16,	1.38,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 53,	129.905,	"Iodine","IODINE",	"I","I",		17,	1.39,	{ 1.0,1.0,1.0,1.0 },
		{ 0,0,32,32,32,32,32,32,32 },	{ -1,0,0,0,0,0,0,0,0 } },
	{ 54,	131.290,	"Xenon","XENON",	"Xe","XE",		18,	1.40,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 55,	132.905,	"Caesium","CAESIUM",	"Cs","CS",		1,	2.44,	{ 0.647,0.165,0.165,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 56,	137.327,	"Barium","BARIUM",	"Ba","BA",		2,	2.15,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 57,	138.905,	"Lanthanum","LANTHANUM","La","LA",		98,	2.07,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 58,	140.115,	"Cerium","CERIUM",	"Ce","CE",		98,	2.04,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 59,	140.908,	"Praesodymium","PRAESODYMIUM","Pr","PR",	98,	2.03,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 60,	144.240,	"Neodymium","NEODYMIUM","Nd","ND",		98,	2.01,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 61,	144.913,	"Prometheum","PROMETHEUM","Pm","PM",		98,	1.99,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 62,	150.360,	"Samarium","SAMARIUM",	"Sm","SM",		98,	1.98,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 63,	151.965,	"Europium","EUROPIUM",	"Eu","EU",		98,	1.98,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 64,	157.250,	"Gadolinium","GADOLINIUM","Gd","GD",		98,	1.96,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 65,	158.925,	"Terbium","TERBIUM",	"Tb","TB",		98,	1.94,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 66,	162.500,	"Dysprosium","DYSPROSIUM","Dy","DY",		98,	1.92,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 67,	164.930,	"Holmium","HOLMIUM",	"Ho","HO",		98,	1.92,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 68,	167.260,	"Erbium","ERBIUM",	"Er","ER",		98,	1.89,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 69,	168.934,	"Thulium","THULIUM",	"Tm","TM",		98,	1.90,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 70,	173.040,	"Ytterbium","YTTERBIUM","Yb","YB",		98,	1.87,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 71,	174.967,	"Lutetium","LUTETIUM",	"Lu","LU",		3,	1.87,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 72,	178.490,	"Hafnium","HAFNIUM",	"Hf","HF",		4,	1.75,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 73,	180.948,	"Tantalum","TANTALUM",	"Ta","TA",		5,	1.70,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 74,	183.840,	"Tungsten","TUNGSTEN",	"W","W",		6,	1.62,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 75,	186.207,	"Rhenium","RHENIUM",	"Re","RE",		7,	1.51,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 76,	190.230,	"Osmium","OSMIUM",	"Os","OS",		8,	1.44,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 77,	192.220,	"Iridium","IRIDIUM",	"Ir","IR",		9,	1.41,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 78,	195.080,	"Platinum","PLATINUM",	"Pt","PT",		10,	1.36,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 79,	196.967,	"Gold","GOLD",		"Au","AU",		11,	1.36,	{ 0.784,0.647,0.094,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 80,	200.590,	"Mercury","MERCURY",	"Hg","HG",		12,	1.32,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 81,	204.383,	"Thallium","THALLIUM",	"Tl","TL",		13,	1.45,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 82,	207.200,	"Lead","LEAD",		"Pb","PB",		14,	1.46,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 83,	208.980,	"Bismuth","BISMUTH",	"Bi","BI",		15,	1.48,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 84,	208.982,	"Polonium","POLONIUM",	"Po","PO",		16,	1.40,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 85,	209.987,	"Astatine","ASTATINE",	"At","AT",		17,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 86,	222.018,	"Radon","RADON",	"Rn","RN",		18,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 87,	223.020,	"Francium","FRANCIUM",	"Fr","FR",		1,	2.60,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 88,	226.025,	"Radon","RADON",	"Ra","RA",		2,	2.21,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 89,	227.028,	"Actinium","ACTINIUM",	"Ac","AC",		99,	2.15,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 90,	232.038,	"Thorium","THORIUM",	"Th","TH",		99,	2.06,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 91,	231.036,	"Protactinium","PROTACTINIUM","Pa","PA",	99,	2.00,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 92,	238.029,	"Uranium","URANIUM",	"U","U",		99,	1.96,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 93,	237.048,	"Neptunium","NEPTUNIUM","Np","NP",		99,	1.90,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 94,	244.064,	"Plutonium","PLUTONIUM","Pu","PU",		99,	1.87,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 95,	243.061,	"Americium","AMERICIUM","Am","AM",		99,	1.80,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 96,	247.070,	"Curium","CURIUM",	"Cm","CM",		99,	1.69,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 97,	247.070,	"Berkelium","BERKELIUM","Bk","BK",		99,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 98,	251.080,	"Californium","CALIFORNIUM","Cf","CF",		99,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 99,	252.083,	"Einsteinium","EINSTEINIUM","Es","ES",		99,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 100,	257.095,	"Fermium","FERMIUM",	"Fm","FM",		99,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 101,	258.100,	"Mendelevium","MENDELEVIUM","Md","MD",		99,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 102,	259.101,	"Nobelium","NOBELIUM",	"No","NO",		99,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 103,	262.110,	"Lawrencium","LAWRENCIUM","Lr","LR",		3,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 104,	261.000,	"Rutherfordium","RUTHERFORDIUM","Rf","RF",	4,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 105,	262.000,	"Dubnium","DUBNIUM",	"Db","DB",		5,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 106,	266.000,	"Seaborgium","SEABORGIUM","Sg","SG",		6,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 107,	264.000,	"Bohrium","BOHRIUM",	"Bh","BH",		7,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 108,	269.000,	"Hassium","HASSIUM",	"Hs","HS",		8,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 109,	268.000,	"Meitnerium","MEITNERIUM","Mt","MT",		9,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 110,	281.000,	"Darmstadtium","DARMSTADTIUM","Ds","DS",	10,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 111,	272.000,	"Roentgenium","ROENTGENIUM","Rg","RG",		11,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 112,	285.000,	"Copernicium","COPERNICIUM","Cn","CN",		12,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 113,	284.000,	"Nihonium","NIHONIUM","Nh","NH",		13,	1.50,	{ 1.0,1.0,1.0,1.0 },	// Proposed by IUPAC June 2016 (due for acceptance on/after November 2016)
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 114,	289.000,	"Flerovium","FLEROVIUM","Fl","FL",		14,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 115,	289.000,	"Moscovium","MOSCOVIUM","Mc","MC",		15,	1.50,	{ 1.0,1.0,1.0,1.0 },	// Proposed by IUPAC June 2016 (due for acceptance end 2016)
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 116,	293.000,	"Livermorium","LIVERMORIUM","Lv","LV",		16,	1.50,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 117,	294.000,	"Tennessine","TENNESSINE","Ts","TS",		17,	1.50,	{ 1.0,1.0,1.0,1.0 },	// Proposed by IUPAC June 2016 (due for acceptance on/after November 2016)
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 118,	294.000,	"Oganesson","OGANESSON","Og","OG",		18,	1.50,	{ 1.0,1.0,1.0,1.0 },	// Proposed by IUPAC June 2016 (due for acceptance on/after November 2016)
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 119,	0.000,		"","","","",					-1,	0.00,	{ 1.0,1.0,1.0,1.0 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } },
	{ 120,	0.000,		"Vacancy","VACANCY","Vac","VAC",		0,	0.00,	{ 0.2,0.2,0.2,0.35 },
		{ 32,32,32,32,32,32,32,32,32 },	{ 0,0,0,0,0,0,0,0,0 } }
};

/*
 * Element Data
 */

// Initialise data before first use
void ElementMap::initialise(Aten* aten)
{
	// Private variables
	aten_ = aten;

	// Determine number of defined elements, and double-check against MAXELEMENTSant
	nElements_ = sizeof(defaultElements_) / sizeof(defaultElements_[0]);
	if (nElements_ > MAXELEMENTS) printf("Warning - Number of internally-defined elements exceeds MAXELEMENTSant.\n");

	// Copy default element data to current element array
	elements_ = new Element[nElements_];
	for (int n=0; n <nElements_; ++n) elements_[n] = defaultElements_[n];

	// Create backup array while we're here as well
	backupElements_ = new Element[nElements_];

	// Set default value for namemapping
	mappings_.setDefaultValue(-1);
}

// Delete data after final use
void ElementMap::finalise()
{
	delete[] backupElements_;
	delete[] elements_;
}

// Copy current element data to backup structure
void ElementMap::backupData()
{
	for (int n=0; n <nElements_; ++n) backupElements_[n] = elements_[n];
}

// Copy backed up element data to actual element data
void ElementMap::restoreData()
{
	for (int n=0; n <nElements_; ++n) elements_[n] = backupElements_[n];
}

// Return number of defined elements
int ElementMap::nElements()
{
	return nElements_;
}

// Return pointer to specified element
Element* ElementMap::element(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::element() : Atomic number %i is out of range.\n", z);
		return &elements_[0];
	}
	return &elements_[z];
}

// Clear all name mappings
void ElementMap::clearMappings()
{
	mappings_.clear();
}

// Add name to import map
void ElementMap::addMapping(int element, QString name)
{
	mappings_.add(name, element);
}

// Return Z of specified element symbol
int ElementMap::z(QString symbol)
{
	QString ucase = symbol.toUpper();
	for (int n=0; n<nElements_; ++n) if (ucase == elements_[n].ucSymbol) return n;
	return 0;
}

// Return Z of element with corresponding mass (within tolerance)
int ElementMap::z(double targetMass, double tolerance)
{
	for (int n=0; n<nElements_; ++n) if (fabs(elements_[n].atomicMass - targetMass) < tolerance) return n;
	return 0;
}

// Convert string from Z to element number
int ElementMap::numberToZ(QString number)
{
	// Check that the string is entirely numerical
	bool isNumber = true;
	for (int n=0; n<number.length(); ++n)
	{
		if (number.at(n).isDigit()) continue;

		isNumber = false;
		break;
	}

	if (isNumber)
	{
		// Check range of number before returning
		int z = number.toInt();
		if ((z < 0) || (z > nElements_))
		{
			Messenger::print("Warning: Converted element number is out of range (%i)", z);
			z = 0;
		}
		return z;
	}
	else return -1;
}

// Convert string from alpha to element number
int ElementMap::alphaToZ(QString alpha)
{
	// Ignore numbers. Convert up to non-alpha character.
	QString stripped;
	int n;
	for (n=0; n<alpha.length(); ++n)
	{
		if (alpha.at(n) == ' ') continue;
		else if (alpha.at(n).isUpper()) stripped += alpha.at(n);
		else if (alpha.at(n).isLower()) stripped += alpha.at(n).toUpper();
		else if (alpha[n] == '_') break;
	}
	for (n=0; n<nElements_; ++n) if (stripped == elements_[n].ucSymbol) return n;
	return -1;
}

// Convert string from first alpha part to element number
int ElementMap::firstAlphaToZ(QString alpha)
{
	// Convert up to non-alpha character.
	QString stripped;
	int n;
	for (n=0; n<alpha.length(); ++n)
	{
		if (alpha.at(n) == ' ') continue;
		else if (alpha.at(n).isUpper()) stripped += alpha.at(n);
		else if (alpha.at(n).isLower()) stripped += alpha.at(n).toUpper();
		else break;
	}
	for (n=0; n<nElements_; ++n) if (stripped == elements_[n].ucSymbol) return n;
	return -1;
}

// Convert string from first alpha character to element number
int ElementMap::singleAlphaToZ(QString alpha)
{
	// Convert first alpha character.
	QString stripped;
	int n;
	for (n=0; n<alpha.length(); ++n)
	{
		if (alpha[n] == ' ') continue;
		else if (alpha.at(n).isUpper()) { stripped += alpha.at(n); break; }
		else if (alpha.at(n).isLower()) { stripped += alpha.at(n).toUpper(); break; }
		else break;
	}
	for (n=0; n<nElements_; ++n) if (stripped == elements_[n].ucSymbol) return n;
	return -1;
}

// Convert string from name to element number
int ElementMap::nameToZ(QString alpha)
{
	// Ignore numbers. Convert up to non-alpha character.
	QString stripped;
	int n;
	for (n=0; n<alpha.length(); ++n)
	{
		if (alpha.at(n).isUpper()) stripped += alpha.at(n);
		else if (alpha.at(n).isLower()) stripped += alpha.at(n).toUpper();
		else if (alpha.at(n) == '_') break;
	}
	for (n=0; n<nElements_; ++n) if (stripped == elements_[n].ucName) return n;
	return -1;
}

// Convert string from fftype to element number
int ElementMap::ffToZ(QString s)
{
	if (!aten_) return -1;
	ForcefieldAtom* ffa;
	int result = -1;
	for (Forcefield* ff = aten_->forcefields(); ff != NULL; ff = ff->next)
	{
		ffa = ff->findType(s);
		// Found a match, so find out what element it is...
		if (ffa != NULL) result = ffa->neta()->characterElement();
		if (result != -1) break;
	}
	return result;
}

// Search for element named 'query' in the list of known elements
int ElementMap::find(QString query, ElementMap::ZMapType zmt)
{
	// Get the element number from the element name provided.
	Messenger::enter("ElementMap::find");
	int result = -1;
	if (query.isEmpty())
	{
		Messenger::print("Warning: Element search requested on blank string.");
		Messenger::exit("ElementMap::find");
		return 0;
	}

	// Attempt conversion of the string first from any defined mappings
	result = mappings_.data(query);
	if (result != -1) return result;

	// Convert the query string according to the specified rule
	switch (zmt)
	{
		// Automatic determination
		case (ElementMap::AutoZMap):
			// First, try pure numeric conversion
			result = numberToZ(query);
			if (result != -1) break;
			// Then, try alpha conversion
			result = alphaToZ(query);
			if (result != -1) break;
			// Conversion to atomtype name in loaded FF
			result = ffToZ(query);
			if (result != -1) break;
			// Single alpha character conversion
			result = singleAlphaToZ(query);
			if (result != -1) break;
			// Finally, try name conversion
			result = nameToZ(query);
			if (result != -1) break;
			break;
		// Name search
		case (ElementMap::NameZMap):
			result = nameToZ(query);
			break;
		// Search loaded forcefields for atom names
		case (ElementMap::ForcefieldZMap):
			result = ffToZ(query);
			// Attempt an alpha conversion if the FF conversion failed
			if (result == -1) result = alphaToZ(query);
			break;
		// Convert based on alpha-part of atom name only
		case (ElementMap::AlphaZMap):
			result = alphaToZ(query);
			break;
		// Convert based on first alpha-part of atom name only
		case (ElementMap::FirstAlphaZMap):
			result = firstAlphaToZ(query);
			break;
		// Convert based on first alpha-character of atom name only
		case (ElementMap::SingleAlphaZMap):
			result = singleAlphaToZ(query);
			break;
		// Convert based on numeric part only
		case (ElementMap::NumericZMap):
			result = numberToZ(query);
			break;
		default:
			break;
	}
	Messenger::exit("ElementMap::find");
	return ((result == -1) ? 0 : result);
}

// Return first forcefield atom type matching supplied name
ForcefieldAtom* ElementMap::forcefieldAtom(QString name)
{
	ForcefieldAtom* ffa;

	for (Forcefield* ff = aten_->forcefields(); ff != NULL; ff = ff->next)
	{
		ffa = ff->findType(name);
		if (ffa != NULL) return ffa;
	}
	return NULL;
}

/*
 * Data by Z
 */

// Return group number of atomic number 'z'
int ElementMap::group(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::group() : Atomic number %i is out of range.\n", z);
		return -1;
	}
	return elements_[z].group;
}

// Return atomic mass of atomic number 'z'
double ElementMap::atomicMass(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::atomicMass() : Atomic number %i is out of range.\n", z);
		return 0.0;
	}
	return elements_[z].atomicMass;
}

// Return name of atomic number 'z'
const char* ElementMap::name(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::name() : Atomic number %i is out of range.\n", z);
		return "NULL";
	}
	return elements_[z].name;
}

// Return symbol of atomic number 'z'
const char* ElementMap::symbol(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::symbol() : Atomic number %i is out of range.\n", z);
		return "NULL";
	}
	return elements_[z].symbol;
}

// Set radius of atomic number 'z'
void ElementMap::setAtomicRadius(int z, double r)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::setAtomicRadius() : Atomic number %i is out of range.\n", z);
		return;
	}
	elements_[z].atomicRadius = r;
}

// Return effective radius of atomic number 'z'
double ElementMap::atomicRadius(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::atomicRadius() : Atomic number %i is out of range.\n", z);
		return 0.0;
	}
	return elements_[z].atomicRadius;
}

// Return whether radius has changed for ztomic number 'z'
bool ElementMap::radiusHasChanged(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::radiusHasChanged() : Atomic number %i is out of range.\n", z);
		return false;
	}
	return (fabs(elements_[z].atomicRadius-defaultElements_[z].atomicRadius) > 1.0e-5);
}

// Return bond order penalty for TBO 'bo' of atomic number 'z'
int ElementMap::bondOrderPenalty(int z, int bo)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::bondOrderPenalty() : Atomic number %i is out of range.\n", z);
		return 0;
	}
	return elements_[z].bondOrderPenalty[bo];
}

// Return the ambient colour of the element
double* ElementMap::colour(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::colour() : Atomic number %i is out of range.\n", z);
		static double dummyColour[4] = { 0.0, 0.0, 0.0, 1.0 };
		return dummyColour;
	}
	return elements_[z].colour;
}

// Set ambient colour component of element
void ElementMap::setColour(int z, int rgb, double value)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::setColour() : Atomic number %i is out of range.\n", z);
		return;
	}
	elements_[z].colour[rgb] = (GLfloat) value;
}

// Set ambient colour component
void ElementMap::setColour(int z, double r, double g, double b, double a)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::setColour() : Atomic number %i is out of range.\n", z);
		return;
	}
	elements_[z].setColour(r, g, b, a);
}

// Copy the colour of the element into the GLfloat array provided
void ElementMap::copyColour(int z, GLfloat* v)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::copyColour() : Atomic number %i is out of range.\n", z);
		return;
	}
	elements_[z].copyColour(v);
}

// Copy the colour of the element into the double array provided
void ElementMap::copyColour(int z, double* v)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::copyColour() : Atomic number %i is out of range.\n", z);
		return;
	}
	elements_[z].copyColour(v);
}

// Copy the colour of the element into the Vec4 provided
void ElementMap::copyColour(int z, Vec4<GLfloat>& v)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::copyColour() : Atomic number %i is out of range.\n", z);
		return;
	}
	elements_[z].copyColour(v);
}

// Return whether colour of specified element has changed from the default
bool ElementMap::colourHasChanged(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::colourHasChanged() : Atomic number %i is out of range.\n", z);
		return false;
	}
	for (int n=0; n<4; ++n) if (fabs(elements_[z].colour[n]-defaultElements_[z].colour[n]) > 1.0e-5) return true;
	return false;
}

// Return QIcon for the given element
QIcon ElementMap::icon(int z)
{
	if ((z < 0) || (z > nElements_))
	{
		Messenger::error("ElementMap::colourHasChanged() : Atomic number %i is out of range.\n", z);
		return QIcon();
	}

	QPixmap pixmap(32,32);

	QPainter painter(&pixmap);

	// Grab colour and set brush
	QColor colour;
	colour.setRgbF(elements_[z].colour[0], elements_[z].colour[1], elements_[z].colour[2]);
	painter.setBrush(colour);

	// Set up pen
	QPen pen;
	pen.setWidth(2);
	pen.setColor(Qt::black);

	// Set up font
	QFont font;
	font.setPointSize(10);
	
	// Draw rectangle and text
	painter.drawRect(0, 0, 30, 30);
	painter.setFont(font);
	painter.drawText(0, 0, 31, 31, Qt::AlignCenter, elements_[z].symbol);

	painter.end();

	return QIcon(pixmap);
}

/*
 * Data by Atom*
 */

// Return group number of atomic number 'z'
int ElementMap::group(Atom* i)
{
	return group(i->element());
}

// Return atomic mass of atomic number 'z'
double ElementMap::atomicMass(Atom* i)
{
	return atomicMass(i->element());
}

// Return name of atomic number 'z'
const char* ElementMap::name(Atom* i)
{
	return name(i->element());
}

// Return symbol of atomic number 'z'
const char* ElementMap::symbol(Atom* i)
{
	return symbol(i->element());
}

// Return effective radius of atomic number 'z'
double ElementMap::atomicRadius(Atom* i)
{
	return atomicRadius(i->element());
}

// Return bond order penalty for TBO 'bo' of atomic number 'z'
int ElementMap::bondOrderPenalty(Atom* i, int bo)
{
	return bondOrderPenalty(i->element(), bo);
}

// Return the colour of the element
double* ElementMap::colour(Atom* i)
{
	return colour(i->element());
}
