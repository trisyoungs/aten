/*
	*** Command definitions
	*** src/command/commands.cpp
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

#include "command/commandlist.h"
#include "command/commands.h"
#include "classes/bundle.h"
#include "base/sysfunc.h"
#include <string.h>

// Postfix increment operator for command_action enum
command_action &operator++(command_action &ca, int) { return ca = (ca == CA_NITEMS ? CA_NITEMS : command_action(ca + 1)); }

// Command action
commanddata CA_data[CA_NITEMS] = {
	// Variables
	{ "character",		"",		"<variables>",
				"Create character (string) variables with the names provided" },
	{ "integer",		"",		"<variables>",
				"Create integer variables with the names provided" },
	{ "double",		"",		"<variables>",
				"Create double variables with the names provided" },
	{ "atom",		"",		"<variables>",
				"Create atom* variables with the names provided" },
	{ "pattern",		"",		"<variables>",
				"Create pattern* variables with the names provided" },
	{ "model",		"",		"<variables>",
				"Create model* variables with the names provided" },
	{ "bond",		"",		"<variables>",
				"Create bond* variables with the names provided" },
	{ "angle",		"",		"<variables>",
				"Create angle* variables with the names provided" },
	{ "torsion",		"",		"<variables>",
				"Create torsion* variables with the names provided" },
	{ "atomtype",		"",		"<variables>",
				"Create atomtype* variables with the names provided" },
	
	// Root node
	{ "_ROOTNODE_",		"",		"",
				"" },
	
	// Analysis commands
	{ "finalise",		"",		"",
				"Finalise all calculated quantities" },
	{ "frameanalyse",	"",		"",
				"Analyse quantities for the current trajectory frame" },
	{ "modelanalyse",	"",		"",
				"Analyse quantities for the current model" },
	{ "pdens",		"VVVVVV",	"<name> <site1> <site2> <griddelta> <nsteps> <filename>",
				"Request calculation of a probability density between sites" },
	{ "printjobs",		"",		"",
				"Print the current list of quantities to calculate" },
	{ "rdf",		"VVVVVVV",	"<name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>",
				"Request calculation of radial distribution function between sites" },
	{ "savequantities",	"",		"",
				"Save calculated quantities to file" },
	{ "trajanalyse",	"VVv",		"<startframe> <frameskip> [nframes]",
				"Analyse quantities for all frames in current trajectory" },
	
	// Atom commands
	{ "addatom",		"Vvvv",		"<element> [x y z]",
				"Create a new atom in the current model" },
	{ "addatomfrac",	"VVVV",		"<element> <fracx> <fracy> <fracz>",
				"Create a new atom in the current model, converting fractional coordinates to real coordinates" },
	{ "addchain",		"Vv",		"<element> [bondtype]",
				"Create a new atom in the current model, bound to the last" },
	{ "endchain",		"",		"",
				"End the current bond chain, so 'addchain' will create an unbound atom" },
	{ "setcharge",		"Vv",		"<q> [id]",
				"Set the charge of the current (or specified) atom" },
	{ "setcoords",		"VVVv",		"<x> <y> <z> [id]",
				"Set the coordinates of the current (or specified) atom" },
	{ "setelement",		"Vv",		"<element> [id]",
				"Set the element of the current (or specified) atom" },
	{ "setforces",		"VVVv",		"<fx> <fy> <fz> [id]",
				"Set the forces of the current (or specified) atom" },
	{ "setfx",		"Vv",		"<fx> [id]",
				"Set the x force of the current (or specified) atom" },
	{ "setfy",		"Vv",		"<fy> [id]",
				"Set the y force of the current (or specified) atom" },
	{ "setfz",		"Vv",		"<fz> [id]",
				"Set the z force of the current (or specified) atom" },
	{ "setid",		"Vv",		"<id> [id]",
				"Set the id of the current (or specified) atom" },
	{ "setrx",		"Vv",		"<rx> [id]",
				"Set the x coordinate of the current (or specified) atom" },
	{ "setry",		"Vv",		"<ry> [id]",
				"Set the y coordinate of the current (or specified) atom" },
	{ "setrz",		"Vv",		"<rz> [id]",
				"Set the z coordinate of the current (or specified) atom" },
	{ "setvelocities",	"VVVv",		"<vx> <vy> <vz> [id]",
				"Set the velocities of the current (or specified) atom" },
	{ "setvx",		"Vv",		"<vx> [id]",
				"Set the x velocity of the current (or specified) atom" },
	{ "setvy",		"Vv",		"<vy> [id]",
				"Set the y velocity of the current (or specified) atom" },
	{ "setvz",		"Vv",		"<vz> [id]",
				"Set the z velocity of the current (or specified) atom" },
	
	// Bonding commands
	{ "addbond",		"VVv",		"<atom1> <atom2> [bondtype]",
				"Create a bond between specified atoms" },
	{ "addbondid",		"VVv",		"<id1> <id2> [bondtype]",
				"Create a bond between atoms with ids specified" },
	{ "augment",		"",		"",
				"Automatically augment all bonds in the current model" },
	{ "bondtolerance",	"V",		"<tolerance>",
				"Set bonding tolerance for automatic calculation" },
	{ "bondpatterns",	"",		"",
				"Calculate bonds between atoms, restricted to atoms in pattern molecules" },
	{ "bondselection",	"",		"",
				"Calculate bonds between atoms in the current selection" },
	{ "clearbonds",		"",		"",
				"Delete all bonds in the current model" },
	{ "rebond",		"",		"",
				"Calculate bonding in the current model" },
	
	// Build commands
	{ "addhydrogen",	"v",		"[atom|id]",
				"Hydrogen satisfy all (or specified) atom in model" },
	{ "delete",		"",		"",
				"Delete selected atoms" },
	{ "locate",		"VVV",		"<x> <y> <z>",
				"Position pen at specified coordinates" },
	{ "move",		"VVV",		"<dx> <dy> <dz>",
				"Move pen by specified coordinates" },
	{ "rotx",		"V",		"<angle>",
				"Rotate pen x axis by given angle" },
	{ "roty",		"V",		"<angle>",
				"Rotate pen y axis by given angle" },
	{ "rotz",		"V",		"<angle>",
				"Rotate pen z axis by given angle" },
	{ "transmute",		"V",		"<element>",
				"Transmute selection to element given" },
	
	// Cell commands
	{ "fold",		"",		"",
				"Fold atoms into model's unit cell" },
	{ "fractoreal",		"",		"",
				"Convert (assumed) fractional model coordinates to real coordinates" },
	{ "pack",		"",		"",
				"Pack the unit cell with symmetry operators list in associated spacegroup" },
	{ "printcell",		"",		"",
				"Print the unit cell of the current model" },
	{ "replicatecell",	"VVVVVV",	"<negx> <negy> <negz> <posx> <posy> <posz>",
				"Replicate the cell along the directions given" },
	{ "scalecell",		"VVV",		"<x> <y> <z>",
				"Scale the unit cell of the current model" },
	{ "setcell",		"VVVVVV",	"<a> <b> <c> <alpha> <beta> <gamma>",
				"Set or create a unit cell for the current model from lengths/angles provided" },
	{ "setcellaxes",	"VVVVVVVVV",	"<ax> <ay> <az> <bx> <by> <bz> <cx> <cy> <cz>",
				"Set or create a unit cell for the current model from the cell axes provided" },
	{ "setspacegroup",	"V",		"<spgrp>",
				"Set the spacegroup for the current model" },
	
	// Charge commands
	{ "chargeff",		"",		"",
				"Charge atoms in the model according to their forcefield atom types" },
	{ "chargefrommodel",	"",		"",
				"Charge atoms in the current trajectory frame from the parent model" },
	{ "chargepatom",	"VV",		"<pattern> <id> <q>",
				"Set charges for specific atoms in all molecules of the specified pattern" },
	{ "chargeselection",	"V",		"<q>",
				"Set charges of atoms in the current selection" },
	{ "chargetype",		"VV",		"<type> <q>",
				"Set charges of all atoms of the given type" },
	{ "clearcharges",	"",		"",
				"Zero all charges in the current model" },
	
	// Disordered Builder Commands
	{ "addcomponent",	"VVV",		"<name> <model> <nmols>",
				"Add a model to the list of disordered builder components" },
	{ "disorder",		"V",		"<nsteps>",
				"Run the disordered builder" },
	{ "printcomponents",	"",		"",
				"Print a list of the components requested in the disordered builder" },
	{ "setcentre",		"VVVV",		"<name> <x> <y> <z>",
				"Set the region centre of the named component" },
	{ "setgeometry",	"VVVVv",	"<name> <x> <y> <z> [l]",
				"Set the region geometry of the named component" },
	{ "setoverlap",		"VV",		"<name> yes|no",
				"Set the overlap flag of the named component" },
	{ "setshape",		"VV",		"<name> <shape>",
				"Set the region shape of the named component" },
	{ "vdwscale",		"V",		"<scale>",
				"Set the VDW scaling factor to use in the disordered builder" },
	
	// Energy commands
	{ "frameenergy",	"",		"",
				"Calculate the energy of the current trajectory frame" },
	{ "modelenergy",	"",		"",
				"Calculate the energy of the current model" },
	{ "printelec",		"",		"",
				"Print the electrostatic pattern matrix of the last calculated energy" },
	{ "printewald",		"",		"",
				"Print the Ewald decomposition of the last calculated energy" },
	{ "printinter",		"",		"",
				"Print the total intermolecular pattern matrix of the last calculated energy" },
	{ "printintra",		"",		"",
				"Print the total intramolecular pattern matrix of the last calculated energy" },
	{ "printenergy",	"",		"",
				"Print a short description of the last calculated energy" },
	{ "printsummary",	"",		"",
				"Print a one-line summary of the last calculated energy" },
	{ "printvdw",		"",		"",
				"Print the VDW pattern matrix of the last calculated energy" },
	
	// Expression commands
	{ "createexpression",	"",		"",
				"Create and fill a forcefield expression for the current model" },
	{ "printsetup",		"",		"",
				"Print the current energy/force calculation setup" },
	{ "saveexpression",	"VV",		"<format> <filename>",
				"Save the expression for the current model" },

	// Flow control
	{ "else",		"",		"",
				"Perform the subsequent block if all previous if/elseif tests failed" },
	{ "elseif",		"VSE",		"",
				"Perform a conditional test on the supplied variable against the second variable (or constant), if all previous tests failed" },
	{ "end",		"",		"",
				"End the current for/if block" },
	{ "for",		"Vvv",		"<variable> [start] [end]",
				"" },
	{ "_GOTO_",		"",		"",
				"" },
	{ "_GOTONONIF_",	"",		"",
				"" },
	{ "if",			"VSV",		"<variable> <condition> <variable|constant>",
				"Perform a conditional test on the supplied variable against the second variable (or constant)" },
	{ "_TERMINATE_",	"",		"",
				"" },
	
	// Force commands
	{ "frameforces",	"",		"",
				"Calculate forces for the current trajectory frame" },
	{ "modelforces",	"",		"",
				"Calculate forces for the current model" },
	{ "printforces",	"",		"",
				"Print calculated forces for the current model" },
	
	// Forcefield commands
	{ "ffmodel",		"",		"",
				"Associate current forcefield to current model" },
	{ "ffpattern",		"",		"",
				"Associate current forcefield to current pattern" },
	{ "ffpatternid",	"V",		"<patternid>",
				"Associate current forcefield to specified pattern ID" },
	{ "loadff",		"Vv",		"<filename> [name]",
				"Load forcefield" },
	{ "selectff",		"V",		"<name>",
				"Select named (loaded) forcefield and make it current" },
	{ "typemodel",		"",		"",
				"Perform atom typing on the current model" },
	{ "typetest",		"V",		"<ffid> <atomid>",
				"Test atomtype score on atom provided" }, 

	// Glyph commands
	{ "addglyph",		"V",		"<style>",
				"Add a glyph with the specified style to the current model" },
	{ "setglyphatom",	"Vv",		"<n> [atom|atomid]",
				"Set current (or specified) atom as data <n> in current glyph" },
	{ "setglyphdata",	"VVVV",		"<n> <x> <y> <z>",
				"Set vector data <n> in current glyph" },

	// Grid commands
	{ "addgridpoint",	"VVVV",		"<ix> <iy> <iz> <value>",
				"Set specific gridpoint value" },
	{ "addnextgridpoint",	"V",		"<value>",
				"Add next gridpoint value" },
	{ "finalisegrid",	"",		"",
				"Finalise grid import" },
	{ "newgrid",		"V",		"<title>",
				"Create new grid data" },
	{ "setgrid",		"VVVVVVVVV",	"<ax> <ay> <az> <bx> <by> <bz> <cx> <cy> <cz>",
				"Set axes for current grid" },
	{ "setgridcubic",	"V",		"<l>",
				"Set the axes system for the current grid to be cubic" },
	{ "setgridlooporder",	"V",		"<xyz|zyx|213...>",
				"Set the loop ordering to use in 'addnextgridpoint'" },
	{ "setgridorigin",	"VVV",		"<x> <y> <z>",
				"Set the origin of the axes system for the current grid" },
	{ "setgridortho",	"VVV",		"<a> <b> <c>",
				"Set the axes system for the current grid to be orthorhombic" },
	{ "setgridsize",	"VVV",		"<nx> <ny> <nz>",
				"Set the number of points along each axis for the current grid" },
	
	// Image commands
	{ "savebitmap",		"VV",		"<format> <filename>",
				"Save the current model view as a bitmap image: formats available are bmp, jpg, png, ppm, xbm, and xpm" },
	{ "savevector",		"VV",		"<format> <filename>",
				"Save the current model view as a vector image: formats available are ps, eps, tex, pdf, svg, and pgf" },
	
	// Labeling commands
	{ "clearlabels",	"",		"",
				"Remove all atom labels in the current model" },
	{ "addlabel",		"V",		"<label>",
				"Add labels to the current atom selection" },
	{ "removelabel",	"V",		"<label>",
				"Remove labels from the current atom selection" },
	
	// MC commands
	{ "mcaccept",		"VV",		"<movetype> <energy>",
				"Set Monte Carlo move type acceptance energies" },
	{ "mcallow",		"VV",		"<movetype> yes|no",
				"Restrict or allow Monte Carlo move types" },
	{ "mcmaxstep",		"VV",		"<movetype> <step>",
				"Set maximum step sizes for Monte Carlo move types" },
	{ "mcntrials",		"VV",		"<movetype> <ntrials>",
				"Set trial numbers for Monte Carlo move types" },
	{ "printmc",		"",		"",
				"Print current Monte Carlo parameters" },
	
	// Messaging
	{ "error",		"G",		"<message>",
				"Raise an error message (causes exit of current command list)" },
	{ "print",		"G",		"<message>",
				"Print a message" },
	{ "verbose",		"G",		"<message>",
				"Print a message when verbose output is enabled" },
	{ "warn",		"G",		"<message>",
				"Raise a warning message (command list will continue)" },
	
	// Minimisation commands
	{ "cgminimise",		"",		"",
				"Run a conjugate gradient minimiser on the current model" },
	{ "converge",		"VV",		"<energy> <forces>",
				"Set energy and RMS force convergence limits for minimisation algorithms" },
	{ "linetol",		"V",		"<tolerance>",
				"Set tolerance of line minimiser" },
	{ "mcminimise",		"V",		"<maxsteps>",
				"Run Monte Carlo minimiser on the current model" },
	{ "sdminimise",		"V",		"<maxsteps>",
				"Run steepest descent minimiser on the current model" },
	{ "simplexminimise",	"",		"",
				"Run the Simplex minimiser on the current model" },
	
	// Model commands
	{ "createatoms",	"",		"",
				"Create enough atoms in the current trajectory frame to match the parent model" },
	{ "finalisemodel",	"",		"",
				"Finalise the current model" },
	{ "listmodels",		"",		"",
				"List the currently-loaded models" },
	{ "loadmodel",		"Vv",		"<filename> [name]",
				"Load a model from file" },
	{ "modeltemplate",	"",		"",
				"Template the atoms in the current trajectory frame, matching the parent model" },
	{ "newmodel",		"V",		"<name>",
				"Create a new model" },
	{ "printmodel",		"",		"",
				"Print data on the current model" },
	{ "savemodel",		"VV",		"<format> <filename>",
				"Save the current model to <filename> in the specified model <format>" },
	{ "selectmodel",	"V",		"<name>",
				"Select the named (loaded) model and make it current" },
	{ "settitle",		"V",		"<title>",
				"Set the title of the current model" },
	
	// Pattern commands
	{ "addpattern",		"VVV",		"<name> <nmols> <natoms>",	
				"Add a pattern definition to the current model" },
	{ "clearpatterns",	"",		"",
				"Remove all pattern definitions from the current model" },
	{ "createpatterns",	"",		"",
				"Automatically determine pattern definitions for the current model" },
	{ "printpatterns",	"",		"",
				"Print the pattern definition for the current model" },
	{ "selectpattern",	"V",		"<name>",
				"Select the named pattern and make it current" },
	
	// Preferences commands
	{ "atomdetail",		"V",		"<n>",
				"Set the quadric detail of atoms" },
	{ "bonddetail",		"V",		"<n>",
				"Set the quadric detail of bonds" },
	{ "colour",		"VVVV",		"<colour> <r> <g> <b>",
				"Set the specified colour" },
	{ "densityunits",	"V",		"atomsperang|gpercm",
				"Set the unit of density to use" },
	{ "ecut",		"V",		"<cutoff>",
				"Set the electrostatic cutoff distance" },
	{ "elec",		"Vvvvv",	"<none|coulomb|ewald|ewaldauto> [ [precision] | [alpha] [kx] [ky] [kz] ]",
				"Set the style of electrostatic energy calculation" },
	{ "elementambient",	"VVVV",		"<element> <r> <g> <b>",
				"Set ambient colour of element" },
	{ "elementdiffuse",	"VVVV",		"<element> <r> <g> <b>",
				"Set diffuse colour of element" },
	{ "elementradius",	"VV",		"<element> <radius>",
				"Set effective radius of element" },
	{ "energyunits",	"V",		"j|kj|cal|kcal|ha",
				"Set the unit of energy to use" },
	{ "gl",			"VV",		"<option> on|off",
				"Turn on/off various OpenGL options: fog, linealias, polyalias, backcull" },
	{ "intra",		"V",		"on|off",
				"Turn on/off energy and force calculation of intramolecular terms" },
	{ "key",		"VV",		"ctrl|shift|alt <action>",
				"Set the action of modifier keys" },
	{ "mouse",		"VV",		"left|middle|right|wheel <action>",
				"Set the action of mouse buttons" },
	{ "radius",		"VV",		"<style> <r>",
				"Set the general atom scales for view styles" },
	{ "shininess",		"V",		"<n>",
				"Set the shininess of atoms" },
	{ "show",		"VV",		"<object> yes|no",
				"Set the visibility of view objects: atoms, cell, cellaxes, cellrepeat, forcearrows, globe, labels, measurements, regions" },
	{ "style",		"V",		"style <style>",
				"Draw models in the specified style" },
	{ "vcut",		"V",		"<cutoff>",
				"Set the VDW cutoff distance" },
	{ "vdw",		"V",		"on|off",
				"Turn on/off VDW energy/force calculation" },

	// Read / Write Commands
	{ "addreadoption",	"V",		"<option>",
				"Add a read option: usequotes, skipblanks, stripbrackets" },
	{ "find",		"VVv",		"<string> <resultvar> [linevar]",
				"Search for a string in the input file" },
	{ "readchars",		"VV",		"<variable> <nchars>",
				"Read a number of characters from the input file" },
	{ "readdouble",		"V",		"<variable>",
				"Read a double (8-byte) value from the input file" },
	{ "readint",		"V",		"<variable>",
				"Read an integer (4-byte) value from the input file" },
	{ "readline",		"F",		"<format>",
				"Read and parse a line from the input file" },
	{ "readnext",		"V",		"<variable>",
				"Read the next delimited item from the file" },
	{ "readvar",		"VF",		"<variable> <format>",
				"Parse a variable according to the supplied format" },
	{ "removereadoption",	"V",		"<option>",
				"Remove a read option" },
	{ "rewind",		"",		"",
				"Rewind to the start of the input file" },
	{ "skipchars",		"V",		"<nchars>",
				"Skip a number of characters in the input file" },
	{ "skipline",		"v",		"[nlines]",
				"Skip a number of lines in the input file" },
	{ "writeline",		"G",		"<format>",
				"Write a line to the output file" },
	
	// Selection commands
	{ "selectall",		"",		"",
				"Select all atoms in the current model" },
	{ "selectatom",		"V",		"<id>",
				"Select specific atoms in the current model" },
	{ "selectelement",	"V",		"<el>",
				"Select all atoms of a specific element" },
	{ "selectfftype",	"V",		"<typename>",
				"Select all atoms of a specific forcefield type" },
	{ "invert",		"",		"",
				"Invert the current selection" },
	{ "selectnone",		"",		"",
				"Deselect all atoms in the current model" },
	{ "selectoverlaps",	"V",		"<tolerance>",
				"Select all atoms which are within a given distance of each other" },
	{ "selecttype",		"VV",		"<element> <typedesc>",
				"Select all atoms that match the provided atomtype description" },
	
	// Site commands
	{ "addsite",		"VVv",		"<name> <pattern> [atomlist]",
				"Adds a new site definition to the current model" },
	{ "printsites",		"",		"",
				"Print all sites defined for the current model" },
	{ "selectsite",		"V",		"<name>",
				"Select the defined site and make it current" },
	{ "setaxes",		"VV",		"<atomlist> <atomlist>",
				"Set the axis definitions for the current site" },

	// System commands
	{ "gui",		"",		"",
				"Start the GUI (if it is not already active)" },
	{ "help",		"V",		"<command>",
				"Provide short help on the command supplied" },
	{ "quit",		"",		"",
				"Exit the program" },

	// Trajectory commands
	{ "firstframe",		"",		"",
				"Go to the first frame in the current trajectory" },
	{ "lastframe",		"",		"",
				"Go to the last frame in the current trajectory" },
	{ "loadtrajectory",	"V",		"<filename>",
				"Load the specified trajectory and associate it to the current model" },
	{ "nextframe",		"",		"",
				"Go to the next frame in the current trajectory" },
	{ "prevframe",		"",		"",
				"Go to the previous frame in the current trajectory" },
	
	// Transformation commands		// Transformation commands
	{ "centre",		"VVV",		"<x> <y> <z>",
				"Centre the atom selection of the current model at the specified coordinates" },
	{ "translate",		"VVV",		"<dx> <dy> <dz>",
				"Translate the atom selection of the current model" },
	{ "translateatom",	"VVV",		"<dx> <dy> <dz>",
				"Translate the current atom" },
	{ "mirror",		"V",		"<axis>",
				"Mirror the atom selection of the current model about its geometric centre in the specified axis" },
	
	// Variables
	{ "dec",		"V",		"<variable>",
				"Decrease the specified variable" },
	{ "eval",		"V=E",		"<variable> = <expression>",
				"Evaluate the given expression into the supplied variable" },
	{ "inc",		"V",		"<variable>",
				"Increase the specified variable" },
	{ "let",		"V=V",		"<variable> = <variable>",
				"Set the specified variable to a supplied value/variable" },
	
	// View
	{ "resetview",		
				"Reset the camera and rotation for the current model" },
	{ "rotateview",		"VV",		"<x> <y>",
				"Rotate the current model about the x and y axes by the specified amounts" },
	{ "speedtest",		"v",		"[nrender]",
				"Time 100 (or [nrender]) updates of the model display." },
	{ "translateview",	"VVV",		"<dx> <dy> <dz>",
				"Translate the camera for the current model" },
	{ "viewalong",		"VVV",		"<x> <y> <z>",
				"Set the rotation for the current model so the view is along the specified vector" },
	{ "viewalongcell",	"VVV",		"<x> <y> <z>",
				"Set the rotation for the current model so the view is along the specified cell vector" },
	{ "zoomview",		"V",		"<dz>",
				"Zoom in/out the camera - equivalent to 'translateview 0 0 dz'" },
	{ "zrotateview",	"V",		"<dr>",
				"Rotate the model in the plane of the screen" }
};

command_action CA_from_text(const char* s)
{
	command_action result;
	for (result = CA_CHAR; result < CA_NITEMS; result++) if (strcmp(CA_data[result].keyword,s) == 0) break;
	return result;
}
