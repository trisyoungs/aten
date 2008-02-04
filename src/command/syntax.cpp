/*
	*** Command syntax
	*** src/command/scriptsyntax.cpp
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

#include "command/commandlist.h"
#include "base/sysfunc.h"
#include <string.h>

// Script command data
const char *CA_syntax[CA_NITEMS] = {

	// Variables
	"character <variables>,Create character (string) variables with the names provided",
	"integer <variables>,Create integer variables with the names provided",
	"double <variables>,Create double variables with the names provided",
	"atom <variables>,Create atom* variables with the names provided",
	"pattern <variables>,Create pattern* variables with the names provided",
	"model <variables>,Create model* variables with the names provided",
	"bond <variables>,Create bond* variables with the names provided",
	"angle <variables>,Create angle* variables with the names provided",
	"torsion <variables>,Create torsion* variables with the names provided",
	"atomtype <variables>,Create atomtype* variables with the names provided",

	// Root node
	"_ROOTNODE_",
	"help <command>,Provide short help on the command supplied",

	// Analysis
	"finalise,Finalise all calculated quantities",
	"frameanalyse,Analyse quantities for the current trajectory frame",
	"modelanalyse,Analyse quantities for the current model",
	"pdens <name> <site1> <site2> <griddelta> <nsteps> <filename>,Request calculation of a probability density between sites",
	"printjobs,Print the current list of quantities to calculate",
	"rdf <name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>,Request calculation of radial distribution function between sites",
	"savequantities,Save calculated quantities to file",
	"trajanalyse <startframe> <frameskip> [nframes],Analyse quantities for all frames in current trajectory",

	// Atoms
	"addatom <element>,Create a new atom in the current model",
	"addchain <element> [bondtype],Create a new atom in the current model, bound to the last",
	"endchain,End the current bond chain, so 'addchain' will create an unbound atom",
	"setcharge <q> [id],Set the charge of the current (or specified) atom",
	"setcoords <x> <y> <z> [id],Set the coordinates of the current (or specified) atom",
	"setelement <element> [id],Set the element of the current (or specified) atom",
	"setforces <fx> <fy> <fz> [id],Set the forces of the current (or specified) atom",
	"setfx <fx> [id],Set the x force of the current (or specified) atom",
	"setfy <fy> [id],Set the y force of the current (or specified) atom",
	"setfz <fz> [id],Set the z force of the current (or specified) atom",
	"setid <id> [id],Set the id of the current (or specified) atom",
	"setrx <rx> [id],Set the x coordinate of the current (or specified) atom",
	"setry <ry> [id],Set the y coordinate of the current (or specified) atom",
	"setrz <rz> [id],Set the z coordinate of the current (or specified) atom",
	"setvelocities <vx> <vy> <vz> [id],Set the velocities of the current (or specified) atom",
	"setvx <vx> [id],Set the x velocity of the current (or specified) atom",
	"setvy <vy> [id],Set the y velocity of the current (or specified) atom",
	"setvz <vz> [id],Set the z velocity of the current (or specified) atom",

	// Bonding
	"addbond <atom1> <atom2> [bondtype],Create a bond between specified atoms",
	"addbondid <id1> <id2> [bondtype],Create a bond between atoms with ids specified",
	"augment,Automatically augment all bonds in the current model",
	"bondtolerance <tolerance>,Set bonding tolerance for automatic calculation",
	"bondpatterns,Calculate bonds between atoms, restrict to atoms in patterns",
	"bondselection,Calculate bonds between atoms in the current selection",
	"clearbonds,Delete all bonds in the current model",
	"rebond,Calculate bonding in the current model",

	// Build commands
	"addhydrogen [atom|id],Hydrogen satisfy all (or specified) atom in model",
	"delete,Delete selected atoms",
	"locate <x> <y> <z>,Position pen at specified coordinates",
	"move <dx> <dy> <dz>,Move pen by specified coordinates",
	"rotx <angle>,Rotate pen x axis by given angle",
	"roty <angle>,Rotate pen y axis by given angle",
	"rotz <angle>,Rotate pen z axis by given angle",
	"transmute <element>,Transmute selection to element given",

	// Cell commands
	"fold,Fold atoms into model's unit cell",
	"fractoreal,Convert (assumed) fractional model coordinates to real coordinates",
	"pack,Pack the unit cell with symmetry operators list in associated spacegroup",
	"printcell,Print the unit cell of the current model",
	"replicatecell <negx> <negy> <negz> <posx> <posy> <posz>,Replicate the cell along the directions given",
	"scalecell <x> <y> <z>,Scale the unit cell of the current model",
	"setcell <a> <b> <c> <alpha> <beta> <gamma>,Set or create a unit cell for the current model from lengths/angles provided",
	"setcellaxes <ax> <ay> <az> <bx> <by> <bz> <cx> <cy> <cz>,Set or create a unit cell for the current model from the cell axes provided",
	"setspacegroup <spgrp>,Set the spacegroup for the current model",

	// Charge commands
	"chargeff,Charge atoms in the model according to their forcefield atom types",
	"chargefrommodel,Charge atoms in the current trajectory frame from the parent model",
	"chargepatom <pattern> <id> <q>,Set charges for specific atoms in all molecules of the specified pattern",
	"chargeselection <q>,Set charges of atoms in the current selection",
	"chargetype <type> <q>,Set charges of all atoms of the given type",
	"clearcharges,Zero all charges in the current model",

	// Disordered Builder Commands
	"addcomponent <name> <model> <nmols>,Add a model to the list of disordered builder components",
	"disorder <nsteps>,Run the disordered builder",
	"printcomponents,Print a list of the components requested in the disordered builder",
	"setcentre <name> <x> <y> <z>,Set the region centre of the named component",
	"setgeometry <name> <x> <y> <z> [l],Set the region geometry of the named component",
	"setoverlap <name> yes|no,Set the overlap flag of the named component",
	"setshape <name> <shape>,Set the region shape of the named component",
	"vdwscale <scale>,Set the VDW scaling factor to use in the disordered builder",

	// Energy commands
	"frameenergy,Calculate the energy of the current trajectory frame",
	"modelenergy,Calculate the energy of the current model",
	"printelec,Print the electrostatic pattern matrix of the last calculated energy",
	"printewald,Print the Ewald decomposition of the last calculated energy",
	"printinter,Print the total intermolecular pattern matrix of the last calculated energy",
	"printintra,Print the total intramolecular pattern matrix of the last calculated energy",
	"printenergy,Print a short description of the last calculated energy",
	"printsummary,Print a one-line summary of the last calculated energy",
	"printvdw,Print the VDW pattern matrix of the last calculated energy",

	// Expression commands
	"createexpression,Create and fill a forcefield expression for the current model",
	"ecut <cutoff>,Set the electrostatic cutoff distance",
	"elec <none|coulomb|ewald|ewaldauto> [ [precision] | [alpha] [kx] [ky] [kz] ],Set the style of electrostatic energy calculation",
	"intra on|off,Turn on/off energy and force calculation of intramolecular terms",
	"printsetup,Print the current energy/force calculation setup",
	"vcut <cutoff>,Set the VDW cutoff distance",
	"vdw on|off,Turn on/off VDW energy/force calculation",

	// Field commands
	"savefield <format> <filename>,Save a forcefield definition of the current model",

	// Flow control
	"else",
	"elseif,rxe",
	"end",
	"for <variable> [start] [end]",
	"_GOTO_",
	"_GOTONONIF_",
	"if,rxe",
	"quit",
	"_TERMINATE_",

	// Force commands
	"frameforces,Calculate forces for the current trajectory frame",
	"modelforces,Calculate forces for the current model",
	"printforces,Print calculated forces for the current model",

	// Forcefield commands
	"ffmodel,Associate current forcefield to current model",
	"ffpattern,Associate current forcefield to current pattern",
	"ffpatternid <patternid>,Associate current forcefield to specified pattern ID",
	"loadff <filename> [name],Load forcefield",
	"selectff <name>,Select named (loaded) forcefield and make it current",
	"typemodel,Perform atom typing on the current model",
	"typetest <ffid> <atomid>,Test atomtype score on atom provided",

	// Grid commands
	"addgridpoint <ix> <iy> <iz> <value>,Set specific gridpoint value",
	"addnextgridpoint <value>,Add next gridpoint value",
	"finalisegrid,Finalise grid import",
	"newgrid <title>,Create new grid data",
	"setgrid <ax> <ay> <az> <bx> <by> <bz> <cx> <cy> <cz>,Set axes for current grid",
	"setgridcubic <l>,Set the axes system for the current grid to be cubic",
	"setgridorigin <x> <y> <z>,Set the origin of the axes system for the current grid",
	"setgridortho <a> <b> <c>,Set the axes system for the current grid to be orthorhombic",
	"setgridsize <nx> <ny> <nz>,Set the number of points along each axis for the current grid",

	// Image commands
	"savebitmap <format> <filename>,Save the current model view as a bitmap image: formats available are bmp, jpg, png, ppm, xbm, and xpm",
	"savevector <format> <filename>,Save the current model view as a vector image: formats available are ps, eps, tex, pdf, svg, and pgf",

	// Labeling commands
	"clearlabels,Remove all atom labels in the current model",
	"addlabel <label>,Add labels to the current atom selection",
	"removelabel <label>,Remove labels from the current atom selection",

	// MC commands
	"mcaccept <movetype> <energy>,Set Monte Carlo move type acceptance energies",
	"mcallow <movetype> yes|no,Restrict or allow Monte Carlo move types",
	"mcmaxstep <movetype> <step>,Set maximum step sizes for Monte Carlo move types",
	"mcntrials <movetype> <ntrials>,Set trial numbers for Monte Carlo move types",
	"printmc,Print current Monte Carlo parameters",

	// Messaging
	"error <message>,Raise an error message (causes exit of current command list)",
	"print <message>,Print a message",
	"warn <message>,Raise a warning message (command list will continue)",

	// Minimisation commands
	"cgminimise,Run a conjugate gradient minimiser on the current model",
	"converge <energy> <forces>,Set energy and RMS force convergence limits for minimisation algorithms",
	"linetol <tolerance>,Set tolerance of line minimiser",
	"mcminimise <maxsteps>,Run Monte Carlo minimiser on the current model",
	"sdminimise <maxsteps>,Run steepest descent minimiser on the current model",
	"simplexminimise,Run the Simplex minimiser on the current model",

	// Model commands
	"createatoms,Create enough atoms in the current trajectory frame to match the parent model",
	"finalisemodel,Finalise the current model",
	"listmodels,List the currently-loaded models",
	"loadmodel <filename> [name],Load a model from file",
	"modeltemplate,Template the atoms in the current trajectory frame, matching the parent model",
	"newmodel <name>,Create a new model",
	"printmodel,Print data on the current model",
	"savemodel <format> <filename>,Save the current model",
	"selectmodel <name>,Select the named (loaded) model and make it current",
	"settitle <title>,Set the title of the current model",

	// Pattern commands
	"addpattern <name> <nmols> <natoms>,Add a pattern definition to the current model",
	"clearpatterns,Remove all pattern definitions from the current model",
	"createpatterns,Automatically determine pattern definitions for the current model",
	"printpatterns,Print the pattern definition for the current model",
	"selectpattern <name>,Select the named pattern and make it current",

	// Preferences commands
	"atomdetail <n>,Set the quadric detail of atoms",
	"bonddetail <n>,Set the quadric detail of bonds",
	"colour <colour> <r> <g> <b>,Set the specified colour",
	"densityunits atomsperang|gpercm,Set the unit of density to use",
	"elementambient <element> <r> <g> <b>,Set ambient colour of element",
	"elementdiffuse <element> <r> <g> <b>,Set diffuse colour of element",
	"elementradius <element> <radius>,Set effective radius of element",
	"energyunits j|kj|cal|kcal|ha,Set the unit of energy to use",
	"gl <option> on|off,Turn on/off various OpenGL options: fog, linealias, polyalias, backcull",
	"key ctrl|shift|alt <action>,Set the action of modifier keys",
	"mouse left|middle|right|wheel <action>,Set the action of mouse buttons",
	"radius <style> <r>,Set the general atom scales for view styles",
	"shininess <n>,Set the shininess of atoms",
	"show <object> yes|no,Set the visibility of view objects: atoms, cell, cellaxes, cellrepeat, forcearrows, globe, labels, measurements, regions",
	"style <style>,Draw models in the specified style",

	// Read / Write Commands
	"addreadoption <option>,Add a read option: usequotes, skipblanks, stripbrackets",
	"find <string> <resultvar> [linevar],Search for a string in the input file",
	"readchars <variable> <nchars>,Read a number of characters from the input file",
	"readdouble <variable>,Read a double (8-byte) value from the input file",
	"readint <variable>,Read an integer (4-byte) value from the input file",
	"readline <format>,Read and parse a line from the input file",
	"readnext <variable>,Read the next delimited item from the file",
	"readvar <variable> <format>,Parse a variable according to the supplied format",
	"removereadoption <option>,Remove a read option",
	"rewind,Rewind to the start of the input file",
	"skipchars <nchars>,Skip a number of characters in the input file",
	"skipline [nlines],Skip a number of lines in the input file",
	"writeline <format>,Write a line to the output file",

	// Selection commands
	"selectall,Select all atoms in the current model",
	"selectatom <id>,Select specific atoms in the current model",
	"selectelement <el>,Select all atoms of a specific element",
	"selectfftype <typename>,Select all atoms of a specific forcefield type",
	"invert,Invert the current selection",
	"selectnone,Deselect all atoms in the current model",
	"selectoverlaps <tolerance>,Select all atoms which are within a given distance of each other",
	"selecttype <element> <typedesc>,Select all atoms that match the provided atomtype description",

	// Site commands
	"addsite <name> <pattern> [atomlist],Adds a new site definition to the current model",
	"printsites,Print all sites defined for the current model",
	"selectsite <name>,Select the defined site and make it current",
	"setaxes <atomlist> <atomlist>,Set the axis definitions for the current site",

	// Trajectory commands
	"firstframe,Go to the first frame in the current trajectory",
	"lastframe,Go to the last frame in the current trajectory",
	"loadtrajectory <filename>,Load the specified trajectory and associate it to the current model",
	"nextframe,Go to the next frame in the current trajectory",
	"prevframe,Go to the previous frame in the current trajectory",

	// Transformation commands
	"centre <x> <y> <z>,Centre the atom selection of the current model at the specified coordinates",
	"translate <dx> <dy> <dz>,Translate the atom selection of the current model",
	"translateatom <dx> <dy> <dz>,Translate the current atom",
	"mirror <axis>,Mirror the atom selection of the current model about its geometric centre in the specified axis",

	// Variable commands
	"dec <variable>,Decrease the specified variable",
	"eval <variable> = <expression>,Evaluate the given expression into the supplied variable",
	"inc <variable>,Increase the specified variable",
	"let <variable> = <variable>,Set the specified variable to a supplied value/variable",

	// View
	"resetview,Reset the camera and rotation for the current model",
	"rotateview <x> <y>,Rotate the current model about the x and y axes by the specified amounts",
	"translateview <dx> <dy> <dz>,Translate the camera for the current model",
	"viewalong <x> <y> <z>,Set the rotation for the current model so the view is along the specified vector",
	"viewalongcell <x> <y> <z>,Set the rotation for the current model so the view is along the specified cell vector",
	"zoomview <dz>,Zoom in/out the camera - equivalent to 'translateview 0 0 dz'",
	"zrotateview <dr>,Rotate the model in the plane of the screen"

	};
const char *syntax_from_CA(command_action ca)
	{ return get_before_comma(CA_syntax[ca]); }
const char *description_from_CA(command_action ca)
	{ return get_after_comma(CA_syntax[ca]); }

