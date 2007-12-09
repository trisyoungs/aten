/*
	*** Script definition
	*** src/script/script.h
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

#ifndef H_SCRIPT_H
#define H_SCRIPT_H

#include "command/commandlist.h"
#include "templates/vector3.h"
#include "model/model.h"
#include "base/sysfunc.h"

// Script
class script
{
	public:
	// Constructor / Destructor
	script();
	~script();
	// List pointers
	script *prev, *next;

	/*
	// Script
	*/
	private:
	// Active pattern within the script
	pattern *activepattern;

	// Active atom within the script
	atom *activeatom;
	// Active bond within the script
	bond *activebond;
	// Active site within the script
	site *activesite;
	// Returns master's active model
	model *check_activemodel(const char*);
	// Returns TRUE if a valid activepattern exists
	bool check_activepattern(const char*);
	// Returns master's active forcefield
	forcefield *check_activeff(const char*);
	// Return the active site of the current model
	site *check_activesite(const char*);
	// Returns TRUE if a valid activeatom exists
	bool check_activeatom(const char*);
	// Returns TRUE if a valid activebond exists
	bool check_activebond(const char*);
	// Returns TRUE is the activemodel has a trajectory
	bool check_traj(const char*);

	public:
	// List of converted commands from file
	commandlist<script_command> commands;

	/*
	// Command Subroutines
	*/
	private:
	// Analyse commands
	bool command_analyse(command_node<script_command>*);
	// Bonding commands
	bool command_bonds(command_node<script_command>*);
	// Atom building commands
	bool command_build(command_node<script_command>*);
	// Cell commands
	bool command_cell(command_node<script_command>*);
	// Charge commands
	bool command_charge(command_node<script_command>*);
	// Disorder commands
	bool command_disorder(command_node<script_command>*);
	// Energy commands 
	bool command_energy(command_node<script_command>*);
	// Expression commands
	bool command_expr(command_node<script_command>*);
	// Forcefield commands
	bool command_ff(command_node<script_command>*);
	// Field commands
	bool command_field(command_node<script_command>*);
	// Force commands
	bool command_forces(command_node<script_command>*);
	// Image commands
	bool command_image(command_node<script_command>*);
	// MC commands
	bool command_mc(command_node<script_command>*);
	// Minimisation commands
	bool command_minimise(command_node<script_command>*);
	// Model commands
	bool command_model(command_node<script_command>*);
	// Pattern commands
	bool command_pattern(command_node<script_command>*);
	// Pen commands
	bool command_pen(command_node<script_command>*);
	// Preferences commands
	bool command_prefs(command_node<script_command>*);
	// Selection commands
	bool command_select(command_node<script_command>*);
	// Site commands
	bool command_site(command_node<script_command>*);
	// Trajectory commands
	bool command_traj(command_node<script_command>*);
	// Transformation commands
	bool command_transform(command_node<script_command>*);

	/*
	// Initialisation / Execution
	*/
	public:
	// Cache the commands in the file specified
	bool load(const char*);
	// Cache single command
	bool cache_command();
	// Cache line of semicolon-separated commands
	bool cache_line(const char*);
	// Run the cached commands in the script
	void run();

	/*
	// Variables
	*/
	private:
	// Variable list
	variable_list variables;
};

#endif
