/*
	*** Disorder Commands
	*** src/command/disorder.cpp
	Copyright T. Youngs 2007-2015

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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "methods/mc.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Performs MC insertion ('disorder <scheme>')
bool Commands::function_Disorder(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	// Parse the first option so we can get at any options
	LineParser parser;
	parser.getArgsDelim(0, c->argc(0));
	
	// First argument should always be the scheme name
	PartitioningScheme* scheme = aten_.findPartitioningScheme(parser.argc(0));
	if (scheme == NULL) return FALSE;
	
	// Loop over remaining arguments (widget/global variable assignments)
	for (int n = 1; n < parser.nArgs(); ++n) if (!scheme->setVariable(beforeStr(parser.argc(n),"="), afterStr(parser.argc(n),"="))) return FALSE;
	
	Messenger::print("Performing disordered build for model '%s'\n", obj.m->name());
	rv.reset();
	bool result = mc.disorder(aten_.models(), obj.m, scheme, c->hasArg(1) ? c->argb(1) : TRUE);
	return result;
}

// Print current component list ('listcomponents')
bool Commands::function_ListComponents(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Messenger::print("Current component specification:\n");
	Vec3<double> v1, v2;
	Dnchar text;
	Messenger::print("Model           Policy     Partition  Population   Density\n");
	for (Model* m = aten_.models(); m != NULL; m = m->next)
	{
		if (m->cell()->type() != UnitCell::NoCell) continue;
		Messenger::print("%-15s %-10s     %i        %5i    %8.4f\n", m->name(), Model::insertionPolicy(m->componentInsertionPolicy()), m->componentPartition()+1, m->componentPopulation(), m->componentDensity());
	}
	rv.reset();
	return TRUE;
}

// Setup current model as component for disorder builder
bool Commands::function_SetupComponent(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Model::InsertionPolicy policy = Model::insertionPolicy(c->argc(0), TRUE);
	if (policy == Model::nInsertionPolicies) return FALSE;
	obj.m->setComponentInsertionPolicy(policy);
	if (c->hasArg(1)) obj.m->setComponentPartition(c->argi(1)-1);
	if (c->hasArg(2)) obj.m->setComponentPopulation(c->argi(2));
	if (c->hasArg(3)) obj.m->setComponentDensity(c->argd(3));
	if (c->hasArg(4)) obj.m->setComponentRotatable(c->argb(4));
	return TRUE;
}

