/*
	*** Flow control functions
	*** src/command/flow.cpp
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
#include "model/model.h"
#include "base/messenger.h"
#include "base/aten.h"
#include "classes/pattern.h"
#include <fstream>

// Root node (no action)
int CommandData::function_CA_ROOTNODE(Command *&c, Bundle &obj)
{
	return CR_SUCCESS;
}

// Else statement
int CommandData::function_CA_ELSE(Command *&c, Bundle &obj)
{
	c = c->branchCommands();
	return CR_SUCCESSNOMOVE;
}

// Elseif statement
int CommandData::function_CA_ELSEIF(Command *&c, Bundle &obj)
{
	if (c->ifEvaluate()) c = c->branchCommands();
	else c = c->next;
	return CR_SUCCESSNOMOVE;
}

int CommandData::function_CA_END(Command *&c, Bundle &obj)
{
	// This should never be called....
	return CR_SUCCESS;
}

// Loop over atoms
int CommandData::function_CA_FOR(Command *&c, Bundle &obj)
{
	// Grab variable list from command's parent list
	VariableList &vars = c->parent()->variables;
	bool status = TRUE;
	if (c->isLoopActive())
	{
		// Do loop iteration.
		// Increase count and iteration variables
		c->arg(0)->increase(1);
		c->increaseIterations();
		// Set new variables from loop variable and check for termination
		switch (c->argt(0))
		{
			case (Variable::IntegerVariable):
				// If third argument (loop limit) was provided, check against it. Otherwise. continue loop forever unless we're reading from a file where we check for end of file.
				if (c->hasArg(2))
				{
					if (c->argi(0) > c->argi(2)) status = FALSE;
				}
				else if (c->parent()->inputFile() != NULL)
				{
					// Check for end of file...
					if (c->parent()->inputFile()->peek() == -1)
					{
						msg.print(Messenger::Verbose,"Infinite 'for' reached end of file.\n");
						status = FALSE;
					}
				}
				break;
			case (Variable::AtomVariable):
				// If only one argument, check for NULL. If two, check for last atom in pattern.
				// If three, check for last atom in molecule
				if (c->hasArg(2))
				{
					if (c->loopIterations() > c->argp(1)->nAtoms()) status = FALSE;
				}
				else if (c->hasArg(1))
				{
					if (c->loopIterations() > c->argp(1)->totalAtoms()) status = FALSE;
				}
				else if (c->arga(0) == NULL) status = FALSE;
				c->parent()->setAtomVariables(c->arg(0)->name(), c->arga(0));
				break;
			case (Variable::PatternVariable):
				if (c->argp(0) == NULL) status = FALSE;
				c->parent()->setPatternVariables(c->arg(0)->name(), c->argp(0));
				break;
			case (Variable::BondVariable):
			case (Variable::AngleVariable):
			case (Variable::TorsionVariable):
				if (c->argpb(0) == NULL) status = FALSE;
				c->parent()->setPatternBoundVariables(c->arg(0)->name(), c->argpb(0));
				break;
			case (Variable::AtomtypeVariable):
				if (c->argffa(0) == NULL) status = FALSE;
				c->parent()->setAtomtypeVariables(c->arg(0)->name(), c->argffa(0));
				break;
			default:
				printf("Don't know how to set iterate loop with variable of type '%s'.\n", Variable::variableType(c->argt(0)));
				return CR_FAIL;
		}
		if (status == TRUE) c = c->branchCommands();
		else
		{
			c->setLoopActive(FALSE);
			c = c->next;
		}
	}
	else
	{
		// Initialise loop variable in arg(0), depending on its type
		//msg.print(Messenger::Verbose,"Initialising loop : count variable is '%s', type = '%s'\n", countvar->name(), text_from_VT(counttype));
		switch (c->argt(0))
		{
			// Integer loop: 1 arg  - loop from 1 until end of file or termination
			//		 3 args - loop from arg 2 (int) to arg 3 (int)
			case (Variable::IntegerVariable):
				if (!c->hasArg(2))
				{
					c->arg(0)->set(1);
					// Check for end of file...
					if (c->parent()->inputFile() != NULL)
						if (c->parent()->inputFile()->peek() == -1)
						{
							msg.print(Messenger::Verbose,"Command 'for' reached end of file.\n");
							status = FALSE;
						}
				}
				else
				{
					c->arg(0)->set(c->argi(1));
					if (c->argi(1) > c->argi(2)) status = FALSE;
				}
				break;
			// Atom loop:	1 arg  - loop over all atoms in model
			//		2 args - loop over all atoms in arg 2 (pattern)
			//		3 args - loop over atoms in molecule arg 3 in pattern arg 2
			case (Variable::AtomVariable):
				if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
				// If no second variable is given, loop over all atoms
				if (c->hasArg(1))
				{
					// Second argument determines pattern
					Pattern *p;
					if (c->argt(1) == Variable::PatternVariable) p = c->argp(1);
					else if (c->argt(1) == Variable::IntegerVariable) p = obj.m->pattern(c->argi(1));
					else
					{
						msg.print( "Atom loop argument 2 must be of Pattern or Integer type.\n");
						return CR_FAIL;
					}
					// Must have a valid pattern pointer here
					if (p == NULL)
					{
						msg.print( "Atom loop was not given a valid pattern.\n");
						return CR_FAIL;
					}
					// Check on third argument - if provided, must be an int
					if (c->hasArg(2))
					{
						if (c->argt(2) == Variable::IntegerVariable)
						{
							int i = c->argi(2);
							// Check molecule range
							if ((i < 1) || (i > p->nMols()))
							{
								msg.print( "Atom loop pattern molecule is out of range.\n");
								return CR_FAIL;
							}
							int m = p->startAtom();
							m += (i-1) * p->nAtoms();
							c->arg(0)->set(obj.m->atom(m));
						}
						else
						{
							msg.print( "Atom loop argument 3 must be of Integer type.\n.");
							return CR_FAIL;
						}
					}
					else c->arg(0)->set(p->firstAtom());
				}
				else c->arg(0)->set(obj.m->atoms());
				if (c->arga(0) == NULL) status = FALSE;
				// Set secondary variables from atom loop variable
				c->parent()->setAtomVariables(c->arg(0)->name(), c->arga(0));
				break;
			// Pattern loop	 1 arg  - loop over patterns in model
			case (Variable::PatternVariable):
				if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
				if (c->argt(0) == Variable::PatternVariable) c->arg(0)->set(obj.m->patterns());
				else
				{
					msg.print( "Pattern loop variable must be of Pattern type.\n");
					return CR_FAIL;
				}
				if (c->argp(0) == NULL) status = FALSE;
				// Set pattern variables from the pattern pointer
				c->parent()->setPatternVariables(c->arg(0)->name(), c->argp(0));
				break;
			// Loop over forcefield bond terms of pattern
			case (Variable::BondVariable):
				if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
				if (c->argt(1) != Variable::PatternVariable)
				{
					msg.print( "Bond loop must be given a variable of Pattern type.\n");
					return CR_FAIL;
				}
				c->arg(0)->set(c->argp(1)->bonds());
				c->parent()->setPatternBoundVariables(c->arg(0)->name(), (PatternBound*) c->arg(0)->asPointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			// Loop over forcefield angle terms of pattern
			case (Variable::AngleVariable):
				if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
				if (c->argt(1) != Variable::PatternVariable)
				{
					msg.print( "Angle loop must be given a variable of Pattern type.\n");
					return CR_FAIL;
				}
				c->arg(0)->set(c->argp(1)->angles());
				c->parent()->setPatternBoundVariables(c->arg(0)->name(), (PatternBound*) c->arg(0)->asPointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			// Loop over forcefield torsion terms of pattern
			case (Variable::TorsionVariable):
				if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
				if (c->argt(1) != Variable::PatternVariable)
				{
					msg.print( "Torsion loop must be given a variable of Pattern type.\n");
					return CR_FAIL;
				}
				c->arg(0)->set(c->argp(1)->torsions());
				c->parent()->setPatternBoundVariables(c->arg(0)->name(), (PatternBound*) c->arg(0)->asPointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			// Loop over unique ForcefieldAtoms in model
			case (Variable::AtomtypeVariable):
				if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
				if (c->hasArg(1))
				{
					if (c->argt(1) != Variable::AtomtypeVariable)
					{
						msg.print( "Second argument to atomtype loop must be a variable of Atomtype type.\n");
						return CR_FAIL;
					}
					// Start atomtype loop at type given instead of first
					c->arg(0)->set((ForcefieldAtom*) c->arg(1)->asPointer());
				}
				else c->arg(0)->set(obj.m->uniqueTypes());
				c->parent()->setAtomtypeVariables(c->arg(0)->name(), (ForcefieldAtom*) c->arg(0)->asPointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			default:
				printf("Kick Developer - Loops over '%s' are missing.\n", Variable::variableType(c->argt(0)));
				return CR_FAIL;
		}
		// Check loop's starting status
		if (status)
		{
			c->setLoopActive(TRUE);
			c->setLoopIterations(1);
			msg.print(Messenger::Commands,"Loop is initialised and running.\n");
			c = c->branchCommands();
		}
		else
		{
			c->setLoopActive(FALSE);
			c->setLoopIterations(0);
			msg.print(Messenger::Commands,"Loop terminated on initialisation.\n");
			c = c->next;
		}
	}
	return CR_SUCCESSNOMOVE;
}

// Jump to specified node
int CommandData::function_CA_GOTO(Command *&c, Bundle &obj)
{
	c = c->pointer();
	return CR_SUCCESSNOMOVE;
}

// Jump to next node in current list that is *not* an ELSE(IF)
int CommandData::function_CA_GOTONONIF(Command *&c, Bundle &obj)
{
	//printf("Searching for next non-if node...\n");
	c = c->pointer();
	// Skip to node after the source node and begin search.
	c = c->next;
	// If we find an BC_IF node we stop and continue on from there.
	//printf("Skipped back to node %li (%s)\n",fn,text_from_FC(fn->command()));
	CommandAction ca = c->command();
	while ((ca == CA_ELSEIF) || (ca == CA_ELSE))
	{
		c = c->next;
		ca = c->command();
	}
	return CR_SUCCESSNOMOVE;
}

// If statement
int CommandData::function_CA_IF(Command *&c, Bundle &obj)
{
	if (c->ifEvaluate()) c = c->branchCommands();
	else c = c->next;
	return CR_SUCCESSNOMOVE;
}

// Internal TERMINATE command for flow control
int CommandData::function_CA_TERMINATE(Command *&c, Bundle &obj)
{
	return CR_EXIT;
}
