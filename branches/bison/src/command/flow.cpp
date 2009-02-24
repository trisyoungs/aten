/*
	*** Flow control functions
	*** src/command/flow.cpp
	Copyright T. Youngs 2007-2009

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

#include "variables/accesspath.h"
#include "command/commandlist.h"
#include "model/model.h"
#include "base/pattern.h"
#include "main/aten.h"

// Root node (no action)
int Command::function_CA_ROOTNODE(CommandNode *&c, Bundle &obj)
{
	return Command::Success;
}

// Break out of current loop
int Command::function_CA_BREAK(CommandNode *&c, Bundle &obj)
{
	// Set next command to be the node after the root FOR command
	c = c->pointer();
	c->setLoopActive(FALSE);
	c = c->next;
	return Command::SuccessNoMove;
}

// Cycle current loop
int Command::function_CA_CONTINUE(CommandNode *&c, Bundle &obj)
{
	// Set next command to be the root loop node
	c = c->pointer();
	return Command::SuccessNoMove;
}

// Else statement
int Command::function_CA_ELSE(CommandNode *&c, Bundle &obj)
{
	c = c->branchCommands();
	return Command::SuccessNoMove;
}

// Elseif statement
int Command::function_CA_ELSEIF(CommandNode *&c, Bundle &obj)
{
	if (c->ifEvaluate()) c = c->branchCommands();
	else c = c->next;
	return Command::SuccessNoMove;
}

int Command::function_CA_END(CommandNode *&c, Bundle &obj)
{
	// This should never be called....
	return Command::Success;
}

// Loop over atoms
int Command::function_CA_FOR(CommandNode *&c, Bundle &obj)
{
	// Grab variable list from command's parent list
	bool status = TRUE;
	int n;
	if (c->isLoopActive())
	{
		// Do loop iteration.
		// Increase count and iteration variables
		c->arg(0)->step(1);
		c->increaseIterations();
		// Set new variables from loop variable and check for termination
		switch (c->argt(0))
		{
			case (VTypes::IntegerData):
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
			case (VTypes::AtomData):
				// If only one argument, check for NULL. If two, check for last atom in pattern.
				// If three, check for last atom in molecule specified
				if (c->hasArg(2))
				{
					n = c->argi(2) * ((Pattern*) c->argp(1, VTypes::PatternData))->nAtoms();
					if (c->loopIterations() > n) status = FALSE;
				}
				else if (c->hasArg(1))
				{
					n = ((Pattern*) c->argp(1, VTypes::PatternData))->totalAtoms();
					if (c->loopIterations() > n) status = FALSE;
				}
				else if (c->argp(0, VTypes::AtomData) == NULL) status = FALSE;
				break;
			case (VTypes::PatternData):
				if (c->argp(0, VTypes::PatternData) == NULL) status = FALSE;
				break;
			case (VTypes::ForcefieldAtomData):
				if (c->argp(0, VTypes::ForcefieldAtomData) == NULL) status = FALSE;
				break;
			case (VTypes::ModelData):
				if (c->argp(0, VTypes::ModelData) == NULL) status = FALSE;
				break;
			default:
				printf("Don't know how to set iterate loop with variable of type '%s'.\n", VTypes::dataType(c->argt(0)));
				return Command::Fail;
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
			case (VTypes::IntegerData):
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
			case (VTypes::AtomData):
				if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
				// If no second variable is given, loop over all atoms
				if (c->hasArg(1))
				{
					// Second argument determines pattern
					Pattern *p;
					if (c->argt(1) == VTypes::PatternData) p = (Pattern*) c->argp(1, VTypes::PatternData);
					else if (c->argt(1) == VTypes::IntegerData) p = obj.rs->pattern(c->argi(1));
					else
					{
						msg.print( "Atom loop argument 2 must be of Pattern or Integer type.\n");
						return Command::Fail;
					}
					// Must have a valid pattern pointer here
					if (p == NULL)
					{
						msg.print( "Atom loop was not given a valid pattern.\n");
						return Command::Fail;
					}
					// Check on third argument - if provided, must be an int
					if (c->hasArg(2))
					{
						if (c->argt(2) == VTypes::IntegerData)
						{
							int i = c->argi(2);
							// Check molecule range
							if ((i < 1) || (i > p->nMolecules()))
							{
								msg.print( "Atom loop pattern molecule is out of range.\n");
								return Command::Fail;
							}
							int m = p->startAtom();
							m += (i-1) * p->nAtoms();
							c->arg(0)->set(obj.rs->atom(m), VTypes::AtomData);
						}
						else
						{
							msg.print( "Atom loop argument 3 must be of Integer type.\n.");
							return Command::Fail;
						}
					}
					else c->arg(0)->set(p->firstAtom(), VTypes::AtomData);
				}
				else c->arg(0)->set(obj.rs->atoms(), VTypes::AtomData);
				if (c->argp(0, VTypes::AtomData) == NULL) status = FALSE;
				break;
			// Pattern loop	 1 arg  - loop over patterns in model
			case (VTypes::PatternData):
				if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
				if (c->argt(0) == VTypes::PatternData) c->arg(0)->set(obj.rs->patterns(), VTypes::PatternData);
				else
				{
					msg.print( "Pattern loop variable must be of Pattern type.\n");
					return Command::Fail;
				}
				if (c->argp(0, VTypes::PatternData) == NULL) status = FALSE;
				break;
			// Loop over unique ForcefieldAtoms in model
			case (VTypes::ForcefieldAtomData):
				if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
				if (c->hasArg(1))
				{
					if (c->argt(1) != VTypes::ForcefieldAtomData)
					{
						msg.print( "Second argument to atomtype loop must be a variable of Atomtype type.\n");
						return Command::Fail;
					}
					// Start atomtype loop at type given instead of first
					c->arg(0)->set(c->arg(1)->asPointer(VTypes::ForcefieldAtomData), VTypes::ForcefieldAtomData);
				}
				else c->arg(0)->set(obj.rs->uniqueTypes(), VTypes::ForcefieldAtomData);
				if (c->argp(0, VTypes::ForcefieldAtomData) == NULL) status = FALSE;
				break;
			// Model loop
			case (VTypes::ModelData):
				if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
				if (c->argt(0) == VTypes::ModelData) c->arg(0)->set(aten.models(), VTypes::ModelData);
				else
				{
					msg.print( "Modeltern loop variable must be of Model type.\n");
					return Command::Fail;
				}
				if (c->argp(0, VTypes::ModelData) == NULL) status = FALSE;
				break;
			default:
				printf("Kick Developer - Loops over '%s' are missing.\n", VTypes::dataType(c->argt(0)));
				return Command::Fail;
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
	return Command::SuccessNoMove;
}

// Jump to specified node
int Command::function_CA_GOTO(CommandNode *&c, Bundle &obj)
{
	c = c->pointer();
	return Command::SuccessNoMove;
}

// Jump to next node in current list that is *not* an ELSE(IF)
int Command::function_CA_GOTONONIF(CommandNode *&c, Bundle &obj)
{
	//printf("Searching for next non-if node...\n");
	c = c->pointer();
	// Skip to node after the source node and begin search.
	c = c->next;
	// If we find an BC_IF node we stop and continue on from there.
	//printf("Skipped back to node %li (%s)\n",fn,text_from_FC(fn->command()));
	Command::Function cf = c->function();
	while ((cf == CA_ELSEIF) || (cf == CA_ELSE))
	{
		c = c->next;
		cf = c->function();
	}
	return Command::SuccessNoMove;
}

// If statement
int Command::function_CA_IF(CommandNode *&c, Bundle &obj)
{
	if (c->ifEvaluate()) c = c->branchCommands();
	else c = c->next;
	return Command::SuccessNoMove;
}

// Internal TERMINATE command for flow control
int Command::function_CA_TERMINATE(CommandNode *&c, Bundle &obj)
{
	return Command::Exit;
}
