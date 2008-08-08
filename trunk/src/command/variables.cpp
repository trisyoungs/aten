/*
	*** Variable command functions
	*** src/command/variables.cpp
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
#include "base/elements.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"

// Decrease variable by 1
int CommandData::function_CA_DECREASE(Command *&c, Bundle &obj)
{
	c->arg(0)->decrease(1);
	// Set subvariables if necessary
	c->parent()->setSubvariables( c->arg(0) );
	return CR_SUCCESS;
}

// Increase variable
int CommandData::function_CA_INCREASE(Command *&c, Bundle &obj)
{
	c->arg(0)->increase(1);
	// Set subvariables if necessary
	c->parent()->setSubvariables( c->arg(0) );
	return CR_SUCCESS;
}

// Set non-pointer or non-character variable to value, variable, or expression
int CommandData::function_CA_LET(Command *&c, Bundle &obj)
{
	// Our action depends on the type of the variable being assigned to
	Variable::VariableType type1 = c->argt(0);
	Variable::VariableType type2 = c->argt(2);
	// Integer and real variables may only be set from character, integer, real, or expression variables
	switch (type1)
	{
		case (Variable::IntegerVariable):
			if ((type2 > Variable::FloatVariable) && (type2 < Variable::ExpressionVariable))
			{
				msg.print( "Cannot set integer variable '%s' from pointer variable '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
				return CR_FAIL;
			}
			break;
		case (Variable::FloatVariable):
			if ((type2 > Variable::FloatVariable) && (type2 < Variable::ExpressionVariable))
			{
				msg.print( "Cannot set real variable '%s' from pointer variable '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
				return CR_FAIL;
			}
			break;
		// All other types are pointers - the second argument must also then be a pointer
		default:
			printf("CA_LET doesn't know how to handle variable assignments of type '%s'\n", Variable::variableType(c->argt(0)));
			return CR_FAIL;
			break;
	}
	// Perform assignment operation requested
	switch (c->argi(1))
	{
		case (AssignOps::Equals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(2)->asInteger() : c->arg(2)->asDouble() );
			break;
		case (AssignOps::MinusEquals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(0)->asInteger() - c->arg(2)->asInteger() : c->arg(0)->asDouble() - c->arg(2)->asDouble() );
			break;
		case (AssignOps::PlusEquals):
			if (type1 == Variable::IntegerVariable) c->arg(0)->set( c->arg(0)->asInteger() + c->arg(2)->asInteger() );
			else c->arg(0)->set( c->arg(0)->asDouble() + c->arg(2)->asDouble() );
			break;
		case (AssignOps::DivideEquals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(0)->asInteger() / c->arg(2)->asInteger() : c->arg(0)->asDouble() / c->arg(2)->asDouble() );
			break;
		case (AssignOps::MultiplyEquals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(0)->asInteger() * c->arg(2)->asInteger() : c->arg(0)->asDouble() * c->arg(2)->asDouble() );
			break;
	}
	return CR_SUCCESS;
}

// Assign string/variable to character variable only
int CommandData::function_CA_LETCHAR(Command *&c, Bundle &obj)
{
	// Our action depends on the operator provided which we cast from the second argument
	switch (c->argi(1))
	{
		// Straight assigment
		case (AssignOps::Equals):
			c->arg(0)->set(c->argc(2));
			break;
		// Concatenation
		case (AssignOps::PlusEquals):
			c->arg(0)->set(c->argc(2));
			break;
		default:
			printf("Operator given to CA_LETCHAR (%i) that we don't know how to handle.\n", c->argi(1));
			break;
	}
	return CR_SUCCESS;
}

// Assign pointer variable to another pointer variable
int CommandData::function_CA_LETPTR(Command *&c, Bundle &obj)
{
	if (c->argt(0) != c->argt(2))
	{
		msg.print( "Incompatible pointer types for variable assignment of contents of '%s' to '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
		return CR_FAIL;
	}
	else
	{
		c->arg(0)->copyPointer(c->arg(2));
		// Set subvariables
		switch (c->argt(0))
		{
			case (Variable::AtomVariable):
				c->parent()->setAtomVariables(c->arg(0)->name(), c->arga(0));
				break;
			case (Variable::PatternVariable):
				c->parent()->setPatternVariables(c->arg(0)->name(), c->argp(0));
				break;
			case (Variable::ModelVariable):
				c->parent()->setModelVariables(c->arg(0)->name(), c->argm(0));
				break;

		}
	}
	return CR_SUCCESS;
}

/*
// Variable set / create
*/

// Create model variables with specified basename
bool CommandList::createModelVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"title",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"natoms",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"firstatom",Variable::AtomVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nframes",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"currentframe",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.type",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.a",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.b",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.c",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.alpha",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.beta",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.gamma",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.ax",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.ay",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.az",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.bx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.by",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.bz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.cx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.cy",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.cz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.centrex",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.centrey",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.centrez",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variables for model with specified prefix
void CommandList::setModelVariables(const char *base, Model *m)
{
	msg.enter("CommandList::setModelVariables[const char*,Model*]");
	if (m != NULL)
	{
		variables.set(base,"title",m->name());
		variables.set(base,"natoms",m->nAtoms());
		variables.set(base,"nframes",m->totalFrames());
		variables.set(base,"firstatom",m->atoms());
		variables.set(base,"currentframe",m->framePosition());
		Cell *c = m->cell();
		Mat3<double> mat;
		Vec3<double> vec;
		variables.set(base,"cell.type",lowerCase(Cell::cellType(c->type())));
		mat = c->axes();
		variables.set(base,"cell.ax",mat.rows[0].x);
		variables.set(base,"cell.ay",mat.rows[0].y);
		variables.set(base,"cell.az",mat.rows[0].z);
		variables.set(base,"cell.bx",mat.rows[1].x);
		variables.set(base,"cell.by",mat.rows[1].y);
		variables.set(base,"cell.bz",mat.rows[1].z);
		variables.set(base,"cell.cx",mat.rows[2].x);
		variables.set(base,"cell.cy",mat.rows[2].y);
		variables.set(base,"cell.cz",mat.rows[2].z);
		vec = c->lengths();
		variables.set(base,"cell.a",vec.x);
		variables.set(base,"cell.b",vec.y);
		variables.set(base,"cell.c",vec.z);
		vec = c->angles();
		variables.set(base,"cell.alpha",vec.x);
		variables.set(base,"cell.beta",vec.y);
		variables.set(base,"cell.gamma",vec.z);
		vec = c->centre();
		variables.set(base,"cell.centrex",vec.x);
		variables.set(base,"cell.centrey",vec.y);
		variables.set(base,"cell.centrez",vec.z);
		// If this model has a trajectory frame, set those variables as well
		Model *frame = m->renderSource();
		if (frame != m)
		{
			char s[128];
			strcpy(s,base);
			if (s[0] != '\0') strcat(s,".");
			strcat(s,"frame");
			setModelVariables(s, frame);
		}
	}
	msg.exit("CommandList::setModelVariables");
}

// Create atom parameter variables
bool CommandList::createAtomVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"symbol",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"mass",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"name",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"z",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"id",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fixed",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"selected",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fftype",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ffequiv",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"q",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"rx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ry",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"rz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fy",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vy",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variable values for atom
void CommandList::setAtomVariables(const char *varname, Atom *i)
{
	msg.enter("CommandList::setAtomVariables");
	Vec3<double> v;
	if (i != NULL)
	{
		// Element and ff type
		variables.set(varname,"symbol",elements.symbol(i));
		variables.set(varname,"mass",elements.atomicMass(i));
		variables.set(varname,"name",elements.name(i));
		variables.set(varname,"z",i->element());
		variables.set(varname,"id",i->id()+1);
		variables.set(varname,"fixed",i->hasFixedPosition());
		variables.set(varname,"selected",i->isSelected());
		ForcefieldAtom *ffa = i->type();
		variables.set(varname,"fftype",(ffa == NULL ? elements.symbol(i) : ffa->name()));
		variables.set(varname,"ffequiv",(ffa == NULL ? elements.symbol(i) : ffa->equivalent()));
		v = i->r();
		variables.set(varname,"rx",v.x);
		variables.set(varname,"ry",v.y);
		variables.set(varname,"rz",v.z);
		v = i->f();
		variables.set(varname,"fx",v.x);
		variables.set(varname,"fy",v.y);
		variables.set(varname,"fz",v.z);
		v = i->v();
		variables.set(varname,"vx",v.x);
		variables.set(varname,"vy",v.y);
		variables.set(varname,"vz",v.z);
		variables.set(varname,"q",i->charge());
	}
	msg.exit("CommandList::setAtomVariables");
}

// Create pattern parameter variables
bool CommandList::createPatternVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"name",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nmols",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nmolatoms",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"lastid",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"firstid",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"lastatom",Variable::AtomVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"firstatom",Variable::AtomVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"natoms",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nbonds",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nangles",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ntorsions",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ntypes",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variables for pattern
void CommandList::setPatternVariables(const char *varname, Pattern *p)
{
	msg.enter("CommandList::setPatternVariables");
	if (p != NULL)
	{
		variables.set(varname,"name",p->name());
		variables.set(varname,"nmols",p->nMolecules());
		variables.set(varname,"nmolatoms",p->nAtoms());
		variables.set(varname,"natoms",p->totalAtoms());
		variables.set(varname,"firstid",p->startAtom() + 1);
		variables.set(varname,"lastid",p->startAtom() + p->totalAtoms() - 1);
		variables.set(varname,"lastatom",p->lastAtom());
		variables.set(varname,"firstatom",p->firstAtom());
		variables.set(varname,"nbonds",p->nBonds());
		variables.set(varname,"nangles",p->nAngles());
		variables.set(varname,"ntorsions",p->nTorsions());
	}
	msg.exit("CommandList::setPatternVariables");
}

// Create pattern bound term variables
bool CommandList::createPatternBoundVariables(const char *base)
{
	Variable *v;
	static char parm[24];
	int i;
	v = variables.createVariable(base,"form",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	strcpy(parm,"id_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[3] = 105 + i;
		v = variables.createVariable(base,parm,Variable::IntegerVariable);
		if (v == NULL) return FALSE;
	}
	strcpy(parm,"type_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[5] = 105 + i;
		v = variables.createVariable(base,parm,Variable::CharacterVariable);
		if (v == NULL) return FALSE;
	}
	strcpy(parm,"param_X");
	for (i = 0; i < MAXFFPARAMDATA; i++)
	{
		parm[6] = 97 + i;
		v = variables.createVariable(base,parm,Variable::FloatVariable);
		if (v == NULL) return FALSE;
	}
	v = variables.createVariable(base,"escale",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vscale",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variables for PatternBound
void CommandList::setPatternBoundVariables(const char *varname, PatternBound *pb)
{
	msg.enter("CommandList::setPatternBoundVariables");
	static ForcefieldParams ffp;
	static ForcefieldBound *ffb;
	static char parm[24];
	int i;
	if (pb != NULL)
	{
		// Grab ForcefieldBound pointer from pattern bound structure
		ffb = pb->data();
		// Set atom ids involved
		strcpy(parm,"id_X");
		for (i = 0; i < MAXFFBOUNDTYPES; i++)
		{
			parm[3] = 105 + i;
			variables.set(varname,parm,pb->atomId(i)+1);
		}
		// Set type names involved
		strcpy(parm,"type_X");
		for (i = 0; i < MAXFFBOUNDTYPES; i++)
		{
			parm[5] = 105 + i;
			variables.set(varname,parm,ffb->typeName(i));
		}
		// Grab ForcefieldParams data
		ffp = ffb->params();
		strcpy(parm,"param_X");
		for (int i = 0; i < MAXFFPARAMDATA; i++)
		{
			parm[6] = 97 + i;
			variables.set(varname,parm,ffp.data[i]);
		}
		// Set functional form and any additional variables
		switch (ffb->type())
		{
			case (ForcefieldBound::BondInteraction):
				variables.set(varname,"form", BondFunctions::BondFunctions[ffb->bondStyle()].keyword);
				break;
			case (ForcefieldBound::AngleInteraction):
				variables.set(varname,"form", AngleFunctions::AngleFunctions[ffb->angleStyle()].keyword);
				break;
			case (ForcefieldBound::TorsionInteraction):
				variables.set(varname,"form", TorsionFunctions::TorsionFunctions[ffb->torsionStyle()].keyword);
				variables.set(varname,"escale",ffp.data[TF_ESCALE]);
				variables.set(varname,"vscale",ffp.data[TF_VSCALE]);
				break;
			default:	
				printf("CommandList::setPatternBoundVariables <<<< Functional form not defined >>>>\n");
				break;
		}
		
	}
	msg.exit("CommandList::setPatternBoundVariables");
}

// Create atomtype parameter variables
bool CommandList::createAtomtypeVariables(const char *base)
{
	static char parm[24];
	int i;
	Variable *v;
	strcpy(parm,"param_X");
	for (i = 0; i < MAXFFPARAMDATA; i++)
	{
		parm[6] = 97 + i;
		v = variables.createVariable(base,parm,Variable::FloatVariable);
		if (v == NULL) return FALSE;
	}
	v = variables.createVariable(base,"q",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"id",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"name",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"equiv",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"form",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}


// Set variables for pattern
void CommandList::setAtomtypeVariables(const char *varname, ForcefieldAtom *ffa)
{
	msg.enter("CommandList::setAtomtypeVariables");
	static char parm[24];
	int i;
	ForcefieldParams ffp;
	if (ffa != NULL)
	{
		ffp = ffa->params();
		strcpy(parm,"param_X");
		for (i = 0; i < MAXFFPARAMDATA; i++)
		{
			parm[6] = 97 + i;
			variables.set(varname,parm,ffp.data[i]);
		}
		variables.set(varname,"q",ffa->charge());
		variables.set(varname,"id",ffa->typeId());
		variables.set(varname,"name",ffa->name());
		variables.set(varname,"equiv",ffa->equivalent());
		variables.set(varname,"form",VdwFunctions::VdwFunctions[ffa->vdwForm()].keyword);
	}
	msg.exit("CommandList::setAtomtypeVariables");
}
