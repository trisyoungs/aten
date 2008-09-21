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

#include "variables/accesspath.h"
#include "command/commandlist.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/spacegroup.h"
#include "base/pattern.h"
#include "classes/grid.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"

// Decrease variable by 1
int CommandData::function_CA_DECREASE(Command *&c, Bundle &obj)
{
	c->arg(0)->step(-1);
	// Set subvariables if necessary
// 	c->parent()->setSubvariables( c->arg(0) ); TGAY
	return CR_SUCCESS;
}

// Increase variable
int CommandData::function_CA_INCREASE(Command *&c, Bundle &obj)
{
	c->arg(0)->step(1);
	// Set subvariables if necessary
// 	c->parent()->setSubvariables( c->arg(0) ); TGAY
	return CR_SUCCESS;
}

// Set non-pointer or non-character variable to value, variable, or expression
int CommandData::function_CA_LET(Command *&c, Bundle &obj)
{
	// Our action depends on the type of the variable being assigned to
	VTypes::DataType type1 = c->argt(0);
	VTypes::DataType type2 = c->argt(2);
	// Integer and real variables may only be set from character, integer, real, or expression variables
	switch (type1)
	{
		case (VTypes::IntegerData):
			if ((type2 > VTypes::RealData) && (type2 < VTypes::ExpressionData))
			{
				msg.print( "Cannot set integer variable '%s' from pointer variable '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
				return CR_FAIL;
			}
			break;
		case (VTypes::RealData):
			if ((type2 > VTypes::RealData) && (type2 < VTypes::ExpressionData))
			{
				msg.print( "Cannot set real variable '%s' from pointer variable '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
				return CR_FAIL;
			}
			break;
		// All other types are pointers - the second argument must also then be a pointer
		default:
			printf("CA_LET doesn't know how to handle variable assignments of type '%s'\n", VTypes::dataType(c->argt(0)));
			return CR_FAIL;
			break;
	}
	// Perform assignment operation requested
	switch (c->argi(1))
	{
		case (AssignOps::Equals):
			c->arg(0)->set( type1 == VTypes::IntegerData ? c->arg(2)->asInteger() : c->arg(2)->asDouble() );
			break;
		case (AssignOps::MinusEquals):
			c->arg(0)->set( type1 == VTypes::IntegerData ? c->arg(0)->asInteger() - c->arg(2)->asInteger() : c->arg(0)->asDouble() - c->arg(2)->asDouble() );
			break;
		case (AssignOps::PlusEquals):
			if (type1 == VTypes::IntegerData) c->arg(0)->set( c->arg(0)->asInteger() + c->arg(2)->asInteger() );
			else c->arg(0)->set( c->arg(0)->asDouble() + c->arg(2)->asDouble() );
			break;
		case (AssignOps::DivideEquals):
			c->arg(0)->set( type1 == VTypes::IntegerData ? c->arg(0)->asInteger() / c->arg(2)->asInteger() : c->arg(0)->asDouble() / c->arg(2)->asDouble() );
			break;
		case (AssignOps::MultiplyEquals):
			c->arg(0)->set( type1 == VTypes::IntegerData ? c->arg(0)->asInteger() * c->arg(2)->asInteger() : c->arg(0)->asDouble() * c->arg(2)->asDouble() );
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
// 		c->arg(0)->copyPointer(c->arg(2)); TGAY
		// Set subvariables
		switch (c->argt(0))
		{
			// TGAY 
// 			case (VTypes::AtomData):
// 				c->parent()->setAtomVariables(c->arg(0)->name(), c->arga(0));
// 				break;
// 			case (VTypes::PatternData):
// 				c->parent()->setPatternVariables(c->arg(0)->name(), c->argp(0));
// 				break;
// 			case (VTypes::ModelData):
// 				c->parent()->setModelVariables(c->arg(0)->name(), c->argm(0));
// 				break;
// 			case (VTypes::GridData):
// 				c->parent()->setModelVariables(c->arg(0)->name(), c->argm(0));
// 				break;
		}
	}
	return CR_SUCCESS;
}

/*
// Variable set / create
*/
/*
// Create model variables with specified basename
bool CommandList::createModelVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"title",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"natoms",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nbonds",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"firstatom",VTypes::AtomData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ngrids",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nframes",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"currentframe",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.type",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.a",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.b",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.c",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.alpha",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.beta",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.gamma",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.ax",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.ay",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.az",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.bx",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.by",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.bz",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.cx",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.cy",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.cz",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.centrex",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.centrey",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.centrez",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.spgrp.id",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.spgrp.name",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"cell.spgrp.setting",VTypes::IntegerData);
	if (v == NULL) return FALSE;

	return TRUE;
}

// Set variables for model with specified prefix
void CommandList::setModelVariables(const char *base, Model *m)
{
	msg.enter("CommandList::setModelVariables[const char*,Model*]");
	if (m != NULL)
	{
		char s[128];
		variables.set(base,"title",m->name());
		variables.set(base,"natoms",m->nAtoms());
		variables.set(base,"nbonds",m->nBonds());
		variables.set(base,"nframes",m->nTrajectoryFrames());
		variables.set(base,"firstatom",m->atoms());
		variables.set(base,"ngrids",m->nGrids());
		variables.set(base,"currentframe",m->trajectoryPosition());
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
		variables.set(base,"cell.spgrp.id", m->spacegroup());
		variables.set(base,"cell.spgrp.name", spacegroups.name(m->spacegroup()));
		variables.set(base,"cell.spgrp.setting", m->spacegroupSetting());
	}
	msg.exit("CommandList::setModelVariables");
}

// Create atom parameter variables
bool CommandList::createAtomVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"symbol",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"mass",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"name",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"z",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"id",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fixed",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"selected",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fftype",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ffequiv",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"q",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"rx",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ry",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"rz",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fx",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fy",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fz",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vx",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vy",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vz",VTypes::RealData);
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
	v = variables.createVariable(base,"name",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nmols",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nmolatoms",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"lastid",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"firstid",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"lastatom",VTypes::AtomData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"firstatom",VTypes::AtomData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"natoms",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nbonds",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nangles",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ntorsions",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ntypes",VTypes::IntegerData);
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

// Create grid parameter variables
bool CommandList::createGridVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"name",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variables for grid
void CommandList::setGridVariables(const char *varname, Grid *g)
{
	msg.enter("CommandList::setGridVariables");
	if (g != NULL)
	{
		variables.set(varname,"name",g->name());
	}
	msg.exit("CommandList::setGridVariables");
}

// Create pattern bound term variables
bool CommandList::createPatternBoundVariables(const char *base)
{
	Variable *v;
	static char parm[24];
	int i;
	v = variables.createVariable(base,"form",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	strcpy(parm,"id_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[3] = 105 + i;
		v = variables.createVariable(base,parm,VTypes::IntegerData);
		if (v == NULL) return FALSE;
	}
	strcpy(parm,"type_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[5] = 105 + i;
		v = variables.createVariable(base,parm,VTypes::CharacterData);
		if (v == NULL) return FALSE;
	}
	strcpy(parm,"param_X");
	for (i = 0; i < MAXFFPARAMDATA; i++)
	{
		parm[6] = 97 + i;
		v = variables.createVariable(base,parm,VTypes::RealData);
		if (v == NULL) return FALSE;
	}
	v = variables.createVariable(base,"escale",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vscale",VTypes::RealData);
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

// Set variables for PatternBound (from simple Bond)
void CommandList::setPatternBoundVariables(const char *varname, Bond *b)
{
	msg.enter("CommandList::setPatternBoundVariables[bond]");
	static char parm[24];
	int i;
	if (b != NULL)
	{
		// Set atom ids involved
		variables.set(varname,"id_i",b->atomI()->id()+1);
		variables.set(varname,"id_j",b->atomJ()->id()+1);
		strcpy(parm,"id_X");
		for (i = 2; i < MAXFFBOUNDTYPES; i++)
		{
			parm[3] = 105 + i;
			variables.set(varname,parm,0);
		}
		// Set default type names
		strcpy(parm,"type_X");
		for (i = 1; i < MAXFFBOUNDTYPES; i++)
		{
			parm[5] = 105 + i;
			variables.set(varname,parm,"none");
		}
		// Set default ForcefieldParams data
		strcpy(parm,"param_X");
		for (int i = 0; i < MAXFFPARAMDATA; i++)
		{
			parm[6] = 97 + i;
			variables.set(varname,parm,0.0);
		}
		// Set bond form (type)
		variables.set(varname,"form",Bond::bondType(b->type()));
	}
	msg.exit("CommandList::setPatternBoundVariables[bond]");
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
		v = variables.createVariable(base,parm,VTypes::RealData);
		if (v == NULL) return FALSE;
	}
	v = variables.createVariable(base,"q",VTypes::RealData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"id",VTypes::IntegerData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"name",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"equiv",VTypes::CharacterData);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"form",VTypes::CharacterData);
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
}*/
