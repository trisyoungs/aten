/*
	*** Glyph command functions
	*** src/command/glyph.cpp
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
#include "base/glyph.h"

// Auto-add ellipsoids to current atom selection
int CommandData::function_CA_AUTOELLIPSOIDS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.m->addEllipsoidGlyphs();
	return CR_SUCCESS;
}

// Auto-add polyhedra to current atom selection
int CommandData::function_CA_AUTOPOLYHEDRA(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	bool centresonly = TRUE, linkatoms = TRUE;
	Dnchar keywd, value;
	double rcut = 4.0;
	for (int i=0; i < c->nArgs(); i++)
	{
		// Split argument into keyword and value
		keywd = beforeChar(c->argc(i), '=');
		value = afterChar(c->argc(i), '=');
		if (keywd == "fragments") centresonly = FALSE;
		else if (keywd == "centres") centresonly = TRUE;
		else if (keywd == "nolink") linkatoms = FALSE;
		else if (keywd == "rcut") rcut = atof(value.get());
		else msg.print("Unknown option '%s' given to 'autopolyhedra'.\n", keywd.get());
	}
	// Add the polyhedra
	obj.m->addPolyhedraGlyphs(centresonly, linkatoms, rcut);
	return CR_SUCCESS;
}

// Associate atom with current glyph
int CommandData::function_CA_GLYPHATOMF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		msg.print("Data index given to 'setglyphatom' (%i) is out of range.\n", d);
		return CR_FAIL;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	Atom *target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argt(1) == Variable::AtomVariable) target = c->arga(1);
		else target = obj.rs->atom(c->argi(1) - 1);
	}
	// Finally, check pointer currently in target and store it
	obj.gl->setAtom(d, obj.rs->atomIndex(target), GlyphData::ForceData);
	if (target == NULL) msg.print("Warning - NULL atom stored in glyph data %i.\n",d);
	return CR_SUCCESS;
}

// Associate atom with current glyph
int CommandData::function_CA_GLYPHATOMR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		msg.print("Data index given to 'setglyphatom' (%i) is out of range.\n", d);
		return CR_FAIL;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	Atom *target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argt(1) == Variable::AtomVariable) target = c->arga(1);
		else target = obj.rs->atom(c->argi(1) - 1);
	}
	// Finally, check pointer currently in target and store it
	obj.gl->setAtom(d, obj.rs->atomIndex(target), GlyphData::PositionData);
	if (target == NULL) msg.print("Warning - NULL atom stored in glyph data %i.\n",d);
	return CR_SUCCESS;
}

// Associate atom with current glyph
int CommandData::function_CA_GLYPHATOMV(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		msg.print("Data index given to 'setglyphatom' (%i) is out of range.\n", d);
		return CR_FAIL;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	Atom *target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argt(1) == Variable::AtomVariable) obj.gl->setAtom(d, obj.rs->atomIndex(c->arga(1)), GlyphData::VelocityData);
		else obj.gl->setAtom(d, c->argi(1)-1, GlyphData::VelocityData); 
	}
	return CR_SUCCESS;
}

// Associate atoms with current glyph
int CommandData::function_CA_GLYPHATOMSF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argt(d) == Variable::AtomVariable) obj.gl->setAtom(d, obj.rs->atomIndex(c->arga(d)), GlyphData::ForceData);
			else obj.gl->setAtom(d, c->argi(d)-1, GlyphData::ForceData); 
		}
		else break;
	}
	return CR_SUCCESS;
}

// Associate atoms with current glyph
int CommandData::function_CA_GLYPHATOMSR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argt(d) == Variable::AtomVariable) obj.gl->setAtom(d, obj.rs->atomIndex(c->arga(d)), GlyphData::PositionData);
			else obj.gl->setAtom(d, c->argi(d)-1, GlyphData::PositionData); 
		}
		else break;
	}
	return CR_SUCCESS;
}

// Associate atoms with current glyph
int CommandData::function_CA_GLYPHATOMSV(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argt(d) == Variable::AtomVariable) obj.gl->setAtom(d, obj.rs->atomIndex(c->arga(d)), GlyphData::VelocityData);
			else obj.gl->setAtom(d, c->argi(d)-1, GlyphData::VelocityData); 
		}
		else break;
	}
	return CR_SUCCESS;
}

// Store colour data in current glyph
int CommandData::function_CA_GLYPHCOLOUR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	obj.gl->setColour(d, c->argf(1), c->argf(2), c->argf(3), c->hasArg(4) ? c->argf(4) : 1.0f);
	return CR_SUCCESS;
}

// Store vector data in current glyph
int CommandData::function_CA_GLYPHDATA(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	obj.gl->setVector(d, c->argd(1), c->hasArg(2) ? c->argd(2) : 0.0, c->hasArg(3) ? c->argd(3) : 0.0);
	return CR_SUCCESS;
}

// Set 'solid' property of current glyph
int CommandData::function_CA_GLYPHSOLID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	obj.gl->setSolid(c->argb(0));
	return CR_SUCCESS;
}

// Set text property of current glyph
int CommandData::function_CA_GLYPHTEXT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return CR_FAIL;
	obj.gl->setText(c->argc(0));
	return CR_SUCCESS;
}

// Add glyph to current model
int CommandData::function_CA_NEWGLYPH(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	// Get glyph style
	Glyph::GlyphType gt = Glyph::glyphType(c->argc(0));
	if (gt == Glyph::nGlyphTypes)
	{
		msg.print("Unrecognised glyph style '%s'.\n", c->argc(0));
		return CR_FAIL;
	}
	obj.gl = obj.rs->addGlyph(gt);
	// Parse extra options
	Dnchar keywd, value;
	for (int i=1; i < c->nArgs(); i++)
	{
		// Split argument into keyword and value
		keywd = beforeChar(c->argc(i), '=');
		value = afterChar(c->argc(i), '=');
		if (keywd == "text") obj.gl->setText(value.get());
		else if (keywd == "solid") obj.gl->setSolid(TRUE);
		else if (keywd == "wire") obj.gl->setSolid(FALSE);
		else if (keywd == "linewidth") obj.gl->setLineWidth((GLfloat) atof(value.get()));
		else msg.print("Unknown option '%s' given to 'newglyph'.\n", keywd.get());
	}
	return CR_SUCCESS;
}
