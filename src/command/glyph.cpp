/*
	*** Glyph Commands
	*** src/command/glyph.cpp
	Copyright T. Youngs 2007-2010

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
#include "model/model.h"
#include "base/glyph.h"
#include "base/sysfunc.h"

// Auto-add ellipsoids to current atom selection
bool Command::function_AutoEllipsoids(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->addEllipsoidGlyphs();
	rv.reset();
	return TRUE;
}

// Auto-add polyhedra to current atom selection
bool Command::function_AutoPolyhedra(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
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
	rv.reset();
	return TRUE;
}

// Associate atom with current glyph
bool Command::function_GlyphAtomF(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		msg.print("Data index given to 'glyphatomf' (%i) is out of range.\n", d);
		return FALSE;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	Atom *target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argType(1) == VTypes::AtomData) target = (Atom*) c->argp(1, VTypes::AtomData);
		else target = obj.rs->atom(c->argi(1) - 1);
	}
	// Finally, check pointer currently in target and store it
	obj.gl->data(d)->setAtom(target, GlyphData::ForceData);
	if (target == NULL) msg.print("Warning - NULL atom stored in glyph data %i.\n",d);
	rv.reset();
	return TRUE;
}

// Associate atom with current glyph
bool Command::function_GlyphAtomR(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		msg.print("Data index given to 'glyphatomr' (%i) is out of range.\n", d);
		return FALSE;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	Atom *target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argType(1) == VTypes::AtomData) target = (Atom*) c->argp(1, VTypes::AtomData);
		else target = obj.rs->atom(c->argi(1) - 1);
	}
	// Finally, check pointer currently in target and store it
	obj.gl->data(d)->setAtom(target, GlyphData::PositionData);
	if (target == NULL) msg.print("Warning - NULL atom stored in glyph data %i.\n",d);
	rv.reset();
	return TRUE;
}

// Associate atom with current glyph
bool Command::function_GlyphAtomV(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		msg.print("Data index given to 'glyphatomv' (%i) is out of range.\n", d);
		return FALSE;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	Atom *target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argType(1) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(1, VTypes::AtomData), GlyphData::VelocityData);
		else target = obj.rs->atom(c->argi(1) - 1);
	}
	obj.gl->data(d)->setAtom(target, GlyphData::VelocityData); 
	if (target == NULL) msg.print("Warning - NULL atom stored in glyph data %i.\n",d);
	rv.reset();
	return TRUE;
}

// Associate atoms with current glyph
bool Command::function_GlyphAtomsF(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argType(d) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(d, VTypes::AtomData), GlyphData::ForceData);
			else obj.gl->data(d)->setAtom(obj.rs->atom(c->argi(d)-1), GlyphData::ForceData); 
		}
		else break;
	}
	rv.reset();
	return TRUE;
}

// Associate atoms with current glyph
bool Command::function_GlyphAtomsR(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argType(d) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(d, VTypes::AtomData), GlyphData::PositionData);
			else obj.gl->data(d)->setAtom(obj.rs->atom(c->argi(d)-1), GlyphData::PositionData); 
		}
		else break;
	}
	rv.reset();
	return TRUE;
}

// Associate atoms with current glyph
bool Command::function_GlyphAtomsV(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argType(d) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(d, VTypes::AtomData), GlyphData::VelocityData);
			else obj.gl->data(d)->setAtom(obj.rs->atom(c->argi(d)-1), GlyphData::VelocityData); 
		}
		else break;
	}
	rv.reset();
	return TRUE;
}

// Store colour data in current glyph
bool Command::function_GlyphColour(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	obj.gl->data(d)->setColour(c->argGLf(1), c->argGLf(2), c->argGLf(3), c->hasArg(4) ? c->argGLf(4) : 1.0f);
	rv.reset();
	return TRUE;
}

// Store vector data in current glyph
bool Command::function_GlyphData(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	// Check range of supplied data item
	int d = c->argi(0) - 1;
	obj.gl->data(d)->setVector(c->argd(1), c->hasArg(2) ? c->argd(2) : 0.0, c->hasArg(3) ? c->argd(3) : 0.0);
	rv.reset();
	return TRUE;
}

// Set 'solid' property of current glyph
bool Command::function_GlyphSolid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	obj.gl->setSolid(c->argb(0));
	rv.reset();
	return TRUE;
}

// Set text property of current glyph
bool Command::function_GlyphText(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return FALSE;
	obj.gl->setText(c->argc(0));
	return TRUE;
}

// Add glyph to current model
bool Command::function_NewGlyph(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Get glyph style
	Glyph::GlyphType gt = Glyph::glyphType(c->argc(0));
	if (gt == Glyph::nGlyphTypes)
	{
		msg.print("Unrecognised glyph style '%s'.\n", c->argc(0));
		return FALSE;
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
	rv.set(VTypes::GlyphData, obj.gl);
	return TRUE;
}

