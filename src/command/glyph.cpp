/*
	*** Glyph Commands
	*** src/command/glyph.cpp
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
#include "model/bundle.h"
#include "model/model.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Auto-add ellipsoids to current atom selection
bool Commands::function_AutoEllipsoids(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->addEllipsoidGlyphs();
	rv.reset();
	return true;
}

// Auto-add polyhedra to current atom selection
bool Commands::function_AutoPolyhedra(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	bool centresonly = true, linkatoms = true;
	double rcut = 4.0;
	for (int i=0; i < c->nArgs(); i++)
	{
		// Split argument into keyword and value
		QStringList items = c->argc(i).split('=');
		if (items.at(0) == "fragments") centresonly = false;
		else if (items.at(0) == "centres") centresonly = true;
		else if (items.at(0) == "nolink") linkatoms = false;
		else if (items.at(0) == "rcut") rcut = items.at(1).toDouble();
		else Messenger::print("Unknown option '%s' given to 'autopolyhedra'.", qPrintable(items.at(0)));
	}

	// Add the polyhedra
	obj.m->addPolyhedraGlyphs(centresonly, linkatoms, rcut);
	rv.reset();
	return true;
}

// Associate atom with current glyph
bool Commands::function_GlyphAtomF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		Messenger::print("Data index given to 'glyphatomf' (%i) is out of range.", d);
		return false;
	}

	// If second argument was given, it refers to either an atom by pointer or by id
	Atom* target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argType(1) == VTypes::AtomData) target = (Atom*) c->argp(1, VTypes::AtomData);
		else target = obj.rs()->atom(c->argi(1) - 1);
	}

	// Finally, check pointer currently in target and store it
	obj.gl->data(d)->setAtom(target, GlyphData::ForceData);
	if (target == NULL) Messenger::print("Warning - NULL atom stored in glyph data %i.",d);

	rv.reset();
	return true;
}

// Associate atom with current glyph
bool Commands::function_GlyphAtomR(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		Messenger::print("Data index given to 'glyphatomr' (%i) is out of range.", d);
		return false;
	}

	// If second argument was given, it refers to either an atom by pointer or by id
	Atom* target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argType(1) == VTypes::AtomData) target = (Atom*) c->argp(1, VTypes::AtomData);
		else target = obj.rs()->atom(c->argi(1) - 1);
	}

	// Finally, check pointer currently in target and store it
	obj.gl->data(d)->setAtom(target, GlyphData::PositionData);
	if (target == NULL) Messenger::print("Warning - NULL atom stored in glyph data %i.",d);

	rv.reset();
	return true;
}

// Associate atom with current glyph
bool Commands::function_GlyphAtomV(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// Check range of supplied data item
	int d = c->argi(0) - 1;
	if ((d < 0) || (d >= Glyph::nGlyphData(obj.gl->type())))
	{
		Messenger::print("Data index given to 'glyphatomv' (%i) is out of range.", d);
		return false;
	}

	// If second argument was given, it refers to either an atom by pointer or by id
	Atom* target = obj.i;
	if (c->hasArg(1))
	{
		if (c->argType(1) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(1, VTypes::AtomData), GlyphData::VelocityData);
		else target = obj.rs()->atom(c->argi(1) - 1);
	}
	obj.gl->data(d)->setAtom(target, GlyphData::VelocityData); 
	if (target == NULL) Messenger::print("Warning - NULL atom stored in glyph data %i.",d);

	rv.reset();
	return true;
}

// Associate atoms with current glyph
bool Commands::function_GlyphAtomsF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argType(d) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(d, VTypes::AtomData), GlyphData::ForceData);
			else obj.gl->data(d)->setAtom(obj.rs()->atom(c->argi(d)-1), GlyphData::ForceData); 
		}
		else break;
	}

	rv.reset();
	return true;
}

// Associate atoms with current glyph
bool Commands::function_GlyphAtomsR(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argType(d) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(d, VTypes::AtomData), GlyphData::PositionData);
			else obj.gl->data(d)->setAtom(obj.rs()->atom(c->argi(d)-1), GlyphData::PositionData); 
		}
		else break;
	}

	rv.reset();
	return true;
}

// Associate atoms with current glyph
bool Commands::function_GlyphAtomsV(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// If second argument was given, it refers to either an atom by pointer or by id
	for (int d=0; d<Glyph::nGlyphData(obj.gl->type()); d++)
	{
		if (c->hasArg(d))
		{
			if (c->argType(d) == VTypes::AtomData) obj.gl->data(d)->setAtom((Atom*) c->argp(d, VTypes::AtomData), GlyphData::VelocityData);
			else obj.gl->data(d)->setAtom(obj.rs()->atom(c->argi(d)-1), GlyphData::VelocityData); 
		}
		else break;
	}

	rv.reset();
	return true;
}

// Set n'th colour data in current glyph
bool Commands::function_GlyphColour(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// Check range of supplied data item
	int d = c->argi(0) - 1;
	bool result = true;
	if ((d < 0) || (d >= obj.gl->nData()))
	{
		Messenger::print("Data index %i is out of range for current glyph.", c->argi(0));
		result = false;
	}
	else obj.gl->data(d)->setColour(c->argGLf(1), c->argGLf(2), c->argGLf(3), c->hasArg(4) ? c->argGLf(4) : 1.0f);

	rv.reset();
	return result;
}

// Set all colour data in current glyph
bool Commands::function_GlyphColours(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;
	for (int d = 0; d<obj.gl->nData(); ++d) obj.gl->data(d)->setColour(c->argGLf(0), c->argGLf(1), c->argGLf(2), c->hasArg(3) ? c->argGLf(3) : 1.0f);
	rv.reset();
	return true;
}

// Store vector data in current glyph
bool Commands::function_GlyphData(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;

	// Check range of supplied data item
	int d = c->argi(0) - 1;
	bool result = true;
	if ((d < 0) || (d >= obj.gl->nData()))
	{
		Messenger::print("Data index %i is out of range for current glyph.", c->argi(0));
		result = false;
	}
	obj.gl->data(d)->setVector(c->argd(1), c->hasArg(2) ? c->argd(2) : 0.0, c->hasArg(3) ? c->argd(3) : 0.0);

	rv.reset();
	return result;
}

// Set 'solid' property of current glyph
bool Commands::function_GlyphSolid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;
	obj.gl->setSolid(c->argb(0));
	rv.reset();
	return true;
}

// Set text property of current glyph
bool Commands::function_GlyphText(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::GlyphPointer)) return false;
	obj.gl->setText(c->argc(0));
	return true;
}

// Add glyph to current model
bool Commands::function_NewGlyph(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Get glyph style
	Glyph::GlyphType gt = Glyph::glyphType(c->argc(0), true);
	if (gt == Glyph::nGlyphTypes)
	{
		Messenger::print("Unrecognised glyph style '%s'.", qPrintable(c->argc(0)));
		return false;
	}
	obj.gl = obj.rs()->addGlyph(gt);
	rv.set(VTypes::GlyphData, obj.gl);

	// Set default text for text glyphs
	if ((gt == Glyph::TextGlyph) || (gt == Glyph::Text3DGlyph)) obj.gl->setText("NewGlyph");

	// Parse extra options
	Glyph::GlyphOption gopt;
	Vec4<double> rgba;
	LineParser lp, lp2;
	if (c->hasArg(1)) lp.getArgsDelim(LineParser::UseCurlies, c->argc(1));
	for (int i=1; i < c->nArgs(); i++)
	{
		// Split argument into keyword and value
		QStringList items = c->argc(i).split('=');
		gopt = Glyph::glyphOption(items.at(0));
		switch (gopt)
		{
			case (Glyph::GlyphColourOption):
				// Colour should have been supplied as a hex value
				// Break up the value argument into three (four) RGB(A) values
				lp2.getArgsDelim(0, items.at(1));
				if (lp.nArgs() == 3) rgba.set(lp.argd(0), lp.argd(1), lp.argd(2), 1.0);
				else if (lp.nArgs() == 4) rgba.set(lp.argd(0), lp.argd(1), lp.argd(2), lp.argd(3));
				else
				{
					Messenger::print("Mangled RGB(A) specification for glyph 'colour' option ['%s']", qPrintable(items.at(1)));
					return false;
				}
				for (int d = 0; d<obj.gl->nData(); ++d) obj.gl->data(d)->setColour(rgba.x, rgba.y, rgba.z, rgba.w);
				break;
			case (Glyph::GlyphSolidOption):
				obj.gl->setSolid(true);
				break;
			case (Glyph::GlyphTextOption):
				obj.gl->setText(items.at(1));
				break;
			case (Glyph::GlyphWireOption):
				obj.gl->setSolid(false);
				break;
			default:
				Messenger::print("Unknown option '%s' given to 'newglyph'.", qPrintable(items.at(0)));
				return false;
		}
	}
	return true;
}

