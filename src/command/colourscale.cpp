/*
	*** ColourScale Commands
	*** src/command/colourscale.cpp
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
#include "base/prefs.h"
#include "model/bundle.h"

ATEN_USING_NAMESPACE

// Add point to colourscale
bool Commands::function_AddPoint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].addPointAtEnd(c->argd(1), c->argGLf(2), c->argGLf(3), c->argGLf(4), c->hasArg(5) ? c->argGLf(5) : 1.0f);
	rv.reset();
	return TRUE;
}

// Clear points in colourscale
bool Commands::function_ClearPoints(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].clear();
	rv.reset();
	return TRUE;
}

// List current colourscale data ('listscales')
bool Commands::function_ListScales(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	GLfloat col[4];
	Messenger::print("Current colourscale setup:\n");
	for (int n=0; n<10; n++)
	{
		Messenger::print( "Scale %i, name = '%s':\n", n+1, prefs.colourScale[n].name());
		if (prefs.colourScale[n].nPoints() == 0) Messenger::print( "  < No points defined >\n");
		for (ColourScalePoint* csp = prefs.colourScale[n].firstPoint(); csp != NULL; csp = csp->next)
		{
			csp->copyColour(col);
			Messenger::print( "  (%2i)  %12.5e  %8.4f  %8.4f  %8.4f  %8.4f\n", n+1, csp->value(), col[0], col[1], col[2], col[3]);
		}
	}
	rv.reset();
	return TRUE;
}

// Remove specific point in colourscale
bool Commands::function_RemovePoint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].removePoint(c->argi(1)-1);
	rv.reset();
	return TRUE;
}

// Set whether scale is interpolated
bool Commands::function_ScaleInterpolate(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setInterpolated(c->argb(1));
	rv.reset();
	return TRUE;
}

// Print/set name of colourscale ('scalename <id> [name]')
bool Commands::function_ScaleName(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	if (c->hasArg(1)) prefs.colourScale[id].setName(c->argc(1));
	else Messenger::print( "Name of colourscale %i is '%s'.\n",id+1,prefs.colourScale[id].name());
	rv.reset();
	return TRUE;
}

// Set visibility of colourscale ('scalevisible <id> true|false')
bool Commands::function_ScaleVisible(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setVisible(c->argb(1));
	rv.reset();
	return TRUE;
}

// Set existing point in colourscale
bool Commands::function_SetPoint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setPoint(c->argi(1)-1, c->argd(2), c->argGLf(3), c->argGLf(4), c->argGLf(5), c->hasArg(6) ? c->argGLf(6) : 1.0f);
	rv.reset();
	return TRUE;
}

// Set existing point colour in colourscale
bool Commands::function_SetPointColour(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setPointColour(c->argi(1)-1, c->argGLf(2), c->argGLf(3), c->argGLf(4), c->hasArg(5) ? c->argGLf(5) : 1.0f);
	rv.reset();
	return TRUE;
}

// Set existing point value in colourscale
bool Commands::function_SetPointValue(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		Messenger::print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setPointValue(c->argi(1)-1, c->argd(2));
	rv.reset();
	return TRUE;
}

