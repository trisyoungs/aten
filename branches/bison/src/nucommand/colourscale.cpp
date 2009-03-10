/*
	*** ColourScale functions
	*** src/parser/colourscale.cpp
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

#include "nucommand/commands.h"
#include "classes/prefs.h"

// Add point to colourscale
bool NuCommand::function_Addpoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].addPointAtEnd(c->argd(1), c->argf(2), c->argf(3), c->argf(4), c->hasArg(5) ? c->argf(5) : 1.0f);
	return TRUE;
}

// Clear points in colourscale
bool NuCommand::function_Clearpoints(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].clear();
	return TRUE;
}

// List current colourscale data ('listscales')
bool NuCommand::function_Listscales(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	GLfloat col[4];
	msg.print("Current colourscale setup:\n");
	for (int n=0; n<10; n++)
	{
		msg.print( "Scale %i, name = '%s':\n", n+1, prefs.colourScale[n].name());
		if (prefs.colourScale[n].nPoints() == 0) msg.print( "  < No points defined >\n");
		for (ColourScalePoint *csp = prefs.colourScale[n].firstPoint(); csp != NULL; csp = csp->next)
		{
			csp->copyColour(col);
			msg.print( "  (%2i)  %12.5e  %8.4f  %8.4f  %8.4f  %8.4f\n", n+1, csp->value(), col[0], col[1], col[2], col[3]);
		}
	}
	return TRUE;
}

// Remove specific point in colourscale
bool NuCommand::function_Removepoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].removePoint(c->argi(1)-1);
	return TRUE;
}

// Print/set name of colourscale ('scalename <id> [name]')
bool NuCommand::function_Scalename(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	if (c->hasArg(1)) prefs.colourScale[id].setName(c->argc(1));
	else msg.print( "Name of colourscale %i is '%s'.\n",id+1,prefs.colourScale[id].name());
	return TRUE;
}

// Set visibility of colourscale ('scalevisible <id> true|false')
bool NuCommand::function_Scalevisible(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setVisible(c->argb(1));
	return TRUE;
}

// Set existing point in colourscale
bool NuCommand::function_Setpoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setPoint(c->argi(1)-1, c->argd(2), c->argf(3), c->argf(4), c->argf(5), c->hasArg(6) ? c->argf(6) : 1.0f);
	return TRUE;
}

// Set existing point colour in colourscale
bool NuCommand::function_Setpointcolour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setPointColour(c->argi(1)-1, c->argf(2), c->argf(3), c->argf(4), c->hasArg(5) ? c->argf(5) : 1.0f);
	return TRUE;
}

// Set existing point value in colourscale
bool NuCommand::function_Setpointvalue(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return FALSE;
	}
	prefs.colourScale[id].setPointValue(c->argi(1)-1, c->argd(2));
	return TRUE;
}
