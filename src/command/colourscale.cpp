/*
	*** ColourScale command functions
	*** src/command/colourscale.cpp
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
#include "base/prefs.h"

// Add point to colourscale
int CommandData::function_CA_ADDPOINT(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].addPointAtEnd(c->argd(1), c->argf(2), c->argf(3), c->argf(4), c->hasArg(5) ? c->argf(5) : 1.0f);
	return CR_SUCCESS;
}

// Clear points in colourscale
int CommandData::function_CA_CLEARPOINTS(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].clear();
	return CR_SUCCESS;
}

// List current colourscale data ('listscales')
int CommandData::function_CA_LISTSCALES(Command *&c, Bundle &obj)
{
	char s[512], type[16];
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
	return CR_SUCCESS;
}

// Remove specific point in colourscale
int CommandData::function_CA_REMOVEPOINT(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].removePoint(c->argi(1)-1);
	return CR_SUCCESS;
}

// Print/set name of colourscale ('scalename <id> [name]')
int CommandData::function_CA_SCALENAME(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	if (c->hasArg(1)) prefs.colourScale[id].setName(c->argc(1));
	else msg.print( "Name of colourscale %i is '%s'.\n",id+1,prefs.colourScale[id].name());
	return CR_SUCCESS;
}

// Set visibility of colourscale ('scalevisible <id> true|false')
int CommandData::function_CA_SCALEVISIBLE(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].setVisible(c->argb(1));
	return CR_SUCCESS;
}

// Set existing point in colourscale
int CommandData::function_CA_SETPOINT(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].setPoint(c->argi(1)-1, c->argd(2), c->argf(3), c->argf(4), c->argf(5), c->hasArg(6) ? c->argf(6) : 1.0f);
	return CR_SUCCESS;
}

// Set existing point colour in colourscale
int CommandData::function_CA_SETPOINTCOLOUR(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].setPointColour(c->argi(1)-1, c->argf(3), c->argf(4), c->argf(5), c->hasArg(6) ? c->argf(6) : 1.0f);
	return CR_SUCCESS;
}

// Set existing point value in colourscale
int CommandData::function_CA_SETPOINTVALUE(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg.print( "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].setPointValue(c->argi(1)-1, c->argd(2));
	return CR_SUCCESS;
}
