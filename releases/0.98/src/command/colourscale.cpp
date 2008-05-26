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

// List current colourscale data ('listscales')
int CommandData::function_CA_LISTSCALES(Command *&c, Bundle &obj)
{
	char s[512], type[16];
	GLfloat lcol[4], mcol[4], rcol[4];
	msg(Debug::None,"  Id Type  Minimum                     Midpoint                    Maximum               Range\n");
	for (int n=0; n<10; n++)
	{
		// Set descriptive text if necessary.
		if (n == 0) strcpy(type,"(Charge)");
		else if (n == 1) strcpy(type,"(Velocity)");
		else if (n == 2) strcpy(type,"(Force)");
		else strcpy(type,"");
		if (prefs.colourScale[n].type() == ColourScale::TwoPoint)
			sprintf(s,"   %i  2   %12.5e                                            %12.5e          %12.5e  %s\n", n, prefs.colourScale[n].minimum(), prefs.colourScale[n].maximum(), prefs.colourScale[n].range(), type);
		else sprintf(s,"   %i  3   %12.5e                %12.5e                %12.5e          %12.5e  %s\n", n, prefs.colourScale[n].minimum(), prefs.colourScale[n].middle(), prefs.colourScale[n].maximum(), prefs.colourScale[n].range(), type);
		msg(Debug::None,s);
		// Now print colour data...
		prefs.colourScale[n].copyColour(ColourScale::MinColour, lcol);
		prefs.colourScale[n].copyColour(ColourScale::MidColour, mcol);
		prefs.colourScale[n].copyColour(ColourScale::MaxColour, rcol);
		if (prefs.colourScale[n].type() == ColourScale::TwoPoint)
			sprintf(s,"         [ %4.2f %4.2f %4.2f %4.2f ] -->                         --> [ %4.2f %4.2f %4.2f %4.2f ]\n", lcol[0], lcol[1], lcol[2], lcol[3], rcol[0], rcol[1], rcol[2], rcol[3]);
		else sprintf(s,"         [ %4.2f %4.2f %4.2f %4.2f ] --> [ %4.2f %4.2f %4.2f %4.2f ] --> [ %4.2f %4.2f %4.2f %4.2f ] \n", lcol[0], lcol[1], lcol[2], lcol[3], mcol[0], mcol[1], mcol[2], mcol[3], rcol[0], rcol[1], rcol[2], rcol[3]);
		msg(Debug::None,s);
	}
	return CR_SUCCESS;
}

// Set maximum colour value ('scalemaxcolour <id> <r> <g> <b> [a]')
int CommandData::function_CA_SCALEMAXCOLOUR(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg(Debug::None, "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	Vec3<GLfloat> colvec = c->arg3f(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.colourScale[id].setColour(ColourScale::MaxColour, colvec.x, colvec.y, colvec.z, alpha);
	return CR_SUCCESS;
}

// Set midpoint colour value ('scalemidcolour <id> <r> <g> <b> [a]')
int CommandData::function_CA_SCALEMIDCOLOUR(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg(Debug::None, "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	Vec3<GLfloat> colvec = c->arg3f(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.colourScale[id].setColour(ColourScale::MidColour, colvec.x, colvec.y, colvec.z, alpha);
	return CR_SUCCESS;
}

// Manually set midpoint of colour scale ('scalemidpoint <id> <d>')
int CommandData::function_CA_SCALEMIDPOINT(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg(Debug::None, "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].setMiddle(c->argd(1));
	return CR_SUCCESS;
}

// Set minimum colour value ('scalemincolour <id> <r> <g> <b> [a]')
int CommandData::function_CA_SCALEMINCOLOUR(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg(Debug::None, "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	Vec3<GLfloat> colvec = c->arg3f(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.colourScale[id].setColour(ColourScale::MinColour, colvec.x, colvec.y, colvec.z, alpha);
	return CR_SUCCESS;
}

// Set range of colourscale ('scalerange <id> <min> <max>')
int CommandData::function_CA_SCALERANGE(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg(Debug::None, "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	prefs.colourScale[id].setRange(c->argd(1), c->argd(2));
	return CR_SUCCESS;
}

// Set type of colourscale ('scaletype <id> <type>')
int CommandData::function_CA_SCALETYPE(Command *&c, Bundle &obj)
{
	// Check range of colourscale id
	int id = c->argi(0) - 1;
	if ((id < 0) || (id > 9))
	{	
		msg(Debug::None, "Colour scale %i is out of range.\n",id+1);
		return CR_FAIL;
	}
	int type = c->argi(1);
	if (type == 2) prefs.colourScale[id].setType(ColourScale::TwoPoint);
	else if (type == 3) prefs.colourScale[id].setType(ColourScale::ThreePoint);
	else
	{
		msg(Debug::None, "'%s' is not a valid order for a colour scale.\n");
		return CR_FAIL;
	}
	return CR_SUCCESS;
}
