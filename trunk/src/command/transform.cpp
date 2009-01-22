/*
	*** Transformation command functions
	*** src/command/transform.cpp
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
#include "classes/prefs.h"

// Rotate selection about specified axis / origin
int Command::function_CA_AXISROTATE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	double angle;
	Atom *i, *j;
	Vec3<double> v, o;
	// Determine which data has been supplied
	switch (c->nArgs())
	{
		// Two atom ids and theta
		case (3):
			i = obj.rs->atom(c->argi(0)-1);
			j = obj.rs->atom(c->argi(1)-1);
			if ((i == NULL) || (j == NULL)) return Command::Fail;
			v = j->r() - i->r();
			angle = c->argd(2);
			break;
		// Axis and theta
		case (4):
			v.set(c->argd(0), c->argd(1), c->argd(2));
			angle = c->argd(3);
			break;
		// Two atom ids, theta, and an origin
		case (6):
			i = obj.rs->atom(c->argi(0)-1);
			j = obj.rs->atom(c->argi(1)-1);
			if ((i == NULL) || (j == NULL)) return Command::Fail;
			v = j->r() - i->r();
			angle = c->argd(2);
			o.set(c->argd(3), c->argd(4), c->argd(5));
			break;
		// Axis, theta, and origin
		case (7):
			v.set(c->argd(0), c->argd(1), c->argd(2));
			angle = c->argd(3);
			o.set(c->argd(4), c->argd(5), c->argd(6));
			break;
		default:
			msg.print("Odd number of arguments given to 'axisrotate'.\n");
			return Command::Fail;
			break;
	}
	char s[128];
	sprintf(s,"Rotate %i atom(s)\n", obj.rs->nSelected());
	obj.rs->beginUndoState(s);
	obj.rs->rotateSelectionVector(o, v, angle);
	obj.rs->endUndoState();
	return Command::Success;
}

// Centre selection at given coordinates
int Command::function_CA_CENTRE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->parent()->inputFile() == NULL)
	{
		char s[128];
		Vec3<double> centre = c->arg3d(0);
		sprintf(s,"Centre %i atom(s) at %f %f %f\n", obj.rs->nSelected(), centre.x, centre.y, centre.z);
		obj.rs->beginUndoState(s);
		obj.rs->centre(centre);
		obj.rs->endUndoState();
	}
	else if (prefs.centreOnLoad() != Prefs::SwitchOff) obj.rs->centre(c->arg3d(0));
	return Command::Success;
}

// Translate current selection in local coordinates ('translate dx dy dz')
int Command::function_CA_TRANSLATE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	char s[128];
	Vec3<double> tvec = c->arg3d(0);
	sprintf(s,"Translate Cartesian (%i atom(s), %f %f %f)\n", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->beginUndoState(s);
	obj.rs->translateSelectionLocal(tvec);
	obj.rs->endUndoState();
	return Command::Success;
}

// Translate activeatom ('translateatom <dx dy dz>')
int Command::function_CA_TRANSLATEATOM(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	char s[128];
	Vec3<double> tvec = c->arg3d(0);
	sprintf(s,"Translate Cartesian (atom %i, %f %f %f)\n", obj.i->id()+1, tvec.x, tvec.y, tvec.z);
	obj.rs->beginUndoState(s);
	obj.rs->translateAtom(obj.i, tvec);
	obj.rs->endUndoState();
	return Command::Success;
}

// Translate current selection in fractional cell coordinates ('translatecell dx dy dz')
int Command::function_CA_TRANSLATECELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Vec3<double> tvec;
	tvec = obj.rs->cell()->axes() * c->arg3d(0);
	char s[128];
	sprintf(s,"Translate Cell (%i atom(s), %f %f %f)\n", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->beginUndoState(s);
	obj.rs->translateSelectionLocal(tvec);
	obj.rs->endUndoState();
	return Command::Success;
}

// Convert coordinates from one reference frame to another
int Command::function_CA_MATRIXCONVERT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Determine which data has been supplied
	Mat3<double> source, target;
	Vec3<double> o, v;
	int n;
	printf("Shit.\n");
	Atom *i, *j;
	switch (c->nArgs())
	{
		// Twelve atom ids defining both matrices (and optional origin)
		case (12):
		case (15):
			for (n=0; n<6; n+=2)
			{
				// Source matrix
				i = obj.rs->atom(c->argi(n)-1);
				j = obj.rs->atom(c->argi(n+1)-1);
				if ((i == NULL) || (j == NULL)) return Command::Fail;
				v = j->r() - i->r();
				v.normalise();
				// Adjust (if necessary) before storing)
				if (n == 2) v.orthogonalise(source.x());
				else if (n == 3) v.orthogonalise(source.x(), source.y());
				source.set(n/2, v);
				// Target matrix
				i = obj.rs->atom(c->argi(n+6)-1);
				j = obj.rs->atom(c->argi(n+7)-1);
				if ((i == NULL) || (j == NULL)) return Command::Fail;
				v = j->r() - i->r();
				v.normalise();
				// Adjust (if necessary) before storing)
				if (n == 2) v.orthogonalise(target.x());
				else if (n == 3) v.orthogonalise(target.x(), target.y());
				target.set(n/2, v);
			}
			// Get origin if provided
			if (c->nArgs() == 15) o.set(c->argd(12), c->argd(13), c->argd(14));
			break;
		case (18):
		case (21):
			for (n=0; n<9; n++) source.set(n, c->argd(n));
			for (n=9; n<18; n++) target.set(n, c->argd(n));
			// Get origin if provided
			if (c->nArgs() == 21) o.set(c->argd(12), c->argd(13), c->argd(14));
			break;
		default:
			return Command::Fail;
			break;
	}
	if (msg.isOutputActive(Messenger::Verbose))
	{
		msg.print("Source matrix:\n");
		source.print();
		msg.print("Target matrix:\n");
		target.print();
	}
	// Generate necessary rotation matrix
	target = target.transpose();
	Mat3<double> rotmat = target * source;
	// Perform transformation
	char s[128];
	sprintf(s,"Transform %i atom(s)\n", obj.rs->nSelected());
	obj.rs->beginUndoState(s);
	obj.rs->matrixTransformSelection(o, rotmat);
	obj.rs->endUndoState();
	return Command::Success;
}

// Transform coordinates using supplied matrix / origin
int Command::function_CA_MATRIXTRANSFORM(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Determine which data has been supplied
	Mat3<double> mat;
	Vec3<double> o;
	int n;
	switch (c->nArgs())
	{
		// Six atom ids defining matrix axes (and optional origin)
		case (9):
		case (12):
			for (n=0; n<9; n++) mat.set(n, c->argd(n));
			// Get origin if provided
			if (c->nArgs() == 12) o.set(c->argd(9), c->argd(10), c->argd(11));
			break;
		default:
			return Command::Fail;
	}
	// Perform transformation
	char s[128];
	sprintf(s,"Transform %i atom(s)\n", obj.rs->nSelected());
	obj.rs->beginUndoState(s);
	obj.rs->matrixTransformSelection(o, mat);
	obj.rs->endUndoState();
	return Command::Success;
}

// Mirror selection along specified axis
int Command::function_CA_MIRROR(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	char s[128];
	sprintf(s,"Mirror %i atoms along %c\n", obj.rs->nSelected(), 88+c->argi(0));
	obj.rs->beginUndoState(s);
	obj.rs->mirrorSelectionLocal(c->argi(0));
	obj.rs->endUndoState();
	return Command::Success;
}
