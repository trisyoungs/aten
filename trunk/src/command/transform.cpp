/*
	*** Transformation Commands
	*** src/command/transform.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "parser/tree.h"
#include "model/model.h"
#include "classes/prefs.h"

// Rotate selection about specified axis / origin
bool Command::function_AxisRotate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
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
			if ((i == NULL) || (j == NULL)) return FALSE;
			v = obj.rs->cell()->mimd(j,i);
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
			if ((i == NULL) || (j == NULL)) return FALSE;
			v = obj.rs->cell()->mimd(j,i);
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
			return FALSE;
			break;
	}
	obj.rs->beginUndoState("Rotate %i atom(s)", obj.rs->nSelected());
	obj.rs->rotateSelectionVector(o, v, angle);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Centre selection at given coordinates
bool Command::function_Centre(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!c->parent()->isFilter())
	{
		Vec3<double> centre = c->arg3d(0);
		Vec3<int> lock(0,0,0);
		if (c->hasArg(5)) lock = c->arg3i(3);
		obj.rs->beginUndoState("Centre %i atom(s) at %f %f %f", obj.rs->nSelected(), centre.x, centre.y, centre.z);
		obj.rs->centre(centre, lock.x, lock.y, lock.z);
		obj.rs->endUndoState();
	}
	else if (prefs.centreOnLoad() != Prefs::SwitchOff) obj.rs->centre(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Flip selection's x-coordinates
bool Command::function_FlipX(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Flip x-coordinates of %i atom(s)", obj.rs->nSelected());
	obj.rs->mirrorSelectionLocal(0);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Flip selection's y-coordinates
bool Command::function_FlipY(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Flip y-coordinates of %i atom(s)", obj.rs->nSelected());
	obj.rs->mirrorSelectionLocal(1);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Flip selection's z-coordinates
bool Command::function_FlipZ(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Flip z-coordinates of %i atom(s)", obj.rs->nSelected());
	obj.rs->mirrorSelectionLocal(2);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Convert coordinates from one reference frame to another
bool Command::function_MatrixConvert(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Determine which data has been supplied
	Mat3<double> source, target;
	Vec3<double> o, v;
	bool sourcenoz = FALSE, targetnoz = FALSE;
	int n;
	Atom *i, *j;
	switch (c->nArgs())
	{
		// Twelve atom ids defining both matrices (and optional origin)
		case (12):
		case (15):
			// Determine axes for the matrices
			for (n=0; n<3; n++)
			{
				// Source matrix
				if ((n == 2) && (c->argi(n*2) == 0) && (c->argi(n*2+1) == 0)) sourcenoz = TRUE;
				else
				{
					i = obj.rs->atom(c->argi(n*2)-1);
					j = obj.rs->atom(c->argi(n*2+1)-1);
					if ((i == NULL) || (j == NULL)) return FALSE;
					v = obj.rs->cell()->mimd(j,i);
					v.normalise();
					source.set(n, v);
				}
				// Target matrix
				if ((n == 2) && (c->argi(n*2+6) == 0) && (c->argi(n*2+7) == 0)) targetnoz = TRUE;
				else
				{
					i = obj.rs->atom(c->argi(n*2+6)-1);
					j = obj.rs->atom(c->argi(n*2+7)-1);
					if ((i == NULL) || (j == NULL)) return FALSE;
					v = obj.rs->cell()->mimd(j,i);
					v.normalise();
					target.set(n, v);
				}
				// Adjust Y and Z axes
				if (n == 1)
				{
					source.y().orthogonalise(source.x());
					target.y().orthogonalise(target.x());
				}
				else if (n == 2)
				{
					if (sourcenoz) source.z() = source.x() * source.y();
					else source.z().orthogonalise(source.x(), source.y());
					if (targetnoz) target.z() = target.x() * target.y();
					else target.z().orthogonalise(target.x(), target.y());
				}
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
			return FALSE;
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
	obj.rs->beginUndoState("Transform %i atom(s)", obj.rs->nSelected());
	obj.rs->matrixTransformSelection(o, rotmat);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Transform coordinates using supplied matrix / origin
bool Command::function_MatrixTransform(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
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
			return FALSE;
	}
	// Perform transformation
	obj.rs->beginUndoState("Transform %i atom(s)", obj.rs->nSelected());
	obj.rs->matrixTransformSelection(o, mat);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Mirror selection along specified axis
bool Command::function_Mirror(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Mirror %i atoms along %c", obj.rs->nSelected(), 88+c->argi(0));
	obj.rs->mirrorSelectionLocal(c->argi(0));
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Convert coordinates (specified in atoms/ids) from one reference frame to another
bool Command::function_Reorient(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Determine which data has been supplied
	Mat3<double> source, target;
	Vec3<double> o, v;
	bool sourcenoz = FALSE, targetnoz = FALSE;
	int n;
	Atom *i, *j;

	// Determine source matrix
	for (n=0; n<5; n+=2)
	{
		if (c->argType(n) == VTypes::IntegerData)
		{
			if ((n == 4) && (c->argi(n) == 0)) sourcenoz = TRUE;
			else i = obj.rs->atom(c->argi(n)-1);
		}
		else i = (Atom*) c->argp(n, VTypes::AtomData);
		if (c->argType(n+1) == VTypes::IntegerData)
		{
			if ((n == 4) && (c->argi(n+1) == 0)) sourcenoz = TRUE;
			else j = obj.rs->atom(c->argi(n+1)-1);
		}
		else j = (Atom*) c->argp(n+1, VTypes::AtomData);
		if ((i == NULL) || (j == NULL)) return FALSE;
		v = obj.rs->cell()->mimd(j,i);
		v.normalise();
		source.set(n/2, v);
	}
	// Orthogonalise matrix
	source.y().orthogonalise(source.x());
	if (sourcenoz) source.z() = source.x() * source.y();
	else source.z().orthogonalise(source.x(), source.y());

	// Determine target matrix
	for (n=0; n<9; ++n) target.set(0, c->argd(n+6));

	// Get origin if provided
	if (c->nArgs() == 18) o.set(c->argd(15), c->argd(16), c->argd(17));

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
	obj.rs->beginUndoState("Reorient %i atom(s)", obj.rs->nSelected());
	obj.rs->matrixTransformSelection(o, rotmat);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Translate current selection in local coordinates ('translate dx dy dz')
bool Command::function_Translate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> tvec = c->arg3d(0);
	obj.rs->beginUndoState("Translate Cartesian (%i atom(s), %f %f %f)", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->translateSelectionLocal(tvec);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Translate activeatom ('translateatom <dx dy dz>')
bool Command::function_TranslateAtom(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::AtomPointer)) return FALSE;
	Vec3<double> tvec = c->arg3d(0);
	obj.rs->beginUndoState("Translate Cartesian (atom %i, %f %f %f)", obj.i->id()+1, tvec.x, tvec.y, tvec.z);
	obj.rs->translateAtom(obj.i, tvec);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Translate current selection in fractional cell coordinates ('translatecell dx dy dz')
bool Command::function_TranslateCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> tvec;
	tvec = obj.rs->cell()->axes() * c->arg3d(0);
	obj.rs->beginUndoState("Translate Cell (%i atom(s), %f %f %f)", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->translateSelectionLocal(tvec);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Translate current selection in world coordinates ('translateworld dx dy dz')
bool Command::function_TranslateWorld(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> tvec;
	tvec = c->arg3d(0);
	obj.rs->beginUndoState("Translate World (%i atom(s), %f %f %f)", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->translateSelectionWorld(tvec);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}
