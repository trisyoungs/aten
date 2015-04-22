/*
	*** Transformation Commands
	*** src/command/transform.cpp
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

ATEN_USING_NAMESPACE

// Rotate selection about specified axis / origin
bool Commands::function_AxisRotate(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	double angle;
	Atom* i, *j;
	Vec3<double> v, o;
	// Determine which data has been supplied
	switch (c->nArgs())
	{
		// Two atom ids and theta
		case (3):
			i = obj.rs()->atom(c->argi(0)-1);
			j = obj.rs()->atom(c->argi(1)-1);
			if ((i == NULL) || (j == NULL)) return false;
			v = obj.rs()->cell()->mimVector(i,j);
			angle = c->argd(2);
			break;
		// Axis and theta
		case (4):
			v.set(c->argd(0), c->argd(1), c->argd(2));
			angle = c->argd(3);
			break;
		// Two atom ids, theta, and an origin
		case (6):
			i = obj.rs()->atom(c->argi(0)-1);
			j = obj.rs()->atom(c->argi(1)-1);
			if ((i == NULL) || (j == NULL)) return false;
			v = obj.rs()->cell()->mimVector(i,j);
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
			Messenger::print("Odd number of arguments given to 'axisrotate'.");
			return false;
			break;
	}
	obj.rs()->beginUndoState("Rotate %i atom(s)", obj.rs()->nSelected());
	obj.rs()->rotateSelectionVector(o, v, angle);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Centre selection at given coordinates
bool Commands::function_Centre(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->parent()->parser() == NULL)
	{
		Vec3<double> centre = c->arg3d(0);
		Vec3<int> lock(0,0,0);
		if (c->hasArg(5)) lock = c->arg3i(3);
		obj.rs()->beginUndoState("Centre %i atom(s) at %f %f %f", obj.rs()->nSelected(), centre.x, centre.y, centre.z);
		obj.rs()->centre(centre, lock.x, lock.y, lock.z);
		obj.rs()->endUndoState();
	}
	else if (prefs.centreOnLoad() != Choice::No) obj.rs()->centre(c->arg3d(0));
	rv.reset();
	return true;
}

// Flip selection's x-coordinates
bool Commands::function_FlipX(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Flip x-coordinates of %i atom(s)", obj.rs()->nSelected());
	obj.rs()->mirrorSelectionLocal(0);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Flip selection's y-coordinates
bool Commands::function_FlipY(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Flip y-coordinates of %i atom(s)", obj.rs()->nSelected());
	obj.rs()->mirrorSelectionLocal(1);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Flip selection's z-coordinates
bool Commands::function_FlipZ(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Flip z-coordinates of %i atom(s)", obj.rs()->nSelected());
	obj.rs()->mirrorSelectionLocal(2);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Convert coordinates from one reference frame to another
bool Commands::function_MatrixConvert(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Determine which data has been supplied
	Matrix source, target;
	Vec3<double> o, v;
	bool sourcenoz = false, targetnoz = false;
	int n;
	Atom* i, *j;
	switch (c->nArgs())
	{
		// Twelve atom ids defining both matrices (and optional origin)
		case (12):
		case (15):
			// Determine axes for the matrices
			for (n=0; n<3; n++)
			{
				// Source matrix
				if ((n == 2) && (c->argi(n*2) == 0) && (c->argi(n*2+1) == 0)) sourcenoz = true;
				else
				{
					i = obj.rs()->atom(c->argi(n*2)-1);
					j = obj.rs()->atom(c->argi(n*2+1)-1);
					if ((i == NULL) || (j == NULL)) return false;
					v = obj.rs()->cell()->mimVector(i,j);
					v.normalise();
					source.setColumn(n, v, 0.0);
				}
				// Target matrix
				if ((n == 2) && (c->argi(n*2+6) == 0) && (c->argi(n*2+7) == 0)) targetnoz = true;
				else
				{
					i = obj.rs()->atom(c->argi(n*2+6)-1);
					j = obj.rs()->atom(c->argi(n*2+7)-1);
					if ((i == NULL) || (j == NULL)) return false;
					v = obj.rs()->cell()->mimVector(i,j);
					v.normalise();
					target.setColumn(n, v, 0.0);
				}
				// Adjust Y and Z axes
				if (n == 1)
				{
					source.orthogonaliseColumn(1, 0);
					target.orthogonaliseColumn(1, 0);
				}
				else if (n == 2)
				{
					if (sourcenoz) v = source.columnAsVec3(0) * source.columnAsVec3(1);
					else source.orthogonaliseColumn(2, 0, 1);
					if (targetnoz) v = target.columnAsVec3(0) * target.columnAsVec3(1);
					else target.orthogonaliseColumn(2, 0, 1);
				}
			}
			// Get origin if provided
			if (c->nArgs() == 15) o.set(c->argd(12), c->argd(13), c->argd(14));
			break;
		case (18):
		case (21):
			for (n=0; n<9; n++) source[(n/3)*4 + n%3] = c->argd(n);
			for (n=9; n<18; n++) target[((n-9)/3)*4 + n%3] = c->argd(n);
			// Get origin if provided
			if (c->nArgs() == 21) o.set(c->argd(18), c->argd(19), c->argd(20));
			break;
		default:
			return false;
			break;
	}
	if (Messenger::isOutputActive(Messenger::Verbose))
	{
		Messenger::print("Source matrix:");
		source.print();
		Messenger::print("Target matrix:");
		target.print();
	}
	// Generate necessary rotation matrix
	source.invert();
	Matrix rotmat = target * source;
	if (Messenger::isOutputActive(Messenger::Verbose))
	{
		Messenger::print("Generated rotation matrix:");
		rotmat.print();
	}
	// Perform transformation
	obj.rs()->beginUndoState("Transform %i atom(s)", obj.rs()->nSelected());
	obj.rs()->matrixTransformSelection(o, rotmat);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Transform coordinates using supplied matrix / origin
bool Commands::function_MatrixTransform(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Determine which data has been supplied
	Matrix mat;
	Vec3<double> o;
	int n;
	switch (c->nArgs())
	{
		// Six atom ids defining matrix axes (and optional origin)
		case (9):
		case (12):
			for (n=0; n<9; n++) mat[(n/3)*4+n%3] = c->argd(n);
			// Get origin if provided
			if (c->nArgs() == 12) o.set(c->argd(9), c->argd(10), c->argd(11));
			break;
		default:
			printf("Internal Error: 'matrixtransform' was passed a strange (%i) number of arguments.\n", c->nArgs());
			return false;
	}
	// Perform transformation
	obj.rs()->beginUndoState("Transform %i atom(s)", obj.rs()->nSelected());
	obj.rs()->matrixTransformSelection(o, mat);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Mirror selection along specified axis
bool Commands::function_Mirror(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Mirror %i atoms along %c", obj.rs()->nSelected(), 88+c->argi(0));
	obj.rs()->mirrorSelectionLocal(c->argi(0));
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Convert coordinates (specified in atoms/ids) from one reference frame to another
bool Commands::function_Reorient(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Determine which data has been supplied
	Matrix source, target;
	Vec3<double> o, v;
	bool sourcenoz = false;
	int n;
	Atom* i = NULL, *j = NULL;

	// Determine source matrix
	for (n=0; n<5; n+=2)
	{
		if (c->argType(n) == VTypes::IntegerData)
		{
			if ((n == 4) && (c->argi(n) == 0)) sourcenoz = true;
			else i = obj.rs()->atom(c->argi(n)-1);
		}
		else i = (Atom*) c->argp(n, VTypes::AtomData);
		if (c->argType(n+1) == VTypes::IntegerData)
		{
			if ((n == 4) && (c->argi(n+1) == 0)) sourcenoz = true;
			else j = obj.rs()->atom(c->argi(n+1)-1);
		}
		else j = (Atom*) c->argp(n+1, VTypes::AtomData);
		if ((i == NULL) || (j == NULL)) return false;
		v = obj.rs()->cell()->mimVector(i,j);
		v.normalise();
		source.setColumn(n/2, v, 0.0);
	}
	// Orthogonalise matrix
	source.orthogonaliseColumn(1, 0);
	if (sourcenoz) source.setColumn(2, source.columnAsVec3(0) * source.columnAsVec3(1), 0.0);
	else source.orthogonaliseColumn(2, 0, 1);

	// Determine target matrix
	for (n=0; n<9; ++n) target[(n/3)*4+n%3] = c->argd(n+6);

	// Get origin if provided
	if (c->nArgs() == 18) o.set(c->argd(15), c->argd(16), c->argd(17));

	if (Messenger::isOutputActive(Messenger::Verbose))
	{
		Messenger::print("Source matrix:");
		source.print();
		Messenger::print("Target matrix:");
		target.print();
	}

	// Generate necessary rotation matrix
	target = target.transpose();
	Matrix rotmat = target * source;

	// Perform transformation
	obj.rs()->beginUndoState("Reorient %i atom(s)", obj.rs()->nSelected());
	obj.rs()->matrixTransformSelection(o, rotmat);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Alter angle between three specified atoms
bool Commands::function_SetAngle(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.reset();
	Atom* i = c->argType(0) == VTypes::IntegerData ? obj.rs()->atom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	if (i == NULL)
	{
		Messenger::print("Atom 'i' given to 'setangle' is NULL.");
		return false;
	}
	Atom* j = c->argType(1) == VTypes::IntegerData ? obj.rs()->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
	if (j == NULL)
	{
		Messenger::print("Atom 'j' given to 'setangle' is NULL.");
		return false;
	}
	Atom* k = c->argType(2) == VTypes::IntegerData ? obj.rs()->atom(c->argi(2)-1) : (Atom*) c->argp(2, VTypes::AtomData);
	if (k == NULL)
	{
		Messenger::print("Atom 'k' given to 'setangle' is NULL.");
		return false;
	}

	// Clear any current marked selection
	obj.rs()->selectNone(true);

	// Find bond (if any) between i and j
	Bond* b = i->findBond(j);

	// Perform mark-only tree select on atom j, excluding any bond to atom i
	obj.rs()->selectTree(k, true, false, b);

	// If atom 'i' is now marked, there is a cyclic route connecting the two atoms and we can't proceed
	if (i->isSelected(true))
	{
		Messenger::print("Can't alter the angle of three atoms i-j-k where 'i' and 'k' exist in the same cyclic moiety, or are unbound and within the same fragment.");
		return false;
	}

	// Get current angle between the three atoms
	double angle = obj.rs()->angle(i,j,k);

	// Get cross product of bond vectors to define rotation axis
	Vec3<double> v = obj.rs()->cell()->mimVector(j,k) * obj.rs()->cell()->mimVector(j,i);
	v.normalise();
	double delta = c->argd(3) - angle;

	obj.rs()->beginUndoState("Set angle between atoms");
	obj.rs()->rotateSelectionVector(j->r(), v, delta, true);
	obj.rs()->endUndoState();
	return true;
}

// Set angles for all selected atoms
bool Commands::function_SetAngles(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.reset();

	// Determine move type (0 = lowId, 1 = highId, 2 = i & k) and determine whether we are just nudging
	double target = c->argd(0);
	int moveType = 0;
	if (c->hasArg(1))
	{
		if (c->argc(1) == "low") moveType = 0;
		else if (c->argc(1) == "high") moveType = 1;
		else if (c->argc(1) == "both") moveType = 2;
		else Messenger::warn(QString("Move type for 'setAngles' unrecognised (%1) - defaulting to 'low'").arg(c->argc(1)));
	}
	bool nudge = c->hasArg(2) ? c->argb(2) : false;
	if (nudge && (moveType == 2)) target *= 0.5;

	// Start undo state
	obj.rs()->beginUndoState("Set angles between atoms");

	// Loop over selected atoms
	Atom* i, *j, *k;
	Refitem<Bond,int>* bi, *bk;
	Bond* bji, *bjk;
	Vec3<double> rotationVector;
	double delta;
	for (Refitem<Atom,int>* rj = obj.rs()->selection(); rj != NULL; rj = rj->next)
	{
		j = rj->item;

		// Double loop over bonds on this atom
		for (bi = j->bonds(); bi != NULL; bi = bi->next)
		{
			bji = bi->item;
			i = bji->partner(j);
			if (!i->isSelected()) continue;

			// Inner loop
			for (bk = bi->next; bk != NULL; bk = bk->next)
			{
				bjk = bk->item;
				k = bji->partner(j);
				if (!k->isSelected()) continue;

				// Have found an angle... check for a cyclic route between i and k
				// Clear any current marked selection
				obj.rs()->selectNone(true);

				// Perform mark-only tree select on atom k (or i), excluding the bond ji (or jk)
				if (moveType == 0) obj.rs()->selectTree(k, true, false, bji);
				else obj.rs()->selectTree(i, true, false, bjk);

				// If atom 'i' is now marked, there is a cyclic route connecting the two atoms and we can't proceed
				if (i->isSelected(true))
				{
					Messenger::print("Can't alter the angle of three atoms i-j-k (%i-%i-%i) where atoms 'i' and 'k' exist in the same cyclic moiety.", i->id()+1, j->id()+1, k->id()+1);
					continue;
				}

				// Get angle delta, and define rotation axis
				delta = nudge ? target : target - obj.rs()->angle(i,j,k);
				rotationVector = obj.rs()->cell()->mimVector(j,k) * obj.rs()->cell()->mimVector(j,i);
				rotationVector.normalise();

				// Move current marked atoms (exactly which are marked depends on the moveType, but we can move the current selection regardless)
				obj.rs()->rotateSelectionVector(j->r(), rotationVector, delta, true);
				if (moveType == 2)
				{
					obj.rs()->selectNone(true);
					obj.rs()->selectTree(k, true, false, bji);
					obj.rs()->rotateSelectionVector(j->r(), rotationVector, -delta, true);
				}
			}
		}
	}

	// End undostate
	obj.rs()->endUndoState();

	return true;
}

// Alter distance between two specified atoms
bool Commands::function_SetDistance(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.reset();
	Atom* i = c->argType(0) == VTypes::IntegerData ? obj.rs()->atom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	if (i == NULL)
	{
		Messenger::print("Atom 'i' given to 'setdistance' is NULL.");
		return false;
	}
	Atom* j = c->argType(1) == VTypes::IntegerData ? obj.rs()->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
	if (j == NULL)
	{
		Messenger::print("Atom 'j' given to 'setdistance' is NULL.");
		return false;
	}

	// Clear any current marked selection
	obj.rs()->selectNone(true);

	// Find bond (if any) between i and j
	Bond* b = i->findBond(j);

	// Perform mark-only tree select on atom j, excluding any bond to atom i
	obj.rs()->selectTree(j, true, false, b);

	// If atom 'i' is now marked, there is a cyclic route connecting the two atoms and we can't proceed
	if (i->isSelected(true))
	{
		Messenger::print("Can't alter the distance of two atoms i-j that exist in the same cyclic moiety, or are unbound and within the same fragment.");
		return false;
	}

	// Grab the minimum image vector between the two atoms, and shift all those currently marked
	Vec3<double> v = obj.rs()->cell()->mimVector(i,j);
	double delta = c->argd(2) - v.magnitude();
	v.normalise();
	v *= delta;
	obj.rs()->beginUndoState("Set distance between atoms");
	obj.rs()->translateSelectionLocal(v, true);
	obj.rs()->endUndoState();

	return true;
}

// Set distances for all selected atoms
bool Commands::function_SetDistances(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.reset();

	// Determine move type (0 = lowId, 1 = highId, 2 = i & k) and determine whether we are just nudging
	double target = c->argd(0);
	int moveType = 0;
	if (c->hasArg(1))
	{
		if (c->argc(1) == "low") moveType = 0;
		else if (c->argc(1) == "high") moveType = 1;
		else if (c->argc(1) == "both") moveType = 2;
		else Messenger::warn(QString("Move type for 'setDistances' unrecognised (%1) - defaulting to 'low'").arg(c->argc(1)));
	}
	bool nudge = c->hasArg(2) ? c->argb(2) : false;
	if (nudge && (moveType == 2)) target *= 0.5;

	printf("Nudge = %i\n", nudge);
	// Start undo state
	obj.rs()->beginUndoState("Set distances between atoms");

	// Loop over selected atoms
	Atom* i, *j, *order[2];
	Refitem<Bond,int>* bi;
	Bond* bij;
	Vec3<double> translationVector;
	double delta;
	for (Refitem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next)
	{
		i = ri->item;

		// Double loop over bonds on this atom
		for (bi = i->bonds(); bi != NULL; bi = bi->next)
		{
			bij = bi->item;
			j = bij->partner(i);
			if (!j->isSelected()) continue;
			if (i->id() < j->id()) continue;

			// Put atoms into the 'order' array, reflecting the atom indices and moveType specified
			if (((i->id() > j->id()) && (moveType == 1)) || ((i->id() < j->id()) && (moveType == 0))) order[0] = i;
			else order[0] = j;
			order[1] = order[0] == i ? j : i;
			printf("0 = %i %s (to be moved)\n", order[0]->id(), Elements().name(order[0]->element()));
			printf("1 = %i %s (to remain stationary)\n", order[1]->id(), Elements().name(order[1]->element()));

			// Have found a bond... check for a cyclic route between i and j
			// Clear any current marked selection
			obj.rs()->selectNone(true);

			// Perform mark-only tree select on atom in order[0], excluding the bond ji
			obj.rs()->selectTree(order[0], true, false, bij);

			// If atom in order[1] is now marked, there is a cyclic route connecting the two atoms and we can't proceed
			if (order[1]->isSelected(true))
			{
				Messenger::print("Can't alter the distance of two atoms i-j (%i-%i) where atoms 'i' and 'j' exist in the same cyclic moiety.", i->id()+1, j->id()+1);
				continue;
			}

			// Get distance delta, and define translation vector
			translationVector = obj.rs()->cell()->mimVector(order[1], order[0]);
			translationVector.print();
			delta = nudge ? target : target - translationVector.magnitude();
			if (moveType == 2) delta *= 0.5;
			printf("Target = %f, Delta = %f\n", target, delta);
			translationVector.normalise();
			translationVector *= delta;

			// Move current marked atoms (exactly which are marked depends on the moveType, but we can move the current selection regardless)
			obj.rs()->translateSelectionLocal(translationVector, true);
			if (moveType == 2)
			{
				obj.rs()->selectNone(true);
				obj.rs()->selectTree(order[1], true, false, bij);
				obj.rs()->translateSelectionLocal(-translationVector, true);
			}
		}
	}

	// End undostate
	obj.rs()->endUndoState();

	return true;
}

// Alter torsion between four specified atoms
bool Commands::function_SetTorsion(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.reset();
	Atom* i = c->argType(0) == VTypes::IntegerData ? obj.rs()->atom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	if (i == NULL)
	{
		Messenger::print("Atom 'i' given to 'settorsion' is NULL.");
		return false;
	}
	Atom* j = c->argType(1) == VTypes::IntegerData ? obj.rs()->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
	if (j == NULL)
	{
		Messenger::print("Atom 'j' given to 'settorsion' is NULL.");
		return false;
	}
	Atom* k = c->argType(2) == VTypes::IntegerData ? obj.rs()->atom(c->argi(2)-1) : (Atom*) c->argp(2, VTypes::AtomData);
	if (k == NULL)
	{
		Messenger::print("Atom 'k' given to 'settorsion' is NULL.");
		return false;
	}
	Atom* l = c->argType(3) == VTypes::IntegerData ? obj.rs()->atom(c->argi(3)-1) : (Atom*) c->argp(3, VTypes::AtomData);
	if (l == NULL)
	{
		Messenger::print("Atom 'l' given to 'settorsion' is NULL.");
		return false;
	}

	// Clear any current marked selection
	obj.rs()->selectNone(true);

	// Find bond (if any) between j and k
	Bond* b = j->findBond(k);

	// Perform mark-only tree select on atom l, excluding any bond to atom i
	obj.rs()->selectTree(l, true, false, b);

	// If atom 'i' is now marked, there is a cyclic route connecting the two atoms and we can't proceed
	if (i->isSelected(true))
	{
		Messenger::print("Can't alter the angle of four atoms i-j-k-l where 'i' or 'j' exists in the same cyclic moiety as 'k' or 'l', or are unbound and within the same fragment.");
		return false;
	}

	// Get current torsion between the four atoms
	double angle = obj.rs()->torsion(i,j,k,l);

	// Rotation vector will be vector k->j
	Vec3<double> v = obj.rs()->cell()->mimVector(k,j);
	v.normalise();
	double delta = c->argd(4) - angle;

	obj.rs()->beginUndoState("Set torsion between atoms");
	obj.rs()->rotateSelectionVector(j->r(), v, delta, true);
	obj.rs()->endUndoState();

	return true;
}

// Set torsions for all selected atoms
bool Commands::function_SetTorsions(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// ATEN2 TODO
}

// Translate current selection in local coordinates ('translate dx dy dz')
bool Commands::function_Translate(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vec3<double> tvec = c->arg3d(0);
	obj.rs()->beginUndoState("Translate Cartesian (%i atom(s), %f %f %f)", obj.rs()->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs()->translateSelectionLocal(tvec);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Translate activeatom ('translateatom <dx dy dz>')
bool Commands::function_TranslateAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	Vec3<double> tvec = c->arg3d(0);
	obj.rs()->beginUndoState("Translate Cartesian (atom %i, %f %f %f)", obj.i->id()+1, tvec.x, tvec.y, tvec.z);
	obj.rs()->translateAtom(obj.i, tvec);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Translate current selection in fractional cell coordinates ('translatecell dx dy dz')
bool Commands::function_TranslateCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vec3<double> tvec;
	tvec = obj.rs()->cell()->axes() * c->arg3d(0);
	obj.rs()->beginUndoState("Translate Cell (%i atom(s), %f %f %f)", obj.rs()->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs()->translateSelectionLocal(tvec);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Translate current selection in world coordinates ('translateworld dx dy dz')
bool Commands::function_TranslateWorld(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vec3<double> tvec;
	tvec = c->arg3d(0);
	obj.rs()->beginUndoState("Translate World (%i atom(s), %f %f %f)", obj.rs()->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs()->translateSelectionWorld(tvec);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}
