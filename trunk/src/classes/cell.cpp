/*
	*** Periodic cell definition
	*** src/classes/cell.cpp
	Copyright T. Youngs 2007

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

#include "classes/cell.h"
#include "classes/atom.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include <math.h>

// Cell types
const char *CT_strings[CT_NITEMS] = { "None", "Cubic", "Orthorhombic", "Parallelepiped" };
const char *text_from_CT(cell_type i)
	{ return CT_strings[i]; }
cell_type CT_from_text(const char *s)
	{ return (cell_type) enum_search("cell type",CT_NITEMS,CT_strings,s); }
const char **get_CT_strings()
	{ return CT_strings; }

// Constructor
unitcell::unitcell()
{
	type = CT_NONE;
	axes_t.zero();
	inverse.zero();
	recip.zero();
	lengths.zero();
	angles.zero();
	origin.zero();
	volume = 0.0;
	rvolume = 0.0;
	#ifdef MEMDEBUG
		memdbg.create[MD_UNITCELL] ++;
	#endif
}

// Destructor
unitcell::~unitcell()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_UNITCELL] ++;
	#endif
}

/*
// Set
*/

// Determine Type
void unitcell::determine_type()
{
	dbg_begin(DM_CALLS,"unitcell::determine_type");
	// Compare cell angles....
	double ab, bc, ac;
	int count = 0;
	if (fabs(90.0 - angles.x) < 1.0e-5) count ++;
	if (fabs(90.0 - angles.y) < 1.0e-5) count ++;
	if (fabs(90.0 - angles.z) < 1.0e-5) count ++;
	// If all sides are orthogonal then either cubic or orthorhombic (2 == monoclinic, 0 == triclinic)
	if (count == 3)
	{
		// Must check lengths as well
		count = 0;
		if (fabs(lengths.x - lengths.y) < 1.0e-5) count ++;
		if (fabs(lengths.x - lengths.z) < 1.0e-5) count ++;
		if (count == 2) type = CT_CUBIC;
		else type = CT_ORTHORHOMBIC;
	}
	else type = CT_PARALLELEPIPED;
	dbg_end(DM_CALLS,"unitcell::determine_type");
}

// Set (by parameters)
void unitcell::set(const vec3<double> &newlengths, const vec3<double> &newangles)
{
	dbg_begin(DM_CALLS,"unitcell::set[parameters]");
	double temp;
	// Store cell lengths and angles (in degrees) in structure
	angles = newangles;
	lengths = newlengths;
	// Work in unit vectors. Assume that A lays along x-axis
	axes_t.set(0,1.0,0.0,0.0);
	// Assume that B lays in the xy plane. Since A={1,0,0}, cos(gamma) equals 'x' of the B vector.
	temp = cos(angles.z/DEGRAD);
	axes_t.set(1,temp,sqrt(1.0 - temp*temp),0.0);
	// The C vector can now be determined in parts.
	// It's x-component is equal to cos(beta) since {1,0,0}{x,y,z} = {1}{x} = cos(beta)
	axes_t.set(2,cos(angles.y/DEGRAD),0.0,0.0);
	// The y-component can be determined by completing the dot product between the B and C vectors
	axes_t.rows[2].y = ( cos(angles.x/DEGRAD) - axes_t.rows[1].x*axes_t.rows[2].x ) / axes_t.rows[1].y;
	// The z-component is simply the remainder of the unit vector...
	axes_t.rows[2].z = sqrt(1.0 - axes_t.rows[2].x*axes_t.rows[2].x - axes_t.rows[2].y*axes_t.rows[2].y);
	// Lastly, adjust these unit vectors to give the proper cell lengths
	axes_t.rows[0] *= lengths.x;
	axes_t.rows[1] *= lengths.y;
	axes_t.rows[2] *= lengths.z;
	axes_t = axes_t.transpose();
	// Determine type of cell
	determine_type();
	// Calculate the cell volume
	volume = axes_t.determinant();
	calc_origin();
	calc_inverse();
	calc_reciprocal();
	dbg_end(DM_CALLS,"unitcell::set[parameters]");
}

// Set (by matrix)
void unitcell::set(const mat3<double> &newaxes)
{
	dbg_begin(DM_CALLS,"unitcell::set[matrix]");
	// Store the supplied matrix and get transpose for vector calculation
	axes_t = newaxes;
	mat3<double> normaxes = newaxes.transpose();
	// Calculate cell lengths
	lengths.x = normaxes.rows[0].magnitude();
	lengths.y = normaxes.rows[1].magnitude();
	lengths.z = normaxes.rows[2].magnitude();
	// Calculate cell angles
	vec3<double> vecx,vecy,vecz;
	vecx = normaxes.rows[0];
	vecy = normaxes.rows[1];
	vecz = normaxes.rows[2];
	vecx.normalise();
	vecy.normalise();
	vecz.normalise();
	angles.x = acos(vecy.dp(vecz));
	angles.y = acos(vecx.dp(vecz));
	angles.z = acos(vecx.dp(vecy));
	angles *= DEGRAD;
	// Determine type of cell
	determine_type();
	// Calculate the cell volume   TODO Other cell types?
	volume = axes_t.determinant();
	calc_origin();
	calc_inverse();
	calc_reciprocal();
	dbg_end(DM_CALLS,"unitcell::set[matrix]");
}

// Calculate reciprocal cell vectors
void unitcell::calc_reciprocal()
{
	// Calculate the reciprocal cell of 'this->cell'
	dbg_begin(DM_CALLS,"unitcell::calc_reciprocal");
	switch (type)
	{
		case (CT_NONE):
			msg(DM_NONE,"unitcell : Can't calculate reciprocal cell - no cell defined.\n");
			break;
		case (CT_CUBIC):
		case (CT_ORTHORHOMBIC):
			recip.rows[0].set(TWOPI / axes_t.rows[0].x, 0.0, 0.0);
			recip.rows[1].set(0.0, TWOPI / axes_t.rows[1].y, 0.0);
			recip.rows[2].set(0.0, 0.0, TWOPI / axes_t.rows[2].z);
			rvolume = TWOPI / (axes_t.rows[0].x * axes_t.rows[1].y * axes_t.rows[2].z);
			break;
		case (CT_PARALLELEPIPED):
			// Reciprocal cell vectors are perpendicular to normal cell axes_t.
			// Calculate from cross products of normal cell triples
			recip.rows[0] = axes_t.rows[1] * axes_t.rows[2];
			recip.rows[1] = axes_t.rows[0] * axes_t.rows[2];
			recip.rows[2] = axes_t.rows[0] * axes_t.rows[1];
			rvolume = fabs( axes_t.rows[0].x*recip.rows[0].x + axes_t.rows[1].y*recip.rows[1].y + axes_t.rows[2].z*recip.rows[2].z);
			recip.rows[0] = recip.rows[0] * TWOPI / rvolume;
			recip.rows[1] = recip.rows[1] * TWOPI / rvolume;
			recip.rows[2] = recip.rows[2] * TWOPI / rvolume;
			break;
	}
	dbg_end(DM_CALLS,"unitcell::calc_reciprocal");
}

// Calculate origin of coordinate system
void unitcell::calc_origin()
{
	dbg_begin(DM_CALLS,"unitcell::calc_origin");
	if (type != CT_NONE)
	{
		origin.set(-0.5,-0.5,-0.5);
		origin *= axes_t;
	}
	else origin.set(0.0,0.0,0.0);
	dbg_end(DM_CALLS,"unitcell::calc_origin");
}

// Calculate inverse matrix of cell vectors
void unitcell::calc_inverse()
{
	dbg_begin(DM_CALLS,"unitcell::calc_inverse");
	inverse = axes_t;
	inverse.invert();
	dbg_end(DM_CALLS,"unitcell::calc_inverse");
}

/*
// Minimum Image Routines
*/

// Minimum image position
vec3<double> unitcell::mim(const vec3<double> &r1, const vec3<double> &r2) const
{
	// Returns the minimum image coordinates of r1 with respect to r2.
	// Since we work in fractional cell coordinates, can do simple integer subtraction for cubic and pseudo-cubic boundary conditions
	static vec3<double> R;
	switch (type)
	{
		// No cell - just return r1
		case (CT_NONE):
			R = r1;
			break;
		// Remaining boundary conditions are all cubic or pseudo-cubic, so can be grouped together...
		default:
			R = r1 - r2;
			R *= inverse;
			// TODO Test speed of 'int' version
			if (R.x < -0.5) R.x += 1.0;
			if (R.y < -0.5) R.y += 1.0;
			if (R.z < -0.5) R.z += 1.0;
			if (R.x > 0.5) R.x -= 1.0;
			if (R.y > 0.5) R.y -= 1.0;
			if (R.z > 0.5) R.z -= 1.0;
			// R.x -= int(R.x);
			// R.y -= int(R.y);
			// R.z -= int(R.z);
			R *= axes_t;
			R += r2;
	}
	return R;
}

// Minimum image vector
vec3<double> unitcell::mimd(const vec3<double> &r1, const vec3<double> &r2) const
{
	static vec3<double> R;
	R = mim(r1,r2);
	R -= r2;
	return R;
}

// Minimimum image routines with atom and vector pointers
vec3<double> unitcell::mim(atom *i, const vec3<double> &r2) const
{
	return mim(i->r,r2);
}

vec3<double> unitcell::mimd(atom *i, const vec3<double> &r2) const
{
	return mimd(i->r,r2);
}

// Minimimum image routines with atom pointers
vec3<double> unitcell::mim(atom *i, atom *j) const
{
	return mim(i->r,j->r);
}

vec3<double> unitcell::mimd(atom *i, atom *j) const
{
	return mimd(i->r,j->r);
}

// Fold atom
void unitcell::fold(vec3<double> &r) const
{
	// Folds the coordinates in 'r' into the defined unit cell
	dbg_begin(DM_MORECALLS,"unitcell::fold");
	static vec3<double> newr;
	switch (type)
	{
		// No cell, so no image to fold into
		case (CT_NONE):
			break;
		// Remaining cell types are cubic or pseudo-cubic, so subtract integer part of position
		default:
			newr = r;
			// Convert these coordinates into fractional cell coordinates...
			newr *= inverse;
			if (newr.x < 0.0) newr.x += 1.0;
			else if (newr.x >= 1.0) newr.x -= 1.0;
			if (newr.y < 0.0) newr.y += 1.0;
			else if (newr.y >= 1.0) newr.y -= 1.0;
			if (newr.z < 0.0) newr.z += 1.0;
			else if (newr.z >= 1.0) newr.z -= 1.0;
			//newr.x -= floor(newr.x);
			//newr.y -= floor(newr.y);
			//newr.z -= floor(newr.z);
			// Convert back into world coordinates
			r = newr * axes_t;
			break;
	}
	dbg_end(DM_MORECALLS,"unitcell::fold");
}

void unitcell::fold(atom *i) const
{
	fold(i->r);
}

/*
// Geometry Calculation
*/

double unitcell::distance(const vec3<double> &r1, const vec3<double> &r2) const
{
	// Calculate the distance between atoms i and j
	dbg_begin(DM_MORECALLS,"unitcell::distance");
	static vec3<double> mimi;
	mimi = mimd(r1,r2);
	dbg_end(DM_MORECALLS,"unitcell::distance");
	return mimi.magnitude();
}

double unitcell::distance(atom *i, atom *j) const
{
	return distance(i->r,j->r);
}

double unitcell::angle(const vec3<double> &r1, const vec3<double> &r2, const vec3<double> &r3) const
{
	// Calculate the angle formed between atoms i, j, and k
	// Result is returned in radians.
	dbg_begin(DM_MORECALLS,"unitcell::angle");
	static vec3<double> vecij, veckj;
	static double dp, a;
	vecij = mimd(r1,r2);
	veckj = mimd(r3,r2);
	// Normalise vectors and calculate dot product and angle.
	vecij.normalise();
	veckj.normalise();
	dp = vecij.dp(veckj);
	a = acos(dp);
	dbg_end(DM_MORECALLS,"unitcell::angle");
	return a;
}

double unitcell::angle(atom *i, atom *j, atom *k) const
{
	return angle(i->r,j->r,k->r);
}

double unitcell::torsion(const vec3<double> &i, const vec3<double> &j, const vec3<double> &k, const vec3<double> &l) const
{
	// Calculate the torsion angle formed between the atoms i, j, k, and l.
	// Return result is in radians.
	dbg_begin(DM_MORECALLS,"unitcell::calc_torsion");
	static vec3<double> vecji, veckl, vecjk, veckj, mim_k, xpj, xpk;
	static double dp, angle;
	// Vector j->i (minimum image of i w.r.t. j)
	vecji = mimd(i,j);
	// Vectors j->k and k->j (minimum image of k w.r.t. j)
	mim_k = mim(k,j);
	vecjk = mim_k - j;
	veckj = -vecjk;
	// Vector k->l (minimum image of l w.r.t. k (in turn w.r.t. j))
	veckl = mimd(mim_k,l);
	// Calculate cross products
	xpj = vecjk * vecji;
	xpj.normalise();
	xpk = veckj * veckl;
	xpk.normalise();
	dp = xpj.dp(xpk);
	angle = acos(dp);
	// Calculate sign of torsion
	dp = xpj.dp(veckl);
	if (dp > 0) angle = -angle;
	dbg_end(DM_MORECALLS,"unitcell::calc_torsion");
	return angle;
}

double unitcell::torsion(atom *i, atom *j, atom *k, atom *l) const
{
	return torsion(i->r,j->r,k->r,l->r);
}

/*
// Coordinate Transforms
*/

// Return the fractional coordinates of the specified position
vec3<double> unitcell::real_to_frac(const vec3<double> &v) const
{
	// Convert the real coordinates supplied into fractional cell coordinates
	return (v * inverse);
}

// Return the real coordinates of the specified fractional cell coordinate
vec3<double> unitcell::frac_to_real(const vec3<double> &v) const
{
	// Convert the real coordinates supplied into fractional cell coordinates
	//vec3<double> temp = v * axes;
	return (v * axes_t);
}

/*
// Misc
*/

// Generate a random position inside the unit cell
vec3<double> unitcell::random_pos() const
{
	// Multiply some random fractional cell coordinates by the unit cell axes
	static vec3<double> result;
	result.x = cs_random();
	result.y = cs_random();
	result.z = cs_random();
	result *= axes_t;
	return result;
}

// Print
void unitcell::print() const
{
	msg(DM_NONE,"\t  [    A        B        C    ]\n");
	msg(DM_NONE,"\tx <%8.4f %8.4f %8.4f > [alpha=%8.3f]\n", axes_t.rows[0].x, axes_t.rows[0].y, axes_t.rows[0].z, angles.x);
	msg(DM_NONE,"\ty <%8.4f %8.4f %8.4f > [ beta=%8.3f]\n", axes_t.rows[1].x, axes_t.rows[1].y, axes_t.rows[1].z, angles.y);
	msg(DM_NONE,"\tz <%8.4f %8.4f %8.4f > [gamma=%8.3f]\n", axes_t.rows[2].x, axes_t.rows[2].y, axes_t.rows[2].z, angles.z);
	msg(DM_NONE,"\tl [%8.4f %8.4f %8.4f ]\n", lengths.x, lengths.y, lengths.z);
}
