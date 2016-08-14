/*
	*** Delaunay Mesh Generator
	*** src/methods/delaunay.cpp
	Copyright T. Youngs 2007-2016

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

#include "methods/delaunay.h"

/*
 * Delaunay Edge
 */

// Constructor
DelaunayEdge::DelaunayEdge(GridPoint* p1, GridPoint* p2) : ListItem<DelaunayEdge>()
{
	// Private variables
	refCount_ = 0;
	vertexA_ = p1;
	vertexB_ = p2;
}

// Return whether edge contains the supplied points (in either order)
bool DelaunayEdge::connectsVertices(GridPoint* p1, GridPoint* p2)
{
	if ((p1 == vertexA_) && (p2 == vertexB_)) return true;
	if ((p2 == vertexA_) && (p1 == vertexB_)) return true;
	return false;
}

// Return whether the edge uses the specified point
bool DelaunayEdge::usesPoint(GridPoint* gp)
{
	if ((gp == vertexA_) || (gp == vertexB_)) return true;
	return false;
}

// Add reference to edge
void DelaunayEdge::addReference()
{
	refCount_ ++;
}

// Remove reference from edge, returning true if no more references exist
bool DelaunayEdge::removeReference()
{
	refCount_ --;
	return (refCount_ == 0);
}

// Return specified vertex
GridPoint* DelaunayEdge::vertex(int n)
{
	if (n == 0) return vertexA_;
	else if (n == 1) return vertexB_;
	else printf("Internal Error : Vertex index %i is out of bounds for DelaunayEdge.\n", n);
	return NULL;
}

// Print edge information
void DelaunayEdge::print()
{
	printf("Edge %p is between points %p (%f,%f,%f) and %p (%f,%f,%f), refcount = %i\n", this, vertexA_, vertexA_->r().x ,vertexA_->r().y, vertexA_->r().z, vertexB_, vertexB_->r().x, vertexB_->r().y, vertexB_->r().z, refCount_);
}

/*
 * Delaunay Triangle
 */

// Constructor
DelaunayTriangle::DelaunayTriangle(DelaunayEdge *edgea, DelaunayEdge *edgeb, DelaunayEdge *edgec) : ListItem<DelaunayTriangle>()
{
	// Private variables
	refCount_ = 0;
	edges_[0] = edgea;
	edges_[1] = edgeb;
	edges_[2] = edgec;
	for (int n=0; n<3; ++n)
	{
		if (edges_[n] == NULL) printf("Internal Error : DelaunayTriangle has been created with a NULL edge.\n");
		else edges_[n]->addReference();
	}
	// Get lengths of triangle sides and compute radius and centre
	Vec3<double> A, B, C;
	A = vertex(1)->r() - vertex(0)->r();
	B = vertex(2)->r() - vertex(1)->r();
	C = vertex(0)->r() - vertex(2)->r();
	double a = A.magnitude();
	double b = B.magnitude();
	double c = C.magnitude();
// 	radius_ = (2.0 * a * b * c ) / sqrt( (a+b+c)*(b+c-a)*(a-b+c)*(a+b-c));
	double s = (a + b + c) * 0.5;
	double K = sqrt(s*(s-a)*(s-b)*(s-c));
	radius_ = a*b*c / (4.0*K);
	circumCentre_ = (vertex(0)->r()+vertex(2)->r())*0.5 + (C*(A*B)) * (A.dp(B) / (8.0*K*K));
	if (radius_ < 0.0) radius_ = -radius_;
// 	circumCentre_ = vertex(0)->r() + num / (2.0 * ba.dp(ca*da));
// 	printf("Triangle circumcircle radius is %f, circumcentre is at %f %f %f\n", radius_, circumCentre_.x, circumCentre_.y, circumCentre_.z);
}

// Return the circumcentre of the triangle
Vec3<double> DelaunayTriangle::circumCentre()
{
	return circumCentre_;
}

// Return radius of the circumcircle
double DelaunayTriangle::radius()
{
	return radius_;
}

// Return whether triangle has the supplied edges (in any order)
bool DelaunayTriangle::hasEdges(DelaunayEdge *edgea, DelaunayEdge *edgeb, DelaunayEdge *edgec)
{
	int count = 0;
	for (int n=0; n<3; ++n) if ((edges_[n] == edgea) || (edges_[n] == edgeb) || (edges_[n] == edgec)) ++count;
	return (count == 3);
}

// Return whether circumsphere contains supplied point
bool DelaunayTriangle::containsPoint(Vec3<double> r)
{
	Vec3<double> delta = circumCentre_ - r;
	return (delta.magnitude() <= radius_);
}

// Return whether triangle uses specified point
bool DelaunayTriangle::usesPoint(GridPoint* gp)
{
	for (int n=0; n<3; ++n) if (edges_[n]->usesPoint(gp)) return true;
	return false;
}

// Return specified edge
DelaunayEdge *DelaunayTriangle::edge(int n)
{
	if ((n<0) || (n>2)) printf("Internal Error : Edge index %i is out of bounds for DelaunayTriangle.\n", n);
	else return edges_[n];
	return NULL;
}

// Return specified vertex
GridPoint* DelaunayTriangle::vertex(int n)
{
	if (n < 2) return edges_[0]->vertex(n);
	else
	{
		if ((edges_[1]->vertex(0) != edges_[0]->vertex(0)) && (edges_[1]->vertex(0) != edges_[0]->vertex(1))) return edges_[1]->vertex(0);
		else return edges_[1]->vertex(1);
	}
}

// Add reference to triangle
void DelaunayTriangle::addReference()
{
	refCount_ ++;
}

// Remove reference from triangle, returning true if no more references exist
bool DelaunayTriangle::removeReference()
{
	refCount_ --;
	return (refCount_ == 0);
}

// Return current reference count
int DelaunayTriangle::refCount()
{
	return refCount_;
}

// Print edge information
void DelaunayTriangle::print()
{
	printf("Triangle %p is between points %p (%f,%f,%f), %p (%f,%f,%f) and %p (%f,%f,%f), refcount = %i\n", this, vertex(0), vertex(0)->r().x ,vertex(0)->r().y, vertex(0)->r().z, vertex(1), vertex(1)->r().x, vertex(1)->r().y, vertex(1)->r().z, vertex(2), vertex(2)->r().x, vertex(2)->r().y, vertex(2)->r().z, refCount_);
}

/*
 * Delaunay Tetrahedron
 */

// Constructor
DelaunayTetrahedron::DelaunayTetrahedron(DelaunayTriangle *t1, DelaunayTriangle *t2, DelaunayTriangle *t3, DelaunayTriangle *t4) : ListItem<DelaunayTetrahedron>()
{
	// Private variables
	triangles_[0] = t1;
	triangles_[1] = t2;
	triangles_[2] = t3;
	triangles_[3] = t4;
	for (int n=0; n<4; ++n)
	{
		if (triangles_[n] == NULL) printf("Internal Error : DelaunayTetrahedron has been created with a NULL triangle.\n");
		else triangles_[n]->addReference();
	}
	// Compute circumsphere radius...
	Vec3<double> ba, ca, da, num;
	ba = vertex(1)->r() - vertex(0)->r();
	ca = vertex(2)->r() - vertex(0)->r();
	da = vertex(3)->r() - vertex(0)->r();
	double rba, rca, rda;
	rba = ba.magnitude();
	rca = ca.magnitude();
	rda = da.magnitude();
	// Compute circumcentre
	num = (ba*ca)*rda*rda + (da*ba)*rca*rca + (ca*da)*rba*rba;
	radius_ = num.magnitude() / (2.0 * ba.dp(ca*da)) / 2.0;
	if (radius_ < 0.0) radius_ = -radius_;
	circumCentre_ = vertex(0)->r() + num / (2.0 * ba.dp(ca*da));
}

// Return the circumcentre of the tetrahedron
Vec3<double> DelaunayTetrahedron::circumCentre()
{
	return circumCentre_;
}

// Return radius of the circumcircle
double DelaunayTetrahedron::radius()
{
	return radius_;
}

// Return whether circumsphere contains supplied point
bool DelaunayTetrahedron::containsPoint(Vec3<double> r)
{
	Vec3<double> delta = r - circumCentre_;
	return (delta.magnitude() <= radius_);
	Matrix reference, m;
	int n;
	bool result = true;
	for (n=0; n<4; ++n) reference.setColumn(n,vertex(n)->r(),1.0);	// TODO BROKEN Should the fourth element 'w' be 1.0?
	double det, refdet = reference.determinant();
	for (n=0; n<4; ++n)
	{
		m = reference;
		m.setColumn(n,r,1.0);	// Here also
		det = m.determinant();
		if ((det < 0) != (refdet < 0))
		{
			result = false;
			break;
		}
	}
	return result;
}

// Return whether tetrahedron uses specified point
bool DelaunayTetrahedron::usesPoint(GridPoint* gp)
{
	for (int n=0; n<4; ++n) if (triangles_[n]->usesPoint(gp)) return true;
	return false;
}

// Return whether tetrahedron has the supplied faces (in any order)
bool DelaunayTetrahedron::hasFaces(DelaunayTriangle *t1, DelaunayTriangle *t2, DelaunayTriangle *t3, DelaunayTriangle *t4)
{
	int count = 0;
	for (int n=0; n<4; ++n) if ((triangles_[n] == t1) || (triangles_[n] == t2) || (triangles_[n] == t3) || (triangles_[n] == t4)) ++count;
	return (count == 4);
}

// Return specified triangle
DelaunayTriangle *DelaunayTetrahedron::triangle(int n)
{
	if ((n<0) || (n>3)) printf("Internal Error : Triangle index %i is out of bounds for DelaunayTetrahedron.\n", n);
	else return triangles_[n];
	return NULL;
}

// Return specified vertex
GridPoint* DelaunayTetrahedron::vertex(int n)
{
	if (n < 3) return triangles_[0]->vertex(n);
	else for (int m=0; m<3; ++m)
	{
		int o;
		for (o=0; o<3; ++o) if (triangles_[1]->vertex(m) == triangles_[0]->vertex(o)) break;
		if (o == 3) return triangles_[1]->vertex(m);
	}
	printf("Internal Error : Failed to find vertex index %i for DelaunayTriangle.\n", n);
	return NULL;
}

// Return vertex index (0-3) of specified GridPoint
int DelaunayTetrahedron::vertexIndex(GridPoint* gp)
{
	for (int n=0; n<4; ++n) if (vertex(n) == gp) return n;
	return -1;
}

// Print edge information
void DelaunayTetrahedron::print()
{
	printf("Tetrahedron %p is between points %p (%f,%f,%f), %p (%f,%f,%f), %p (%f,%f,%f) and %p (%f,%f,%f)\n", this, vertex(0), vertex(0)->r().x ,vertex(0)->r().y, vertex(0)->r().z, vertex(1), vertex(1)->r().x, vertex(1)->r().y, vertex(1)->r().z, vertex(2), vertex(2)->r().x, vertex(2)->r().y, vertex(2)->r().z, vertex(3), vertex(3)->r().x, vertex(3)->r().y, vertex(3)->r().z);
}

/*
 * Delaunay Surface
 */

// Constructor
DelaunaySurface::DelaunaySurface(Grid* g)
{
	create(g);
}

DelaunayEdge *DelaunaySurface::createEdge(GridPoint* vertexA_, GridPoint* vertexB_)
{
	Messenger::enter("DelaunaySurface::createEdge");
	// Search for existing edge between these points
	DelaunayEdge *edge;
	for (edge = edges_.last(); edge != NULL; edge = edge->prev) if (edge->connectsVertices(vertexA_,vertexB_)) break;
	if (edge == NULL)
	{
		edge = new DelaunayEdge(vertexA_,vertexB_);
		edges_.own(edge);
	}
	Messenger::exit("DelaunaySurface::createEdge");
	return edge;
}

// Delete specified edge, and all other objects that contain it
void DelaunaySurface::removeEdge(DelaunayEdge *edge)
{
	Messenger::enter("DelaunaySurface::removeEdge");
	// If edge has no references, then just remove it, otherwise decrease reference count by one
	if (edge->removeReference()) edges_.remove(edge);
	Messenger::exit("DelaunaySurface::removeEdge");
}

// Remove all edges, triangles, and tetrahedra using specified point
void DelaunaySurface::removePoint(GridPoint* gp)
{
	DelaunayTetrahedron *tet = tetrahedra_.first(), *nexttet;
	while (tet != NULL)
	{
		nexttet = tet->next;
		if (tet->usesPoint(gp)) removeTetrahedron(tet);
		tet = nexttet;
	}
}
	
// Create a triangle and add it to the list
DelaunayTriangle *DelaunaySurface::createTriangle(DelaunayEdge *v1, DelaunayEdge *v2, DelaunayEdge *v3)
{
	Messenger::enter("DelaunaySurface::createTriangle");
	// Search for existing triangle comprising the three supplied edges
	DelaunayTriangle *tri;
	for (tri = triangles_.last(); tri != NULL; tri = tri->prev) if (tri->hasEdges(v1, v2, v3)) break;
	if (tri == NULL)
	{
		tri = new DelaunayTriangle(v1, v2, v3);
		triangles_.own(tri);
	}
	Messenger::exit("DelaunaySurface::createTriangle");
	return tri;
}

// Remove specified triangle, and all other objects that contain it
void DelaunaySurface::removeTriangle(DelaunayTriangle *tri)
{
	Messenger::enter("DelaunaySurface::removeTriangle");
	// If there are no current references to this triangle, just delete it
	if (tri->removeReference())
	{
		for (int n=0; n<3; ++n) removeEdge(tri->edge(n));
		triangles_.remove(tri);
	}
// 	else printf("Can't remove triangle - refs still remain.\n");
	Messenger::exit("DelaunaySurface::removeTriangle");
}

// Create a tetrahedron and add it to the list
DelaunayTetrahedron *DelaunaySurface::createTetrahedron(DelaunayTriangle *t1, DelaunayTriangle *t2, DelaunayTriangle *t3, DelaunayTriangle *t4)
{
	Messenger::enter("DelaunaySurface::createTetrahedron");
	// Search for existing tetrahedron?
	DelaunayTetrahedron *tet = NULL;
	for (tet = tetrahedra_.first(); tet != NULL; tet = tet->next) if (tet->hasFaces(t1,t2,t3,t4)) break;
	if (tet == NULL)
	{
		tet = new DelaunayTetrahedron(t1, t2, t3, t4);
		tetrahedra_.own(tet);
	}
	Messenger::exit("DelaunaySurface::createTetrahedron");
	return tet;
}

// Remove specified tetrahedron
void DelaunaySurface::removeTetrahedron(DelaunayTetrahedron *tet)
{
	Messenger::enter("DelaunaySurface::removeTetrahedron");
	// Cycle through triangle faces, removing each one
	for (int n=0; n<4; ++n) removeTriangle(tet->triangle(n));
	tetrahedra_.remove(tet);
	Messenger::exit("DelaunaySurface::removeTetrahedron");
}

// Return start of triangle list
DelaunayTriangle *DelaunaySurface::triangles()
{
	return triangles_.first();
}

// Return start of tetrahedron list
DelaunayTetrahedron *DelaunaySurface::tetrahedra()
{
	return tetrahedra_.first();
}

/*
 * Create
 */

// Create mesh from 2D data
void DelaunaySurface::create2DMesh(Grid* g)
{
}

// Create 3D mesh
void DelaunaySurface::create3DMesh(Grid* g)
{
	Messenger::enter("DelaunaySurface::create3DMesh");
	/* 
	First, create a regular tetrahedron large enough to contain the grid data.
	From the calculated LLC and URC we can determine a distance which corresponds to the furthest point away.
	We don't need to do this for regular grids, since we will work in integer gridpoint units which will later be
	sheared by the defined axes.
	*/
	if (g->type() == Grid::RegularXYZData)
	{
	}
	else if (g->type() == Grid::FreeXYZData)
	{
	}
	// LAZY test
	double r = ((g->upperRightCorner() - g->lowerLeftCorner()).magnitude()) * 1.10 * sqrt(24.0);
	boundPoints_[0].r().set(0.0,0.0,r);
	boundPoints_[1].r().setFromSpherical(r, 109.5, 0.0);
	boundPoints_[2].r().setFromSpherical(r, 109.5, 120.0);
	boundPoints_[3].r().setFromSpherical(r, 109.5, -120.0);
	DelaunayEdge *boundedges[6];
	boundedges[0] = createEdge(&boundPoints_[0], &boundPoints_[1]);
	boundedges[1] = createEdge(&boundPoints_[0], &boundPoints_[2]);
	boundedges[2] = createEdge(&boundPoints_[0], &boundPoints_[3]);
	boundedges[3] = createEdge(&boundPoints_[1], &boundPoints_[2]);
	boundedges[4] = createEdge(&boundPoints_[1], &boundPoints_[3]);
	boundedges[5] = createEdge(&boundPoints_[2], &boundPoints_[3]);
	DelaunayTriangle *boundtriangles[4];
	boundtriangles[0] = createTriangle(boundedges[0],boundedges[1],boundedges[3]);
	boundtriangles[1] = createTriangle(boundedges[0],boundedges[2],boundedges[4]);
	boundtriangles[2] = createTriangle(boundedges[1],boundedges[2],boundedges[5]);
	boundtriangles[3] = createTriangle(boundedges[3],boundedges[4],boundedges[5]);
	DelaunayTetrahedron *boundtetrahedron = createTetrahedron(boundtriangles[0],boundtriangles[1],boundtriangles[2],boundtriangles[3]);

	static int limit = 1;
	// Cycle through points
	if (g->type() == Grid::RegularXYZData)
	{
	}
	else if (g->type() == Grid::FreeXYZData)
	{
		int count = 0;
		for (GridPoint* gp = g->gridPoints(); gp != NULL; gp = gp->next) { add3DPoint(gp); printf("Added point %i\n", count++); if (count == limit) break;  }
// 		for (GridPoint* gp = g->gridPoints(); gp != NULL; gp = gp->next) add3DPoint(gp);
	}
	limit++;
	Messenger::print("Created mesh with %i triangles.", triangles_.nItems());
	printf("Created mesh with %i edges, %i triangles, and %i tetrahedra.\n", edges_.nItems(), triangles_.nItems(), tetrahedra_.nItems());
	// Remove original bounding tetrahedron
// 	for (int n=0; n < 4; ++n) removePoint(&boundPoints_[n]);
	printf("Pruned mesh contains %i edges, %i triangles, and %i tetrahedra.\n", edges_.nItems(), triangles_.nItems(), tetrahedra_.nItems());
// 	for (DelaunayTetrahedron *tet = tetrahedra_.first(); tet != NULL; tet = tet->next) tet->print();
	Messenger::exit("DelaunaySurface::create");
}

// Add point in 3D
void DelaunaySurface::add3DPoint(GridPoint* gp)
{
	Messenger::enter("DelaunaySurface::add3DPoint");
	// Create reflist of all existing tetrahedra that this point encroaches.
	// At the same time, create a reflist of the triangles involved, counting the number of overlapping tetrahedra that use it
	RefList<DelaunayTetrahedron,int> overlaps;
	RefList<DelaunayTriangle,int> triangles;
	RefListItem<DelaunayTriangle,int>* rtri;
	int n;
	DelaunayEdge *newedges[2], *edge;
	DelaunayTriangle *newtriangles[3];
	
	for (DelaunayTetrahedron *tet = tetrahedra_.first(); tet != NULL; tet = tet->next) if (tet->containsPoint(gp->r()))
	{
		overlaps.add(tet);
		for (n = 0; n<4; ++n)
		{
			rtri = triangles.addUnique(tet->triangle(n), 0);
			// Increase data item (will be our triangle's reference count)
			rtri->data ++;
			printf("Triangle %p has data refcount %i\n", rtri->item, rtri->data);
		}
	}
	// Did we find at least one?
	if (overlaps.nItems() == 0)
	{
		printf("Internal Error : Point {%f,%f,%f} isn't enclosed by any tetrahedron.\n", gp->r().x, gp->r().y, gp->r().z);
		Messenger::exit("DelaunaySurface::add3DPoint");
		return;
	}
	// Now, in the list of stored triangles, any whose actual refCount is greater than our stored refcount will still exist after the removal of the tetrahedra in the list. So, with these triangles, create new tetrahedra with our current point
	for (rtri = triangles.first(); rtri != NULL; rtri = rtri->next)
	{
		if (rtri->data != rtri->item->refCount()) continue;

		// Create new edges and triangles for the new tetrahedron
		for (n=0; n<3; ++n)
		{
			// For each edge 'n' in the current triangle, create two new edges and one new triangle with our current point
			edge = rtri->item->edge(n);
			newedges[0] = createEdge(gp, edge->vertex(0));
			newedges[1] = createEdge(gp, edge->vertex(1));
			newtriangles[n] = createTriangle(edge, newedges[0], newedges[1]);
		}
		// Create new tetrahedron with these new triangles...
		createTetrahedron(rtri->item, newtriangles[0], newtriangles[1], newtriangles[2]);
	}
	// Finally, delete all tetrahedra in the reflist
	for (RefListItem<DelaunayTetrahedron,int>* rtet = overlaps.first(); rtet != NULL; rtet = rtet->next) removeTetrahedron(rtet->item);
	Messenger::exit("DelaunaySurface::add3DPoint");
}

// Triangulize surface from supplied grid data
void DelaunaySurface::create(Grid* g)
{
	Messenger::enter("DelaunaySurface::create");
	// Select creation routine based on grid data type
	switch (g->type())
	{
		case (Grid::RegularXYData):
			create2DMesh(g);
			break;
		case (Grid::RegularXYZData):
		case (Grid::FreeXYZData):
			create3DMesh(g);
			break;
    default:
      break;  
	}
	Messenger::exit("DelaunaySurface::create");
}
