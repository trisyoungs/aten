/*
	*** Delaunay Mesh Generator
	*** src/methods/delaunay.h
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

#ifndef ATEN_DELAUNAY_H
#define ATEN_DELAUNAY_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "base/grid.h"

ATEN_USING_NAMESPACE

// Delaunay Edge
class DelaunayEdge : public ListItem<DelaunayEdge>
{
	public:
	// Constructors
	DelaunayEdge(GridPoint* p1, GridPoint* p2);

	private:
	// Reference counter
	int refCount_;
	// Gridpoints involved in the edge
	GridPoint* vertexA_, *vertexB_;

	public:
	// Return whether edge is between the supplied vertices (in either order)
	bool connectsVertices(GridPoint* p1, GridPoint* p2);
	// Return whether the edge uses the specified point
	bool usesPoint(GridPoint* gp);
	// Add reference to edge
	void addReference();
	// Remove reference from edge (returning true if no more references remain)
	bool removeReference();
	// Return current reference count
	int refCount();
	// Return specified vertex
	GridPoint* vertex(int n);
	// Print edge information
	void print();
};

// Delaunay Triangle
class DelaunayTriangle : public ListItem<DelaunayTriangle>
{
	public:
	// Constructor
	DelaunayTriangle(DelaunayEdge *edgea, DelaunayEdge *edgeb, DelaunayEdge *edgec);

	private:
	// Reference counter
	int refCount_;
	// Edges that make up the triangle
	DelaunayEdge *edges_[3];
	// Circumcentre of the triangle
	Vec3<double> circumCentre_;
	// Radius of the circumcircle
	double radius_;

	public:
	// Return the circumcentre of the triangle
	Vec3<double> circumCentre();
	// Return radius of the circumcircle
	double radius();
	// Return whether triangle has the supplied edges (in any order)
	bool hasEdges(DelaunayEdge *edgea, DelaunayEdge *edgeb, DelaunayEdge *edgec);
	// Return whether circumcircle contains supplied point
	bool containsPoint(Vec3<double> r);
	// Return whether triangle uses specified point
	bool usesPoint(GridPoint* point);
	// Return specified edge
	DelaunayEdge *edge(int n);
	// Return specified vertex
	GridPoint* vertex(int n);
	// Add reference to triangle
	void addReference();
	// Remove reference from triangle (returning true if no more references remain)
	bool removeReference();
	// Return current reference count
	int refCount();
	// Print triangle information
	void print();
};

// Delaunay Tetrahedron
class DelaunayTetrahedron : public ListItem<DelaunayTetrahedron>
{
	public:
	// Constructor
	DelaunayTetrahedron(DelaunayTriangle *t1, DelaunayTriangle *t2, DelaunayTriangle *t3, DelaunayTriangle *t4);

	private:
	// Edges that make up the triangle
	DelaunayTriangle *triangles_[4];
	// Circumcentre of the tetrahedron
	Vec3<double> circumCentre_;
	// Radius of the circumsphere
	double radius_;

	public:
	// Return the circumcentre of the triangle
	Vec3<double> circumCentre();
	// Return radius of the circumcircle
	double radius();
	// Return whether circumsphere contains supplied point
	bool containsPoint(Vec3<double> r);
	// Return whether tetrahedron uses specified point
	bool usesPoint(GridPoint* point);
	// Return whether tetrahedron has the supplied faces (in any order)
	bool hasFaces(DelaunayTriangle *t1, DelaunayTriangle *t2, DelaunayTriangle *t3, DelaunayTriangle *t4); 
	// Return specified triangle
	DelaunayTriangle *triangle(int n);
	// Return specified vertex
	GridPoint* vertex(int n);
	// Return vertex index (0-3) of specified GridPoint
	int vertexIndex(GridPoint* gp);
	// Print tetrahedron information
	void print();
};

// Delaunay surface
class DelaunaySurface
{
	public:
	// Constructor
	DelaunaySurface(Grid* g);

	/*
	 * Primitives
	 */
	private:
	// For bounding points defining enclosing tetrahedron
	GridPoint boundPoints_[4];
	// List of edges contained in the surface
	List<DelaunayEdge> edges_;
	// List of triangles making up the surface
	List<DelaunayTriangle> triangles_;
	// List of triangles making up the surface
	List<DelaunayTetrahedron> tetrahedra_;
	// Create Delaunay edge (or return existing edge)
	DelaunayEdge *createEdge(GridPoint* p1, GridPoint* p2);
	// Remove specified edge (but only delete if its reference count is zero)
	void removeEdge(DelaunayEdge *edge);
	// Create a triangle and add it to the list
	DelaunayTriangle *createTriangle(DelaunayEdge *v1, DelaunayEdge *v2, DelaunayEdge *v3);
	// Remove specified triangle and its edges (but only delete if reference counts are zero)
	void removeTriangle(DelaunayTriangle *tri);
	// Create a tetrahedron and add it to the list
	DelaunayTetrahedron *createTetrahedron(DelaunayTriangle *t1, DelaunayTriangle *t2, DelaunayTriangle *t3, DelaunayTriangle *t4);
	// Remove (delete) specified tetrahedron
	void removeTetrahedron(DelaunayTetrahedron *tet);
	// Remove all edges, triangles, and tetrahedra using specified point
	void removePoint(GridPoint* gp);

	public:
	// Return start of triangle list
	DelaunayTriangle *triangles();
	// Return start of tetrahedron list
	DelaunayTetrahedron *tetrahedra();


	/*
	 * Create
	 */
	private:
	// Create mesh from 2D data
	void create2DMesh(Grid* g);
	// Create 3D mesh
	void create3DMesh(Grid* g);
	// Add point in 3D
	void add3DPoint(GridPoint* gp);

	public:
	// Triangulize surface from supplied grid data
	void create(Grid* g);
};

#endif
