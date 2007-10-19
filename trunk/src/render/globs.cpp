/*
	*** OpenGL objects
	*** src/render/globs.cpp
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

#include "render/globs.h"
#include "base/prefs.h"
#include "base/master.h"
#include "gui/gui.h"

// Initialise
void gl_objects::initialise()
{
	// Generate quadric objects for subsequent use
	quadric1 = gluNewQuadric();			// Creates a quadric object for use.
	quadric2 = gluNewQuadric();			// Creates a quadric object for use.
	gluQuadricDrawStyle(quadric1, GLU_FILL);	// Set drawing style of the quadric to solid.
	gluQuadricNormals(quadric1, GL_SMOOTH);		// Set up normals for shading.
	gluQuadricTexture(quadric1, GL_FALSE);		// Turn off texturing.
	gluQuadricDrawStyle(quadric2, GLU_FILL);	// Set drawing style of the quadric to solid.
	gluQuadricNormals(quadric2, GL_SMOOTH);		// Set up normals for shading.
	gluQuadricTexture(quadric2, GL_FALSE);		// Turn off texturing.
	// Create display lists for globs
	lists[GLOB_STICKATOM] = glGenLists(GLOB_NITEMS);
	for (int n=1; n<GLOB_NITEMS; n++) lists[n] = lists[GLOB_STICKATOM]+n;
	// Create objects for the first time
	create_all();
}

// Create dynamic globs
void gl_objects::generate_dynamic()
{
	// Create display lists for (potentially) constantly changing objects
	dbg_begin(DM_CALLS,"gl_objects:generate_dynamic");
	int n,m, atomdetail;
	float delta, tickdelta, tickheight, ticktop, tickbottom;
	// Grab some oft-used values
	atomdetail = prefs.render_atom_detail;
	/*
	// Selected Atoms
	*/
	gluQuadricDrawStyle(quadric1, GLU_FILL);      // Set drawing style of the quadric to solid.
	// Enlarged sphere (for selections with DS_TUBE)
	glNewList(lists[GLOB_SELTUBEATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_tube_size*prefs.render_selection_scale_current,atomdetail,atomdetail*2);
	glEndList();
	// Enlarged sphere (for selections with DS_SPHERE)
	glNewList(lists[GLOB_SELSPHEREATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_SPHERE]*prefs.render_selection_scale_current,atomdetail,atomdetail*2);
	glEndList();
	// Enlarged sphere (for selections with DS_SCALED)
	glNewList(lists[GLOB_SELUNITATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_selection_scale_current,atomdetail,atomdetail*2);
	glEndList();
	dbg_end(DM_CALLS,"gl_objects::generate_dynamic");
}

void gl_objects::generate_static()
{
	dbg_begin(DM_CALLS,"gl_objects::generate_static");
	int n, m, atomdetail, extent, ticks;
	float delta, tickdelta, tickheight, ticktop, tickbottom, spacing;
	// Grab some oft-used values
	atomdetail = prefs.render_atom_detail;
	spacing = prefs.build_guide_spacing;
	extent = prefs.build_guide_extent;
	ticks = prefs.build_guide_ticks;
	/*
	// Atoms
	*/
	// Stick Atom (for DS_STICK)
	glNewList(lists[GLOB_STICKATOM],GL_COMPILE);
	  glBegin(GL_LINES);
	    glVertex3f(-0.5f,0.0f,0.0f); glVertex3f(0.5f,0.0f,0.0f);
	    glVertex3f(0.0f,-0.5f,0.0f); glVertex3f(0.0f,0.5f,0.0f);
	    glVertex3f(0.0f,0.0f,-0.5f); glVertex3f(0.0f,0.0f,0.5f);
	  glEnd();
	glEndList();
	gluQuadricDrawStyle(quadric1,GLU_FILL);      // Set drawing style of the quadric to solid.
	// Atom Sphere (for DS_TUBE)
	glNewList(lists[GLOB_TUBEATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_TUBE],atomdetail*2,atomdetail);
	glEndList();
	// Atom Sphere (for DS_SPHERE)
	glNewList(lists[GLOB_SPHEREATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_SPHERE],atomdetail*2,atomdetail);
	  //sphere(10,10,prefs.render_atom_size[DS_SPHERE]);
	gluQuadricDrawStyle(quadric1,GLU_FILL);      // Set drawing style of the quadric to solid.
	glEndList();
	// Unit Atom Sphere (for DS_SCALED)
	glNewList(lists[GLOB_UNITATOM],GL_COMPILE);
	  gluSphere(quadric1,1.0,atomdetail*2,atomdetail);
	glEndList();
	gluQuadricDrawStyle(quadric1,GLU_LINE);      // Set drawing style of the quadric to solid.
	// Wire Atom Sphere (for DS_TUBE)
	glNewList(lists[GLOB_WIRETUBEATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_tube_size*1.1,atomdetail*2,atomdetail);
	glEndList();
	// Wire Atom Sphere (for DS_SPHERE)
	glNewList(lists[GLOB_WIRESPHEREATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_SPHERE]*1.1,atomdetail*2,atomdetail);
	glEndList();
	// Wire Unit Atom Sphere (for DS_SCALED)
	glNewList(lists[GLOB_WIREUNITATOM],GL_COMPILE);
	  gluSphere(quadric1,1.1,atomdetail*2,atomdetail);
	glEndList();
	/*
	// Bonds
	*/
	// Bond half
	gluQuadricDrawStyle(quadric1,GLU_FILL);
	glNewList(lists[GLOB_BOND],GL_COMPILE);
	  gluCylinder(quadric2,prefs.render_tube_size,prefs.render_tube_size,1.0f,prefs.render_bond_detail,prefs.render_bond_detail);
	glEndList();
	// Wireframe bond half
	gluQuadricDrawStyle(quadric1,GLU_LINE);
	glNewList(lists[GLOB_WIREBOND],GL_COMPILE);
	  gluCylinder(quadric2,prefs.render_tube_size*1.25,prefs.render_tube_size*1.25,1.0f,prefs.render_bond_detail,prefs.render_bond_detail);
	glEndList();
	/*
	// Others
	*/
	// View axes
	glNewList(lists[GLOB_GLOBE],GL_COMPILE);
	  glBegin(GL_LINES);
	    // X
	    glVertex3f(0.6f,0.0f,0.0f); glVertex3f(0.0f,0.0f,0.0f);
	    glVertex3f(0.65f,-0.05f,0.0f); glVertex3f(0.85f,0.05f,0.0f);
	    glVertex3f(0.65f,0.05f,0.0f); glVertex3f(0.85f,-0.05f,0.0f);
	    // Y
	    glVertex3f(0.0f,0.6f,0.0f); glVertex3f(0.0f,0.0f,0.0f);
	    glVertex3f(0.0f,0.65f,0.0f); glVertex3f(0.0f,0.75f,0.0f);
	    glVertex3f(0.0f,0.75f,0.0f); glVertex3f(0.05f,0.85f,0.0f);
	    glVertex3f(0.0f,0.75f,0.0f); glVertex3f(-0.05f,0.85f,0.0f);
	    // Z
	    glVertex3f(0.0f,0.0f,0.6f); glVertex3f(0.0f,0.0f,0.0f);
	    glVertex3f(-0.05f,0.0f,0.65f); glVertex3f(0.05f,0.0f,0.65f);
	    glVertex3f(0.05f,0.0f,0.65f); glVertex3f(-0.05f,0.0f,0.85f);
	    glVertex3f(-0.05f,0.0f,0.85f); glVertex3f(0.05f,0.0f,0.85f);
	  glEnd();
	  gluQuadricDrawStyle(quadric1,GLU_LINE);
	  gluSphere(quadric1,0.5,10,20);
	glEndList();
	// Drawing guide
	delta = extent * spacing;
	tickdelta = spacing / ticks;
	tickheight = spacing * 0.05;
	glNewList(lists[GLOB_GUIDE],GL_COMPILE);
	  glBegin(GL_LINES);
	    for (n=-extent; n<=extent; n++)
	    {
		// Horizontal gridlines
	  	glVertex3f(-delta,spacing*n,0.0f); glVertex3f(delta,spacing*n,0.0f);
		// Vertical gridlines
	  	glVertex3f(spacing*n,-delta,0.0f); glVertex3f(spacing*n,delta,0.0f);
		// Tick marks
		n == -extent ? tickbottom = spacing*n : tickbottom = spacing*n-tickheight;
		n == extent ? ticktop = spacing*n : ticktop = spacing*n+tickheight;
		for (m=0; m<ticks*extent*2; m++)
			if (m % ticks != 0)
			{
				// Ticks on horizontal gridlines
				glVertex3f(-delta+m*tickdelta,ticktop,0.0f);
				glVertex3f(-delta+m*tickdelta,tickbottom,0.0f);
				// Ticks on vertical gridlines
				glVertex3f(ticktop,-delta+m*tickdelta,0.0f);
				glVertex3f(tickbottom,-delta+m*tickdelta,0.0f);
			}
	    }
	  glEnd();
	glEndList();
	// Unit Circle
	int nsegs = 36;
	glNewList(lists[GLOB_CIRCLE],GL_COMPILE);
	  glBegin(GL_LINE_LOOP);
	    for (int i=0; i < nsegs; i++)
	    {
		float degInRad = i*(360.0/nsegs)/DEGRAD;
		glVertex2f(cos(degInRad),sin(degInRad));
	    }
	  glEnd();
	glEndList();
	// Unit Wire Cube (centred at origin)
	glNewList(lists[GLOB_WIREUNITCUBE],GL_COMPILE);
	  glBegin(GL_LINE_LOOP);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	  glEnd();
	  glBegin(GL_LINES);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(0.5,0.5,0.5);
	  glEnd();
	glEndList();
	// Unit Solid Cube (centred at origin)
	glNewList(lists[GLOB_UNITCUBE],GL_COMPILE);
	  glBegin(GL_QUADS);
	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(-0.5,0.5,0.5);

	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(0.5,0.5,-0.5);

	    glVertex3d(-0.5,-0.5,-0.5);
	    glVertex3d(-0.5,0.5,-0.5);
	    glVertex3d(-0.5,0.5,0.5);
	    glVertex3d(-0.5,-0.5,0.5);
	    glVertex3d(0.5,-0.5,-0.5);
	    glVertex3d(0.5,0.5,-0.5);
	    glVertex3d(0.5,0.5,0.5);
	    glVertex3d(0.5,-0.5,0.5);
	  glEnd();
	glEndList();
	// Cell Axis Arrows
	glNewList(lists[GLOB_CELLAXES],GL_COMPILE);
	  double asize = 0.5, awidth = 0.2, posoffset = 0.5;
	  for (int i=0; i<3; i++)
	  {
		glPushMatrix();
		  if (i == 1) glRotated(-90.0,0.0,1.0,0.0);
		  else if (i == 2) glRotated(90.0,0.0,0.0,1.0);
		  glPushMatrix();
		    glScaled(0.5,awidth,awidth);
		    glTranslated(0.5,0.0,0.0);
		    glCallList(lists[GLOB_UNITCUBE]);
		  glPopMatrix();
		  glTranslated(posoffset,0.0,0.0);
		  glBegin(GL_TRIANGLE_FAN);
		    glVertex3d(asize,0.0,0.0);
		    glVertex3d(0.0,awidth,awidth);
		    glVertex3d(0.0,awidth,-awidth);
		    glVertex3d(0.0,-awidth,-awidth);
		    glVertex3d(0.0,-awidth,awidth);
		    glVertex3d(0.0,awidth,awidth);
		  glEnd();
		glPopMatrix();
	  }
	glEndList();
	dbg_end(DM_CALLS,"gl_objects::generate_static");
}

void gl_objects::sphere(int slices, int stacks, double radius)
{
	// Don't use this to render objects to the view - create a display list first!
	double x[2][slices],y[2][slices],z[2],r,rad;
	double zstep = 2.0 * radius / stacks;
	double delta = 0.01;
	int n, m, row1 = 0, row2 = 1, t;
	z[row1] = -radius;
	// Initialise all elements of first array to 0.0
	for (n=0; n<slices; n++)
	{
		x[row1][n] = 0.0;
		y[row1][n] = 0.0;
	}
	glBegin(GL_QUADS);
	  for (n=1; n<=stacks; n++)
	  {
		z[row2] = z[row1] + zstep;
		rad = cos((z[row2]/radius) * (PI/2.0) + (PI/2));
	//	z[row2] = rad * radius;
		// Fill the x and y array 'row2' with the coordinates of the circle
		//r = sqrt(1 - (z[row2]/radius) * (z[row2]/radius));
		r = sqrt(1 - rad*rad);
		printf("sin=%9.4f %9.4f\n",rad,r);
		for (m=0; m<slices; m++)
		{
			rad = m*(TWOPI/slices);
			x[row2][m] = cos(rad) * r * radius;
			y[row2][m] = sin(rad) * r * radius;
		}
		// Draw QUADS between points...
		for (m=1; m<slices; m++)
		{
			glVertex3d(x[row2][m],y[row2][m],z[row2]);
			glVertex3d(x[row2][m-1],y[row2][m-1],z[row2]);
			glVertex3d(x[row1][m-1],y[row1][m-1],z[row1]);
			glVertex3d(x[row1][m],y[row1][m],z[row1]);
		}
		// Final set of quads - between first and last points
		glVertex3d(x[row1][slices-1],y[row1][slices-1],z[row1]);
		glVertex3d(x[row1][0],y[row1][0],z[row1]);
		glVertex3d(x[row2][0],y[row2][0],z[row2]);
		glVertex3d(x[row2][slices-1],y[row2][slices-1],z[row2]);
		// Swap rows
		t = row2;
		row2 = row1;
		row1 = t;
	  }
	glEnd();
}

void gl_objects::clear_static()
{
	// Delete all display lists
	dbg_begin(DM_CALLS,"gl_objects::delete_static");
	glDeleteLists(lists[GLOB_STICKATOM],GLOB_CELLAXES);
	dbg_end(DM_CALLS,"gl_objects::delete_static");
}

void gl_objects::clear_dynamic()
{
	// Delete dynamic object display lists
	dbg_begin(DM_CALLS,"gl_objects::delete_dynamic");
	glDeleteLists(lists[GLOB_SELTUBEATOM],3);
	dbg_end(DM_CALLS,"gl_objects::delete_dynamic");
}

/*
// Public Routines
*/

// Create all globs
void gl_objects::create_all()
{
	dbg_begin(DM_CALLS,"gl_objects::create_all");
	if (gui.mainview.begin_gl())
	{
		generate_static();
		generate_dynamic();
		gui.mainview.end_gl();
	}
	dbg_end(DM_CALLS,"gl_objects::create_all");
}

// Delete all globs
void gl_objects::delete_all()
{
	dbg_begin(DM_CALLS,"gl_objects::delete_all");
	if (gui.mainview.begin_gl())
	{
		clear_static();
		clear_dynamic();
		gui.mainview.end_gl();
	}
	dbg_end(DM_CALLS,"gl_objects::delete_all");
}

// Recreate all globs
void gl_objects::recreate_all()
{
	dbg_begin(DM_CALLS,"gl_objects::recreate_all");
	if (gui.mainview.begin_gl())
	{
		clear_static();
		clear_dynamic();
		generate_static();
		generate_dynamic();
		gui.mainview.end_gl();
	}
	dbg_end(DM_CALLS,"gl_objects::recreate_all");
}

// Recreate dynamic globs
void gl_objects::recreate_dynamic()
{
	dbg_begin(DM_CALLS,"gl_objects::recreate_dynamic");
	if (gui.mainview.begin_gl())
	{
		clear_dynamic();
		generate_dynamic();
		gui.mainview.end_gl();
	}
	dbg_end(DM_CALLS,"gl_objects::recreate_dynamic");
}
