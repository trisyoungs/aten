/*
	*** Scene objects
	*** src/render/objects.cpp
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

#include "base/master.h"
#include "base/prefs.h"
#include "base/elements.h"
#include "classes/cell.h"
#include "classes/atom.h"
#include "gui/gui.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

/*
// Primitives
*/

void canvas_master::textbitmap(double x, double y, const char *s)
{
	glRasterPos2d(x,y);
	for (int i = 0; s[i] != '\0'; i++)
		glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,s[i]);
}

// Render text at 3D coordinates
void canvas_master::textbitmap(const vec3<double> r, const char *s)
{
	static int i;
	glRasterPos3d(r.x, r.y, r.z);
	for (i=0; s[i] != '\0'; i++)
		glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,s[i]);
}

void canvas_master::textstroke(const char *s)
{
	for (int i = 0; s[i] != '\0'; i++)
		glutStrokeCharacter(GLUT_STROKE_ROMAN,s[i]);
}

// Draw a diamond at the point specified, with 'radius' r.
void canvas_master::gl_diamond(double x, double y, double r)
{
	glBegin(GL_LINE_LOOP);
	  glVertex2d(x,y+r);
	  glVertex2d(x+r,y);
	  glVertex2d(x,y-r);
	  glVertex2d(x-r,y);
	glEnd();
}

// Draw a box centred at x,y with diameter r
void canvas_master::gl_square(double x, double y, double r)
{
	glBegin(GL_LINE_LOOP);
	  glVertex2d(x+r,y+r);
	  glVertex2d(x+r,y-r);
	  glVertex2d(x-r,y-r);
	  glVertex2d(x-r,y+r);
	glEnd();
}

// Draw a box from top-left x1,y1 to bottom-right x2,y2
void canvas_master::gl_rectangle(double x1, double y1, double x2, double y2)
{
	glBegin(GL_LINE_LOOP);
	  glVertex2d(x1,y1);
	  glVertex2d(x1,y2);
	  glVertex2d(x2,y2);
	  glVertex2d(x2,y1);
	glEnd();
}

// Draw a circle of (pixel) radius 'r' centred at x,y
void canvas_master::gl_circle(double x, double y, double r)
{
	//r += master.lineardelta;
	glPushMatrix();
	  glTranslated(x,y,0.0);
	  glScalef(r,r,r);
	  glCallList(list[GLOB_CIRCLE]);
	glPopMatrix();
}

// Draw an arrow from origin along vector v
void canvas_master::gl_arrow(const vec3<double> &origin, const vec3<double> &v)
{
	static vec3<double> perp, v2, v3;
	v2 = v;
	perp = v2.get_orthogonal();
	perp *= 0.1;
	v2 *= 0.9;
	glPushMatrix();
	  glTranslated(origin.x,origin.y,origin.z);
	  glPushMatrix();
	    glBegin(GL_LINES);
	      glVertex3d(0.0,0.0,0.0);
	      glVertex3d(v.x,v.y,v.z);
	      v3 = v2 + perp;
	      glVertex3d(v3.x,v3.y,v3.z);
	      glVertex3d(v.x,v.y,v.z);
	      v3 = v2 - perp;
	      glVertex3d(v3.x,v3.y,v3.z);
	      glVertex3d(v.x,v.y,v.z);
	    glEnd();
	  glPopMatrix();
	glPopMatrix();
}

/*
// Custom
*/

void canvas_master::gl_subsel_3d()
{
	// 3D atom highlights on atoms defined in subselection provided.
	static vec3<double> ir;
	double radius;
	refitem<atom> *ri;
	draw_style renderstyle, style_i;
	atom *i, *lastatom = NULL;
	renderstyle = prefs.render_style;
	ri = subsel.first();
	while (ri != NULL)
	{
		i = (atom*) ri->item;
		ir = i->r;
		// Draw a wireframe sphere at the atoms position
		glPushMatrix();
		  glTranslated(ir.x,ir.y,ir.z);
		  renderstyle == DS_INDIVIDUAL ? style_i = i->get_style() : style_i = renderstyle;
		  switch (style_i)
		  {
			case (DS_STICK): glCallList(list[GLOB_WIRETUBEATOM]); break;
			case (DS_TUBE):	glCallList(list[GLOB_WIRETUBEATOM]); break;
			case (DS_SPHERE): glCallList(list[GLOB_WIRESPHEREATOM]); break;
			case (DS_SCALED): 
				radius = prefs.screenradius(i);
				glPushMatrix();
				  glScaled(radius,radius,radius);
				  glCallList(list[GLOB_WIREUNITATOM]);
				glPopMatrix();
				break;
		  }
		glPopMatrix();
		lastatom = i;
		ri = ri->next;
	}
}

void canvas_master::render_rotation_globe(double *rmat, double camrot)
{
	// Draw the coordinate axes at the bottom right of the screen.
	// First set up the small viewport and apply our stored projection matrix.
	glViewport((int)w-prefs.render_globe_size,0,prefs.render_globe_size,prefs.render_globe_size);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	static double pmat[16];
	GlobePMAT.get_column_major(pmat);
	glMultMatrixd(pmat);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glRotated(camrot*DEGRAD,0.0,0.0,1.0);
	glMultMatrixd(rmat);
	glLineWidth(1.0);
	glDisable(GL_LIGHTING);
	glCallList(list[GLOB_GLOBE]);
	// Reset the viewport back to the entire canvas
	glViewport(0,0,(int)w,(int)h);
}

void canvas_master::gl_cylinder(const vec3<double> &rj, double rij, int style)
{
	/* Styles are:
		0 = solid
		1 = wireframe
		2 = solid + expanded wireframe
	*/
	// Determine spherical coordinates
	static double phi;
	// Calculate angle out of XZ plane
	phi = DEGRAD * acos(rj.z/rij);
	glPushMatrix();
	  // Special case where the bond is exactly in the XY plane.
	  if ((180.0 - phi) < 0.0001) glRotated(phi,1.0,0.0,0.0);
	  else glRotated(phi, -rj.y/rij , rj.x/rij ,0.0f);
	  glScaled(1.0,1.0,rij);
	  // Draw cylinder (bond)
	  if (style == 0) glCallList(list[GLOB_CYLINDER]);
	  else if (style == 1) glCallList(list[GLOB_WIRECYLINDER]);
	  if (style == 2) glCallList(list[GLOB_SELWIRECYLINDER]);
	glPopMatrix();
}

/*
void canvas_master::gl_ellipsoid(vec3<double> *pos, vec3<double> *lookat, vec3<double> *scale)
{
	static double phi, mag;
	static vec3<double> direction;
	direction = *lookat;
	direction = direction - *pos;
	mag = direction.mag_and_normalise();
	// Apply position and scaling transforms
	glPushMatrix();
	  glTranslated(pos->x,pos->y,pos->z);
	  glPushMatrix();
	    // Determine spherical coordinates for rotation
	    phi = DEGRAD * acos(direction.z);		// Angle out of XZ plane
	    // Special case where the bond is exactly in the XY plane.
	    if ((180.0 - phi) < 0.0001)
	    {
		//if (rj->z < 0.0) phi = -phi;
	  	//glRotatef(phi, -rj->y/rj->z , rj->x/rj->z ,0.0f);
		glRotatef(phi,1.0,0.0,0.0);
	    }
	    else glRotatef(phi, -direction.y , direction.x ,0.0f);
	    glScaled(scale->x,scale->y,scale->z);
	    glCallList(list[GLOB_SPHEREATOM]);
	  glPopMatrix();
	glPopMatrix();
}
*/

void canvas_master::gl_ellipsoid(const vec3<double> &centre, const vec3<double> &v1, const vec3<double> &v2)
{
	static double phi, mag1, mag2, r[16], t[16];
	static mat4<double> rotmat;
	static vec3<double> v3;
	// Extremely slow but working ellipsoid drawing. Make a matrix consisting of the two 'axes' defined
	// by vec1 and vec2, and a third orthogonal to these.
	rotmat.rows[1].set(v1.x,v1.y,v1.z,0.0);
	rotmat.rows[0].set(v2.x,v2.y,v2.z,0.0);
	// We will assume (dangerously) that the two vectors are already orthogonal.
	// Create the third from the cross product of the other two
	v3 = v1 * v2;
	v3.normalise();
	rotmat.rows[2].set(-v3.x,-v3.y,-v3.z,0.0);
	// Apply position and scaling transforms
	glPushMatrix();
	  glTranslated(centre.x,centre.y,centre.z);
	  glPushMatrix();
	    rotmat.get_row_major(r);
	    glMultMatrixd(r);
	    glCallList(list[GLOB_UNITATOM]);
	  glPopMatrix();
	glPopMatrix();
}
