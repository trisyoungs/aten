/*
	*** Scene objects
	*** src/render/objects.cpp

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
#include "classes/cell.h"
#include "classes/atom.h"
#include "render/globs.h"
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
	  glTranslatef(x,y,0.0);
	  glScalef(r,r,r);
	  glCallList(globs.lists[GLOB_CIRCLE]);
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
	  glTranslatef(origin.x,origin.y,origin.z);
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
			case (DS_STICK): glCallList(globs.lists[GLOB_WIRETUBEATOM]); break;
			case (DS_TUBE):	glCallList(globs.lists[GLOB_WIRETUBEATOM]); break;
			case (DS_SPHERE): glCallList(globs.lists[GLOB_WIRESPHEREATOM]); break;
			case (DS_SCALED): 
				radius = prefs.screenradius(i);
				glPushMatrix();
				  glScalef(radius,radius,radius);
				  glCallList(globs.lists[GLOB_WIREUNITATOM]);
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
	glRotatef(camrot*DEGRAD,0.0f,0.0f,1.0f);
	glMultMatrixd(rmat);
	glLineWidth(1.0);
	glDisable(GL_LIGHTING);
	glCallList(globs.lists[GLOB_GLOBE]);
	// Reset the viewport back to the entire canvas
	glViewport(0,0,(int)w,(int)h);
}

void canvas_master::gl_cylinderbond(atom *i, atom *j, const vec3<double> &rj, double rij)
{
	// Determine spherical coordinates
	double phi = DEGRAD * acos(rj.z/rij);		// Angle out of XZ plane
	glPushMatrix();
	  // Special case where the bond is exactly in the XY plane.
	  if ((180.0 - phi) < 0.0001)
	  {
		//if (rj->z < 0.0) phi = -phi;
	  	//glRotatef(phi, -rj->y/rj->z , rj->x/rj->z ,0.0f);
		glRotatef(phi,1.0,0.0,0.0);
	  }
	  else glRotatef(phi, -rj.y/rij , rj.x/rij ,0.0f);
	  glScalef(1.0f,1.0f,rij);
	  glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, i->colour);
	  glCallList(globs.lists[GLOB_BOND]);
	  glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, j->colour);
	  glTranslatef(0.0f,0.0f,1.0);
	  glCallList(globs.lists[GLOB_BOND]);
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
	    glCallList(globs.lists[GLOB_SPHEREATOM]);
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
	    glCallList(globs.lists[GLOB_UNITATOM]);
	  glPopMatrix();
	glPopMatrix();
}

void canvas_master::gl_stickbond(atom* i, atom* j, const vec3<double> &v)
{
	glColor3iv(i->colour);
	i->is_selected() ? glLineWidth(3.0) : glLineWidth(1.0);
	glBegin(GL_LINES);
	  glVertex3f(0.0f,0.0f,0.0f);
	  glVertex3f(v.x,v.y,v.z);
	glEnd();
	glColor3iv(j->colour);
	j->is_selected() ? glLineWidth(3.0) : glLineWidth(1.0);
	glBegin(GL_LINES);
	  glVertex3f(v.x,v.y,v.z);
	  glVertex3f(v.x*2.0,v.y*2.0,v.z*2.0);
	glEnd();
}
