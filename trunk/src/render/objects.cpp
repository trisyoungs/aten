/*
	*** Scene objects
	*** src/render/objects.cpp
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

#include "base/master.h"
#include "base/prefs.h"
#include "base/elements.h"
#include "classes/cell.h"
#include "classes/atom.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"

/*
// Primitives
*/

void Canvas::glText(double x, double y, const char *s)
{
	contextWidget_->renderText((int)x, (int)y, s);
}

// Render text at 3D coordinates
void Canvas::glText(const Vec3<double> r, const char *s)
{
	contextWidget_->renderText(r.x, r.y, r.z, s);
}

// Draw a diamond at the point specified, with 'radius' r.
void Canvas::glDiamond(double x, double y, double r)
{
	glBegin(GL_LINE_LOOP);
	  glVertex2d(x,y+r);
	  glVertex2d(x+r,y);
	  glVertex2d(x,y-r);
	  glVertex2d(x-r,y);
	glEnd();
}

// Draw a box centred at x,y with diameter r
void Canvas::glSquare(double x, double y, double r)
{
	glBegin(GL_LINE_LOOP);
	  glVertex2d(x+r,y+r);
	  glVertex2d(x+r,y-r);
	  glVertex2d(x-r,y-r);
	  glVertex2d(x-r,y+r);
	glEnd();
}

// Draw a box from top-left x1,y1 to bottom-right x2,y2
void Canvas::glRectangle(double x1, double y1, double x2, double y2)
{
	glBegin(GL_LINE_LOOP);
	  glVertex2d(x1,y1);
	  glVertex2d(x1,y2);
	  glVertex2d(x2,y2);
	  glVertex2d(x2,y1);
	glEnd();
}

// Draw a circle of (pixel) radius 'r' centred at x,y
void Canvas::glCircle(double x, double y, double r)
{
	//r += master.lineardelta;
	glPushMatrix();
	  glTranslated(x,y,0.0);
	  glScalef(r,r,r);
	  glCallList(list_[GLOB_CIRCLE]);
	glPopMatrix();
}

// Draw an arrow from origin along vector v
void Canvas::glArrow(const Vec3<double> &origin, const Vec3<double> &v)
{
	static Vec3<double> perp, v2, v3;
	v2 = v;
	perp = v2.orthogonal();
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

void Canvas::glSubsel3d()
{
	// 3D atom highlights on atoms defined in subselection provided.
	static Vec3<double> ir;
	double radius;
	Refitem<Atom,int> *ri;
	Atom::DrawStyle renderstyle, style_i;
	Atom *i, *lastatom = NULL;
	renderstyle = prefs.renderStyle();
	ri = subselection_.first();
	while (ri != NULL)
	{
		i = ri->item;
		ir = i->r();
		// Draw a wireframe sphere at the atoms position
		glPushMatrix();
		  glTranslated(ir.x,ir.y,ir.z);
		  renderstyle == Atom::IndividualStyle ? style_i = i->style() : style_i = renderstyle;
		  switch (style_i)
		  {
			case (Atom::StickStyle):
				glCallList(list_[GLOB_WIRETUBEATOM]);
				break;
			case (Atom::TubeStyle):
				glCallList(list_[GLOB_WIRETUBEATOM]);
				break;
			case (Atom::SphereStyle):
				glCallList(list_[GLOB_WIRESPHEREATOM]);
				break;
			case (Atom::ScaledStyle): 
				radius = prefs.screenRadius(i);
				glPushMatrix();
				  glScaled(radius,radius,radius);
				  glCallList(list_[GLOB_WIREUNITATOM]);
				glPopMatrix();
				break;
		  }
		glPopMatrix();
		lastatom = i;
		ri = ri->next;
	}
}

void Canvas::renderRotationGlobe(double *rmat, double camrot)
{
	// Draw the coordinate axes at the bottom right of the screen.
	// First set up the small viewport and apply our stored projection matrix.
	int globesize = prefs.globeSize();
	glViewport((int)width_-globesize,0,globesize,globesize);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	static double pmat[16];
	GlobePMAT.copyColumnMajor(pmat);
	glMultMatrixd(pmat);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glRotated(camrot*DEGRAD,0.0,0.0,1.0);
	glMultMatrixd(rmat);
	glLineWidth(1.0);
	glDisable(GL_LIGHTING);
	glCallList(list_[GLOB_GLOBE]);
	// Reset the viewport back to the entire canvas
	glViewport(0,0,(int)width_,(int)height_);
}

void Canvas::glCylinder(const Vec3<double> &rj, double rij, int style)
{
	/* Styles are:
		0 = solid
		1 = expanded solid
		2 = expanded wireframe
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
	  if (style == 0) glCallList(list_[GLOB_CYLINDER]);
	  else if (style == 1) glCallList(list_[GLOB_SELCYLINDER]);
	  else if (style == 2) glCallList(list_[GLOB_SELWIRECYLINDER]);
	glPopMatrix();
}

/*
void Canvas::gl_ellipsoid(Vec3<double> *pos, Vec3<double> *lookat, Vec3<double> *scale)
{
	static double phi, mag;
	static Vec3<double> direction;
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
	    glCallList(list_[GLOB_SPHEREATOM]);
	  glPopMatrix();
	glPopMatrix();
}
*/

void Canvas::glEllipsoid(const Vec3<double> &centre, const Vec3<double> &v1, const Vec3<double> &v2)
{
	static double phi, mag1, mag2, r[16], t[16];
	static Mat4<double> rotmat;
	static Vec3<double> v3;
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
	    rotmat.copyRowMajor(r);
	    glMultMatrixd(r);
	    glCallList(list_[GLOB_UNITATOM]);
	  glPopMatrix();
	glPopMatrix();
}

void Canvas::glSphere(double radius, bool filled)
{
	// Don't use this to render objects to the view - create a display list first!
	int i, j;
	int lats = prefs.atomDetail();
	int longs = int(lats * 1.5);
	double lat0, z0, zr0, lat1, z1, zr1, lng, x, y;
	glPolygonMode(GL_FRONT_AND_BACK, (filled ? GL_FILL : GL_LINE));
	for(i = 0; i <= lats; i++)
	{
		lat0 = M_PI * (-0.5 + (double) (i - 1) / lats);
		z0  = sin(lat0);
		zr0 =  cos(lat0);

		lat1 = M_PI * (-0.5 + (double) i / lats);
		z1 = sin(lat1);
		zr1 = cos(lat1);

		glBegin(GL_QUAD_STRIP);
		  for(j = 0; j <= longs; j++)
		  {
			lng = 2 * M_PI * (double) (j - 1) / longs;
			x = cos(lng);
			y = sin(lng);
			glNormal3d(x * zr0, y * zr0, z0);
			glVertex3d(x * zr0 * radius, y * zr0 * radius, z0 * radius);
			glNormal3d(x * zr1, y * zr1, z1);
			glVertex3d(x * zr1 * radius, y * zr1 * radius, z1 * radius);
		  }
		glEnd();
	}
}

void Canvas::glCylinder(double radius, bool filled)
{
	int n, m;
	double d;
	glPolygonMode(GL_FRONT_AND_BACK, (filled ? GL_FILL : GL_LINE));
	for (n=0; n<prefs.bondDetail(); n++)		// Slices
	{
		glBegin(GL_QUAD_STRIP);
		  for (m=0; m<=prefs.bondDetail(); m++)	// Stacks
		  {
			  d = m * TWOPI / prefs.bondDetail();
			  glNormal3d(cos(d), sin(d), 0.0);
			  glVertex3d(cos(d) * radius, sin(d) * radius, n * (1.0 / prefs.bondDetail()));
			  glVertex3d(cos(d) * radius, sin(d) * radius, (n + 1) * (1.0 / prefs.bondDetail()));
		  }
		glEnd();
	}
}
