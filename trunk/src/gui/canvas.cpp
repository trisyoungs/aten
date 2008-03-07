/*
	*** Model canvas stub
	*** src/gui/canvas.cpp
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
#include "gui/gui.h"
#include "gui/canvas.h"
#include "render/gl2ps.h"
#include "gui/tcanvas.uih"

// Constructor
canvas::canvas()
{
	valid = FALSE;
	render_point = -1;
	drawing = FALSE;
	displaymodel = NULL;
	drawpixelwidth = 1.0;
	activemode = UA_NONE;
	selectedmode = UA_PICKSELECT;
	list[0] = 0;
	contextwidget = NULL;
	subselect_enabled = FALSE;
	for (int i=0; i<3; i++)
	{
		mb[i] = FALSE;
		keymod[i] = FALSE;
	}
}

// Destructor
canvas::~canvas()
{
}

// Begin GL
bool canvas::begin_gl()
{
	if (!valid) return FALSE;
	drawing = TRUE;
	return TRUE;
}

// Finalize GL commands
void canvas::end_gl()
{
	drawing = FALSE;
}

// Swap buffers
void canvas::swap_buffers()
{
}

/*
// Widget Canvas
*/

// Set widget
bool canvas::set_widget(TCanvas *w)
{
	contextwidget = w;
	return TRUE;
}

// Widget realize
void canvas::realize()
{
	// Sets the canvas to use a widget for output.
	dbg_begin(DM_CALLS,"canvas::realize");
	valid = TRUE;
	init_gl();
	dbg_end(DM_CALLS,"canvas::realize");
}

// Invalidate
void canvas::postredisplay()
{
	dbg_begin(DM_CALLS,"canvas::postredisplay");
	if (gui.exists()) contextwidget->paintGL();
	dbg_end(DM_CALLS,"canvas::postredisplay");
}

// Widget Expose
void canvas::expose()
{
	if ((!gui.exists()) || gui.no_rendering() ) return;
	// Render from the current rendering source
	render_scene(master.get_currentmodel()->get_render_source());
	#ifdef SPEEDTEST
		speedtest_numrenders ++;
		speedtest_totalrenders ++;
	#endif
}

// Widget configure
void canvas::configure()
{
	// Store the new width and height of the widget and re-do projection
	w = (float)contextwidget->width();
	h = (float)contextwidget->height();
	do_projection();
	// Flag that render source needs to be reprojected
	if (displaymodel != NULL) displaymodel->log_change(LOG_VISUAL);
}

// Calculate drawing pixel width
void canvas::calculate_drawpixelwidth()
{
	// Get the Angstrom width of a single pixel at the current draw depth in the current view
	static vec3<double> r1, r2;
	if (displaymodel != NULL)
	{
		r1 = displaymodel->guide_to_model(w/2,h/2);
		r2 = displaymodel->guide_to_model(w/2+1,h/2);
		r2 -= r1;
		drawpixelwidth = r2.x;
	}
	else drawpixelwidth = 1.0;
}

// Set GL options
void canvas::init_gl()
{
	if (!valid) return;
	dbg_begin(DM_CALLS,"canvas::init_gl");
	if (begin_gl())
	{
		// Create lists for globs if this is the first call to init_gl()
		if (list[0] == 0)
		{
			list[GLOB_STICKATOM] = glGenLists(GLOB_NITEMS);
			for (int n=1; n<GLOB_NITEMS; n++) list[n] = list[GLOB_STICKATOM]+n;
		}

		// Fill display lists
		create_lists();

		// Clear colour
		GLint *clrcol = prefs.get_colour(COL_BG);
		glClearColor(clrcol[0],clrcol[1],clrcol[2],clrcol[3]);
		glClearDepth(1.0);
		// Perspective hint
		glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST);
		// Enable depth buffer
		glEnable(GL_DEPTH_TEST);
		// Smooth shading
		glShadeModel(GL_SMOOTH);
		// Auto-calculate surface normals
		glEnable(GL_NORMALIZE);
		// Set alpha-blending function
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
		//glBlendFunc(GL_SRC_ALPHA_SATURATE, GL_ONE);
		// Set up the light model
		glEnable(GL_LIGHTING);
		glLightiv(GL_LIGHT0,GL_AMBIENT,prefs.get_spotlight(SL_AMBIENT));
		glLightiv(GL_LIGHT0,GL_DIFFUSE,prefs.get_spotlight(SL_DIFFUSE));
		glLightiv(GL_LIGHT0,GL_SPECULAR,prefs.get_spotlight(SL_SPECULAR));
		glLightiv(GL_LIGHT0,GL_POSITION,prefs.get_spotlight(SL_POSITION));
		prefs.get_spotlight_on() ? glEnable(GL_LIGHT0) : glDisable(GL_LIGHT0);
		glDisable(GL_BLEND);
		glDisable(GL_LINE_SMOOTH);
		glDisable(GL_POLYGON_SMOOTH);
		// Configure antialiasing
		if (prefs.get_gl_option(GO_LINEALIASING))
		{
			glEnable(GL_BLEND);
			glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_LINE_SMOOTH);
		}
		if (prefs.get_gl_option(GO_POLYALIASING))
		{
			glEnable(GL_BLEND);
			glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_POLYGON_SMOOTH);
		}
		// Configure fog effects
		if (prefs.get_gl_option(GO_FOG))
		{
			glFogi(GL_FOG_MODE, GL_LINEAR);
			GLfloat fogColor[4]= { prefs.colours[COL_BG][0], prefs.colours[COL_BG][1], prefs.colours[COL_BG][2], 1.0 };
			glFogfv(GL_FOG_COLOR, fogColor);
			glFogf(GL_FOG_DENSITY, 0.35f);
			glHint(GL_FOG_HINT, GL_NICEST);
			glFogf(GL_FOG_START,prefs.gl_fog_near);
			glFogf(GL_FOG_END,prefs.gl_fog_far);
			glEnable(GL_FOG);
		}
		else glDisable(GL_FOG);
		// Configure face culling
		glCullFace(GL_BACK);
		prefs.get_gl_option(GO_BACKCULLING) ? glEnable( GL_CULL_FACE ) : glDisable(GL_CULL_FACE);
		// Test
		// End Test
		end_gl();
	}
	else printf("Failed to set-up OpenGL on canvas.\n");
	dbg_end(DM_CALLS,"canvas::init_gl");
}

// Create display lists
void canvas::create_lists()
{
	if (!is_valid()) return;
	dbg_begin(DM_CALLS,"canvas::create_lists");
	// Generate quadric objects for subsequent use
	quadric1 = gluNewQuadric();			// Creates a quadric object for use.
	quadric2 = gluNewQuadric();			// Creates a quadric object for use.
	gluQuadricDrawStyle(quadric1, GLU_FILL);	// Set drawing style of the quadric to solid.
	gluQuadricNormals(quadric1, GL_SMOOTH);		// Set up normals for shading.
	gluQuadricTexture(quadric1, GL_FALSE);		// Turn off texturing.
	gluQuadricDrawStyle(quadric2, GLU_FILL);	// Set drawing style of the quadric to solid.
	gluQuadricNormals(quadric2, GL_SMOOTH);		// Set up normals for shading.
	gluQuadricTexture(quadric2, GL_FALSE);		// Turn off texturing.

	int n,m, atomdetail, extent, ticks;
	double delta, tickdelta, tickheight, ticktop, tickbottom, spacing;
	// Grab some oft-used values
	atomdetail = prefs.render_atom_detail;
	spacing = prefs.build_guide_spacing;
	extent = prefs.build_guide_extent;
	ticks = prefs.build_guide_ticks;

	/*
	// Selected Atoms
	*/
	gluQuadricDrawStyle(quadric1, GLU_FILL);      // Set drawing style of the quadric to solid.
	// Enlarged sphere (for selections with DS_TUBE)
	glNewList(list[GLOB_SELTUBEATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_tube_size*prefs.render_selection_scale,atomdetail,atomdetail*2);
	glEndList();
	// Enlarged sphere (for selections with DS_SPHERE)
	glNewList(list[GLOB_SELSPHEREATOM],GL_COMPILE);
	  //gl_sphere(prefs.render_atom_size[DS_SPHERE]*prefs.render_selection_scale);
	  gluSphere(quadric1,prefs.render_atom_size[DS_SPHERE]*prefs.render_selection_scale,atomdetail,atomdetail*2);
	glEndList();
	// Enlarged sphere (for selections with DS_SCALED)
	glNewList(list[GLOB_SELUNITATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_selection_scale,atomdetail,atomdetail*2);
	glEndList();

	/*
	// Atoms
	*/
	// Stick Atom (for DS_STICK)
	glNewList(list[GLOB_STICKATOM],GL_COMPILE);
	  glBegin(GL_LINES);
	    glVertex3f(-0.5f,0.0f,0.0f); glVertex3f(0.5f,0.0f,0.0f);
	    glVertex3f(0.0f,-0.5f,0.0f); glVertex3f(0.0f,0.5f,0.0f);
	    glVertex3f(0.0f,0.0f,-0.5f); glVertex3f(0.0f,0.0f,0.5f);
	  glEnd();
	glEndList();
	gluQuadricDrawStyle(quadric1,GLU_FILL);      // Set drawing style of the quadric to solid.
	// Atom Sphere (for DS_TUBE)
	glNewList(list[GLOB_TUBEATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_TUBE],atomdetail*2,atomdetail);
	glEndList();
	// Atom Sphere (for DS_SPHERE)
	glNewList(list[GLOB_SPHEREATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_SPHERE],atomdetail*2,atomdetail);
	  //gl_sphere(prefs.render_atom_size[DS_SPHERE]);
	gluQuadricDrawStyle(quadric1,GLU_FILL);      // Set drawing style of the quadric to solid.
	glEndList();
	// Unit Atom Sphere (for DS_SCALED)
	glNewList(list[GLOB_UNITATOM],GL_COMPILE);
	  gluSphere(quadric1,1.0,atomdetail*2,atomdetail);
	glEndList();
	gluQuadricDrawStyle(quadric1,GLU_LINE);      // Set drawing style of the quadric to solid.
	// Wire Atom Sphere (for DS_TUBE)
	glNewList(list[GLOB_WIRETUBEATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_tube_size*1.1,atomdetail*2,atomdetail);
	glEndList();
	// Wire Atom Sphere (for DS_SPHERE)
	glNewList(list[GLOB_WIRESPHEREATOM],GL_COMPILE);
	  gluSphere(quadric1,prefs.render_atom_size[DS_SPHERE]*1.1,atomdetail*2,atomdetail);
	glEndList();
	// Wire Unit Atom Sphere (for DS_SCALED)
	glNewList(list[GLOB_WIREUNITATOM],GL_COMPILE);
	  gluSphere(quadric1,1.1,atomdetail*2,atomdetail);
	glEndList();
	/*
	// Cylinders (bonds)
	*/
	// Solid cylinder
	gluQuadricDrawStyle(quadric1,GLU_FILL);
	glNewList(list[GLOB_CYLINDER],GL_COMPILE);
	  gluCylinder(quadric2,prefs.render_tube_size,prefs.render_tube_size,1.0f,prefs.render_bond_detail,prefs.render_bond_detail);
	glEndList();
	// Wireframe cylinder
	gluQuadricDrawStyle(quadric1,GLU_LINE);
	glNewList(list[GLOB_WIRECYLINDER],GL_COMPILE);
	  gluCylinder(quadric2,prefs.render_tube_size,prefs.render_tube_size,1.0f,prefs.render_bond_detail,prefs.render_bond_detail);
	glEndList();
	// Selected wireframe cylinder
	gluQuadricDrawStyle(quadric1,GLU_LINE);
	glNewList(list[GLOB_SELWIRECYLINDER],GL_COMPILE);
	  gluCylinder(quadric2,prefs.render_tube_size*prefs.render_selection_scale,prefs.render_tube_size*prefs.render_selection_scale,1.0f,prefs.render_bond_detail,prefs.render_bond_detail);
	glEndList();
	/*
	// Others
	*/
	// View axes
	glNewList(list[GLOB_GLOBE],GL_COMPILE);
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
	glNewList(list[GLOB_GUIDE],GL_COMPILE);
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
	glNewList(list[GLOB_CIRCLE],GL_COMPILE);
	  glBegin(GL_LINE_LOOP);
	    for (int i=0; i < nsegs; i++)
	    {
		float degInRad = i*(360.0/nsegs)/DEGRAD;
		glVertex2f(cos(degInRad),sin(degInRad));
	    }
	  glEnd();
	glEndList();
	// Unit Wire Cube (centred at origin)
	glNewList(list[GLOB_WIREUNITCUBE],GL_COMPILE);
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
	glNewList(list[GLOB_UNITCUBE],GL_COMPILE);
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
	glNewList(list[GLOB_CELLAXES],GL_COMPILE);
	  double asize = 0.5, awidth = 0.2, posoffset = 0.5;
	  for (int i=0; i<3; i++)
	  {
		glPushMatrix();
		  if (i == 1) glRotated(-90.0,0.0,1.0,0.0);
		  else if (i == 2) glRotated(90.0,0.0,0.0,1.0);
		  glPushMatrix();
		    glScaled(0.5,awidth,awidth);
		    glTranslated(0.5,0.0,0.0);
		    glCallList(list[GLOB_UNITCUBE]);
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

	dbg_end(DM_CALLS,"canvas::create_lists");
}

/*
// Configuration
*/

// Calculate Projection
void canvas::do_projection()
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"canvas::do_projection");
	double pmat[16], bottom, top;
	// Check source
	if (begin_gl())
	{
		// Set the viewport size to the whole area and grab the matrix
		glViewport(0,0,(int)w,(int)h);
		glGetIntegerv(GL_VIEWPORT,VMAT);
		// Calculate and store a projection matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		aspect = w / h;
		if (prefs.render_perspective)
		{
			// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
			bottom = tan(prefs.render_fov / DEGRAD) * prefs.gl_clip_near;
			top = -bottom;
			glFrustum(aspect*top,aspect*bottom,top,bottom,prefs.gl_clip_near,prefs.gl_clip_far);
		}
		else
		{
			bottom = displaymodel->get_ortho_size();
			top = -bottom;
			//glOrtho(aspect*top,aspect*bottom,top,bottom,-bottom*2.0,bottom*2.0);
			glOrtho(aspect*top,aspect*bottom,top,bottom,-prefs.gl_clip_near,prefs.gl_clip_far);
		}
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		PMAT.set_from_column_major(pmat);
		// Rotation globe projection matrix (square)
		glLoadIdentity();
		glFrustum(-1.0, 1.0, -1.0, 1.0, 0.0, 10.0);
		glGetDoublev(GL_PROJECTION_MATRIX,pmat); // Store the resulting projection and
		GlobePMAT.set_from_column_major(pmat);
		glMatrixMode(GL_MODELVIEW);
		// Calculate the new drawpixelwidth
		calculate_drawpixelwidth();
		end_gl();
	}
	else printf("canvas::do_projection <<<< Failed to reset projection matrix >>>>\n");
	dbg_end(DM_CALLS,"canvas::do_projection");
}

/*
// Misc
*/

// Set valid
void canvas::set_valid(bool b)
{
	// Wait until the canvas is not drawing
	while (!valid) gui.process_events();
	// Now disallow drawing before we set the new status
	valid = FALSE;
	drawing = FALSE;
	valid = b;
}

/*
// Save vector image
*/
void canvas::save_vector(model *source, vector_format vf, const char *filename)
{
	// Open output file
	FILE *vectorfile = fopen(filename, "w");
	if (vectorfile == NULL)
	{
		msg(DM_NONE,"Couldn't open output file for vector export.\n");
		return;
	}
	GLint result = GL2PS_OVERFLOW, bufsize = 0;
	// Loop until the feedback buffer is large enough
	while (result == GL2PS_OVERFLOW)
	{
		bufsize += 1024*1024;
		result = gl2psBeginPage(source->get_name(), "Aten", VMAT, vf, GL2PS_BSP_SORT, GL2PS_DRAW_BACKGROUND | GL2PS_OCCLUSION_CULL, GL_RGBA, 0, 0, 0, 0, 0, bufsize, vectorfile, filename);
		printf("Result = %i\n",result);
		render_scene(source);
		result = gl2psEndPage();
		printf("Result = %i\n",result);
	}
}
