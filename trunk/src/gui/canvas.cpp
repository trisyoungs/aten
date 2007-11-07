/*
	*** Model canvas stub
	*** src/gui/canvas.cpp
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
#include "gui/gui.h"
#include "gui/canvas.h"

// Static variables
bool canvas_master::mb[MB_NITEMS];
bool canvas_master::keymod[MK_NITEMS];
gl_objects canvas_master::globs;

// Constructor
canvas_master::canvas_master()
{
	valid = FALSE;
	render_point = -1;
	drawing = FALSE;
	displaymodel = NULL;
	activemode = UA_NONE;
	selectedmode = UA_PICKSELECT;
	list_modelcontents = 0;
}

// Destructor
canvas_master::~canvas_master()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_BASICCANVAS] ++;
	#endif
}

// Update Canvas
void canvas_master::postredisplay()
{
	printf("canvas_master::postredisplay - Not defined\n");
}

// Called when context is initialised and ready
void canvas_master::realize()
{
	printf("canvas_master::realize - Not defined\n");
}

// Called when context is resized
void canvas_master::configure()
{
	printf("canvas_master::configure - Not defined\n");
}

// Called when context needs to be redrawn
void canvas_master::expose()
{
	printf("canvas_master::expose - Not defined\n");
}

// Swap buffers
void canvas_master::swap_buffers()
{
	printf("canvas_master::swap_buffers - Not defined\n");
}

// Begin GL
bool canvas_master::begin_gl()
{
	printf("canvas_master::begin_gl - Not defined\n");
	return FALSE;
}

// End GL
void canvas_master::end_gl()
{
	printf("canvas_master::end_gl - Not defined\n");
}

// Set GL options
void canvas_master::init_gl()
{
	if (!valid) return;
	dbg_begin(DM_CALLS,"canvas_master::init_gl");
	if (begin_gl())
	{
		// Create model list (if necessary)
		if (list_modelcontents == 0) list_modelcontents = glGenLists(1);
		// Clear colour (with alpha = 0)
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
	dbg_end(DM_CALLS,"canvas_master::setup_gl");
}

// Create display lists
void canvas_master::create_lists()
{
	dbg_begin(DM_CALLS,"canvas_master::create_lists");
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
	for (int n=0; n<GLOB_NITEMS; n++) lists[n] = lists[GLOB_STICKATOM]+n;
	
	dbg_end(DM_CALLS,"canvas_master::create_lists");
}

/*
// Configuration
*/

// Calculate Projection
void canvas_master::do_projection()
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"canvas_master::do_projection");
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
		end_gl();
	}
	else printf("canvas_master::do_projection <<<< Failed to reset projection matrix >>>>\n");
	dbg_end(DM_CALLS,"canvas_master::do_projection");
}

/*
// Misc
*/

// Set valid
void canvas_master::set_valid(bool b)
{
	// Wait until the canvas is not drawing
	while (!valid) gui.process_events();
	// Now disallow drawing before we set the new status
	valid = FALSE;
	drawing = FALSE;
	valid = b;
}
