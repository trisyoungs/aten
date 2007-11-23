/*
	*** GTK canvas
	*** src/gui-gtk/canvas-gtk.cpp
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

#include "gui-gtk/canvas-gtk.h"
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"
#include "base/prefs.h"

// Constructor
widgetcanvas::widgetcanvas()
{
	context_widget = NULL;
	subselect_enabled = FALSE;
	for (int i=0; i<3; i++)
	{
		mb[i] = FALSE;
		keymod[i] = FALSE;
	}
	#ifdef MEMDEBUG
		memdbg.create[MD_WIDGETCANVAS] ++;
	#endif
}

pixmapcanvas::pixmapcanvas()
{
	context_pixmap = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_PIXMAPCANVAS] ++;
	#endif
}

// Destructors
widgetcanvas::~widgetcanvas()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_WIDGETCANVAS] ++;
	#endif
}

pixmapcanvas::~pixmapcanvas()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_PIXMAPCANVAS] ++;
	#endif
}

// Begin GL
bool widgetcanvas::begin_gl()
{
	if (!valid) return FALSE;
	if (!gdk_gl_drawable_gl_begin(gldrawable,glcontext))
	{
		printf("widgetcanvas::begin_gl <<<< Failed to get rendering context! >>>>\n");
		return FALSE;
	}
	else
	{
		drawing = TRUE;
		return TRUE;
	}
}

// Finalize GL commands
void widgetcanvas::end_gl()
{
	gdk_gl_drawable_gl_end(gldrawable);
	drawing = FALSE;
}

// Swap buffers
void widgetcanvas::swap_buffers()
{
	gdk_gl_drawable_swap_buffers(gldrawable);
}

/*
// Widget Canvas
*/

// Set widget
bool widgetcanvas::set_widget(GtkWidget *w)
{
	// Creates the GL context for the supplied widget. Second widget indicates
	// previous glcontext with which to share display lists / textures.
	// Create OpenGL frame buffer configuration
	GdkGLConfig *glconfig = gdk_gl_config_new_by_mode( (GdkGLConfigMode) (
		GDK_GL_MODE_RGB | GDK_GL_MODE_DEPTH | GDK_GL_MODE_DOUBLE ));
	if (glconfig == NULL)
	{
		printf("Failed to create OpenGL frame buffer configuration.\n");
		return FALSE;
	}
	// Set the OpenGL capability of the supplied widget
	gtk_widget_set_gl_capability(w,glconfig,NULL,TRUE,GDK_GL_RGBA_TYPE);
	context_widget = w;
	return TRUE;
}

// Widget realize
void widgetcanvas::realize()
{
	// Sets the canvas to use a widget for output.
	dbg_begin(DM_CALLS,"widgetcanvas::realize");
	glcontext = gtk_widget_get_gl_context(context_widget);
	gldrawable = gtk_widget_get_gl_drawable(context_widget);
	// Enable rendering and initialise
	valid = TRUE;
	init_gl();
	dbg_end(DM_CALLS,"widgetcanvas::realize");
}

// Invalidate
void widgetcanvas::postredisplay()
{
	if (gui.exists()) gdk_window_invalidate_rect(context_widget->window,&context_widget->allocation,TRUE);
}

// Widget Expose
void widgetcanvas::expose()
{
	if ((!gui.exists()) || gui.no_rendering() ) return;
	render_scene(master.get_currentmodel());
	#ifdef SPEEDTEST
		speedtest_numrenders ++;
		speedtest_totalrenders ++;
	#endif
}

// Widget configure
void widgetcanvas::configure()
{
	// Store the new width and height of the widget and re-do projection
	w = (float)context_widget->allocation.width;
	h = (float)context_widget->allocation.height;
	do_projection();
	// Flag that render source needs to be reprojected
	if (displaymodel != NULL) displaymodel->log_change(LOG_VISUAL);
}


/*
// Pixmap Canvas
*/

// Set pixmap size to create and use
bool pixmapcanvas::set_pixmap(int pw, int ph, int pd)
{
	// Sets the canvas to create and use a pixmap for output.
	dbg_begin(DM_CALLS,"pixmapcanvas::set_pixmap");
	w = float(pw);
	h = float(ph);
	d = pd;
	// So, create a new pixmap and set it to be GL capable
	//GdkGLConfig *glconfig = gdk_gl_config_new_by_mode((GdkGLConfigMode) GDK_GL_MODE_RGB );
	GdkGLConfig *glconfig = gdk_gl_config_new_by_mode((GdkGLConfigMode) (GDK_GL_MODE_RGB|GDK_GL_MODE_DEPTH));
	if (glconfig == NULL) printf("Bad config!\n");
	context_pixmap = gdk_pixmap_new(NULL,pw,ph,d); //gdk_gl_config_get_depth(glconfig));
	// Grab the drawable and context pointers
	GdkGLPixmap *glpm = gdk_pixmap_set_gl_capability(context_pixmap,glconfig,NULL);
	gldrawable = gdk_pixmap_get_gl_drawable(context_pixmap);
	if (gldrawable == NULL) printf("Didn't get drawable!\n");
	if (glpm == NULL)
	{
		printf("Couldn't set GL capability for pixmap.\n");
		// TODO delete pixmap
		dbg_end(DM_CALLS,"pixmapcanvas::set_pixmap");
		return FALSE;
	}
	// Create context
	glcontext = gdk_gl_context_new(gldrawable, gui.mainview.get_glcontext(), TRUE, GDK_GL_RGBA_TYPE);
	if (glcontext == NULL) printf("Didn't create a context!\n");
	// Enable rendering and initialise
	if (gdk_pixmap_is_gl_capable(context_pixmap)) printf("Pixmap is GL capable.\n");
	valid = TRUE;
	init_gl();
	dbg_end(DM_CALLS,"pixmapcanvas::set_pixmap");
	return TRUE;
}

/*
// Pixmap Save
*/

// Save pixmap
bool pixmapcanvas::save(const char *filename)
{
	// Save the contents of the pixmap to the file specified
	dbg_begin(DM_CALLS,"pixmapcanvas::save");
	GdkColormap *cmap = gdk_drawable_get_colormap(GDK_DRAWABLE(context_pixmap));
	GdkPixbuf *pb = gdk_pixbuf_get_from_drawable(NULL,GDK_DRAWABLE(context_pixmap),cmap,0,0,0,0,int(w),int(h));
	bool success = gdk_pixbuf_save(pb,filename,"bmp",NULL,NULL);
	dbg_end(DM_CALLS,"pixmapcanvas::save");
	return success;
}
