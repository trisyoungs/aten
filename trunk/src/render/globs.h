/*
	*** OpenGL objects
	*** src/render/globs.cpp

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

#ifndef H_GLOBS_H
#define H_GLOBS_H

#include "base/debug.h"		// To get config.h data i.e. IS_MAC
#ifdef IS_MAC
	#include <OpenGL/gl.h>
	#include <OpenGL/glu.h>
#else
	#include <GL/gl.h>
	#include <GL/glu.h>
#endif

// GL Objects (dynamics last)
enum glob_list { GLOB_STICKATOM, GLOB_TUBEATOM, GLOB_SPHEREATOM, GLOB_UNITATOM, GLOB_WIRETUBEATOM, GLOB_WIRESPHEREATOM, GLOB_WIREUNITATOM, GLOB_BOND, GLOB_WIREBOND,
	GLOB_GLOBE, GLOB_GUIDE, GLOB_CIRCLE, GLOB_CELLAXES, GLOB_SELTUBEATOM, GLOB_SELSPHEREATOM, GLOB_SELUNITATOM, GLOB_WIREUNITCUBE, GLOB_UNITCUBE, GLOB_NITEMS };

class gl_objects
{
	private:
	// Quadric objects
	GLUquadricObj *quadric1, *quadric2;

	/*
	// Display Lists
	*/
	private:
	// Create static globs for rendering
	void generate_static();
	// Create globs for rendering that are prone to change regularly
	void generate_dynamic();
	// Delete static globs
	void clear_static();
	// Delete dynamic globs
	void clear_dynamic();
	// Custom sphere
	void sphere(int, int, double);

	public:
	// Sorted display list ID's
	GLuint lists[GLOB_NITEMS];
	// Create display lists for globs
	void initialise();
	// Initial create all 
	void create_all();
	// Delete before exit
	void delete_all();
	// Recreate all GL objects
	void recreate_all();
	// Recreate only dynamic GL objects
	void recreate_dynamic();
};

#endif
