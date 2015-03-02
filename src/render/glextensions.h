/*
	*** GL Extensions
	*** src/render/glextensions.h
	Copyright T. Youngs 2013-2014

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

#ifndef ATEN_GLEXTENSIONS_H
#define ATEN_GLEXTENSIONS_H

#include "templates/list.h"
#include "base/namespace.h"
#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#include "glext.h"
#elif __APPLE__
#include <OpenGL/gl3.h>
#else
// Need to include glx.h, but this includes Xlib.h which defines 'None', 'Success', and 'Unsorted', not to mention a 'typedef Atom'.
// So, enclose it in its own namespace to prevent the clash with Aten's Atom class, and undefine 'None' and 'Success' so as not to annoy Qt.
// namespace hide_X11 {
#include <GL/glext.h>
// #undef None
// #undef Success
// #undef Unsorted
// }
#endif

ATEN_BEGIN_NAMESPACE

// GL Extensions
class GLExtensions : public ListItem<GLExtensions>
{
	public:
	// Constructor
	GLExtensions();


	/*
	 * Query Extensions
	 */
	private:
	// Whether we have the query extensions
	bool hasQueries_;

	public:
	// Return whether we have the query extension
	bool hasQueries();
	// Query function pointers
	PFNGLBEGINQUERYPROC glBeginQuery;
	PFNGLENDQUERYPROC glEndQuery;
	PFNGLGENQUERIESPROC glGenQueries;
	PFNGLGETQUERYOBJECTIVPROC glGetQueryObjectiv;
	PFNGLGETQUERYOBJECTUI64VPROC glGetQueryObjectui64v;


	/*
	 * VBO Extensions
	 */
	private:
	// Whether we have the VBO extension
	bool hasVBO_;

	public:
	// Return whether we have the VBO extension
	bool hasVBO();
	// VBO function pointers
	PFNGLGENBUFFERSPROC glGenBuffers;
	PFNGLBINDBUFFERPROC glBindBuffer;
	PFNGLBUFFERDATAPROC glBufferData;
	PFNGLBUFFERSUBDATAPROC glBufferSubData;
	PFNGLDELETEBUFFERSPROC glDeleteBuffers;
};

ATEN_END_NAMESPACE

#endif
