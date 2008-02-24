/*
	*** Extra gl2ps code
	*** src/render/gl2ps_extra.cpp
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

#include "base/sysfunc.h"

// Supported Vector Formats (mirrors #defines in src/render/gl2ps.h)
enum vector_format { VIF_PS, VIF_EPS, VIF_TEX, VIF_PDF, VIF_SVG, VIF_PGF, VIF_NITEMS };
const char *VIF_filters[VIF_NITEMS] = { "Postscript (*.ps)", "Enhanced Postscript (*.eps)", "LaTeX (*.tex)", "Protable Document Format (*.pdf)", "Scalable Vector Graphics (*.svg)", "Portable LaTeX Graphics (*.pgf)" };
const char *VIF_extensions[VIF_NITEMS] = { "ps", "eps", "tex", "pdf", "svg", "pgf" };
vector_format VIF_from_text(const char *s)
	{ return (vector_format) enum_search("vector format",VIF_NITEMS,VIF_extensions,s); }
const char *filter_from_VIF(vector_format vf)
	{ return VIF_filters[vf]; }
const char *extension_from_VIF(vector_format vf)
	{ return VIF_extensions[vf]; }
