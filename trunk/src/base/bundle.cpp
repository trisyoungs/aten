/*
	*** Pointer Bundle
	*** src/base/Bundle.cpp
	Copyright T. Youngs 2007-2010

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

#include "base/bundle.h"
//#include "model/model.h"
#include "base/constants.h"
#include <stdio.h>

// Constructor
Bundle::Bundle()
{
	m = 0;
	rs = 0;
	s = 0;
	ff = 0;
	g = 0;
	i = 0;
	p = 0;
	gl = 0;
}

// Check for non-null pointers
bool Bundle::isNull(int ptrs)
{
	if ((ptrs&Bundle::AtomPointer) && (i == 0)) return TRUE;
	if ((ptrs&Bundle::PatternPointer) && (p == 0)) return TRUE;
	if ((ptrs&Bundle::ModelPointer) && (m == 0)) return TRUE;
	if ((ptrs&Bundle::GridPointer) && (g == 0)) return TRUE;
	if ((ptrs&Bundle::SitePointer) && (s == 0)) return TRUE;
	if ((ptrs&Bundle::ForcefieldPointer) && (ff == 0)) return TRUE;
	if ((ptrs&Bundle::GlyphPointer) && (gl == 0)) return TRUE;
	return FALSE;
}

// Notify of non-null pointers
bool Bundle::notifyNull(int ptrs)
{
	if ((ptrs&Bundle::AtomPointer) && (i == 0))
	{
		printf("--> No active atom.\n");
		return TRUE;
	}
	if ((ptrs&Bundle::PatternPointer) && (p == 0))
	{
		printf("--> No active pattern.\n");
		return TRUE;
	}
	if ((ptrs&Bundle::ModelPointer) && (m == 0))
	{
		printf("--> No active model.\n");
		return TRUE;
	}
	if ((ptrs&Bundle::GridPointer) && (g == 0))
	{
		printf("--> No active grid.\n");
		return TRUE;
	}
	if ((ptrs&Bundle::SitePointer) && (s == 0))
	{
		printf("--> No active site.\n");
		return TRUE;
	}
	if ((ptrs&Bundle::ForcefieldPointer) && (ff == 0))
	{
		printf("--> No active forcefield.\n");
		return TRUE;
	}
	if ((ptrs&Bundle::GlyphPointer) && (gl == 0))
	{
		printf("--> No active glyph.\n");
		return TRUE;
	}
	return FALSE;
}
