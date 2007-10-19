/*
	*** Monte Carlo molecule component
	*** src/classes/component.cpp
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

#include "classes/component.h"
#include "base/constants.h"

// Constructor
component::component()
{
	// Set initial values for component
	compmodel = NULL;
	comppattern = NULL;
	nrequested = 0;
	nfilled = 0;
	id = 0;
	prev = NULL;
	next = NULL;
	allowed_moves[MT_INSERT] = TRUE;
	allowed_moves[MT_DELETE] = FALSE;
	allowed_moves[MT_TRANSLATE] = TRUE;
	allowed_moves[MT_ROTATE] = TRUE;
	allowed_moves[MT_ZMATRIX] = FALSE;
	#ifdef MEMDEBUG
	memdbg.create[MD_COMPONENT] ++;
	#endif
}

// Destructor
component::~component()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_COMPONENT] ++;
	#endif
}
