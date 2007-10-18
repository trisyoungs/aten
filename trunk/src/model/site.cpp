/*
	*** Model site functions
	*** src/model/site.cpp

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

#include "model/model.h"
#include "classes/site.h"

// Find site by name
site *model::find_site(const char *s)
{
	dbg_begin(DM_CALLS,"model::find_site");
	site *result = NULL;
	for (site *xsite = sites.first(); xsite != NULL; xsite = xsite->next)
		if (strcmp(xsite->get_name(),s) == 0) result = xsite;
	dbg_end(DM_CALLS,"model::find_site");
	return result;
}
