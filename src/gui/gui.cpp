/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007-2015

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

#include "main/aten.h"
#include "main/version.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/selectpattern.h"
#include "gui/selectvariable.h"
#include "gui/selectelement.h"
#include "gui/progress.h"
#include "gui/about.h"
#include "gui/ffeditor.h"
#include "gui/tcanvas.uih"
#include "gui/disorderwizard.h"
#include "gui/atomlist.h"
#include "gui/build.h"
#include "gui/celldefinition.h"
#include "gui/celltransform.h"
#include "gui/command.h"
#include "gui/forcefields.h"
#include "gui/fragments.h"
#include "gui/geometry.h"
#include "gui/glyphs.h"
#include "gui/grids.h"
#include "gui/messages.h"
#include "gui/modellist.h"
#include "gui/pores.h"
#include "gui/position.h"
#include "gui/scriptmovie.h"
#include "gui/select.h"
#include "gui/trajectory.h"
#include "gui/transform.h"
#include "gui/vibrations.h"
#include "gui/viewbasis.h"
#include "gui/vieweigenvector.h"
#include "gui/zmatrix.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include <QtGui/QMessageBox>
#include <QtCore/QTextStream>

