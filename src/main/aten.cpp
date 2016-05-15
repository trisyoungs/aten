/*
	*** Aten's master structure
	*** src/main/aten.cpp
	Copyright T. Youngs 2007-2016

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
#include "gui/mainwindow.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "ff/forcefield.h"
#include "base/grid.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "parser/aten.h"
#include "parser/commandnode.h"
#include "parser/variable.h"
#include "parser/tree.h"
#include "parser/parser.h"

ATEN_USING_NAMESPACE

// Constructor
Aten::Aten() : commands_(*this)
{
	// Set Aten pointers in dependent classes
	CommandNode::setAten(this);
	CommandParser::setAten(this);
	Variable::setAten(this);
	Tree::setAten(this);
	Elements().setAten(this);

	// Models
	modelId_ = 0;
	targetModelList_ = Aten::MainModelList;

	// Default program mode
	programMode_ = Aten::GuiMode;

	// Program control / settings (not prefs)
	typeExportMapping_ = false;

	// Clipboards
	userClipboard = new Clipboard;
	gridClipboard_ = NULL;

	// Single-shot mode variables
	exportFilter_ = NULL;

	// Fragments
	fragmentModelId_ = 0;
	fragmentBondId_ = 0;
	currentFragment_ = 0;

	// Partitioning schemes
	poresPartitioningScheme_.initialiseAbsolute("Generated Scheme", "Scheme generated from model pores");

	// Pointer to AtenWindow
	atenWindow_ = NULL;
}

// Destructor
Aten::~Aten()
{
	// User data
	models_.clear();
	forcefields_.clear();
	scripts_.clear();

	// Filters
	for (int i=0; i<FilterData::nFilterTypes; i++) filters_[i].clear();

	// Clipboards
	userClipboard->clear();
	delete userClipboard;
	if (gridClipboard_ != NULL) delete gridClipboard_;
}

// Set pointer to AtenWindow
void Aten::setAtenWindow(AtenWindow* atenWindow)
{
	atenWindow_ = atenWindow;
}

// Return pointer to AtenWindow
AtenWindow* Aten::atenWindow()
{
	return atenWindow_;
}
