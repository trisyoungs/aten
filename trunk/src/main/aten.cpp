/*
	*** Aten's master structure
	*** src/main/aten.cpp
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
#include "gui/grids.h"
#include "gui/modellist.h"
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

	// Models
	modelId_ = 0;
	targetModelList_ = Aten::MainModelList;

	// Default program mode
	programMode_ = Aten::GuiMode;

	// Program control / settings (not prefs)
	typeExportMapping_ = FALSE;
	redirectedImagesActive_ = FALSE;
	redirectedImageFormat_ = "";
	redirectedImageCount_ = 0;
	maxRedirectedImages_ = 100;

	// Misc 
	#ifdef _WIN32
	homeDir_ = "C:\\";
	atenDir_ = "aten";
	#else
	homeDir_ = "/tmp";
	atenDir_ = ".aten";
	#endif
	nFiltersFailed_ = 0;
	dataDirSet_ = FALSE;

	// Clipboards
	userClipboard = new Clipboard;
	gridClipboard_ = NULL;

	// Single-shot mode variables
	exportFilter_ = NULL;

	// Fragments
	fragmentModelId_ = 0;

	// Pointer to AtenWindow
	atenWindow_ = NULL;
}

// Destructor
Aten::~Aten()
{
	clear();
	delete userClipboard;
	if (gridClipboard_ != NULL) delete gridClipboard_;
}

// Clear
void Aten::clear()
{
	models_.clear();
	forcefields_.clear();
	userClipboard->clear();
	scripts_.clear();
	for (int i=0; i<FilterData::nFilterTypes; i++) filters_[i].clear();
}

// Return the current program mode
Aten::ProgramMode Aten::programMode() const
{
	return programMode_;
}

/*
// Program Control / Settings (not prefs)
*/

// Set whether type export conversion is enabled
void Aten::setTypeExportMapping(bool b)
{
	typeExportMapping_ = b;
}

// Return whether type export conversion is enabled
bool Aten::typeExportMapping() const
{
	return typeExportMapping_;
}

// Convert supplied type name according to export type map
const char* Aten::typeExportConvert(const char* oldname) const
{
	if (!typeExportMapping_) return oldname;
	KVPair *kvp = typeExportMap.search(oldname);
	return (kvp == NULL ? oldname : kvp->value());
}

// Return whether saveImage redirect is active (for scripted movie making)
bool Aten::redirectedImagesActive()
{
	return redirectedImagesActive_;
}

// Initialise image redirection
void Aten::initialiseImageRedirect(const char* filenameFormat, int maxImages)
{
	redirectedImageCount_ = 0;
	maxRedirectedImages_ = maxImages;
	redirectedImageFormat_ = filenameFormat;
	redirectedImagesActive_ = TRUE;
	Messenger::print(Messenger::Verbose, "Image redirect active - name format = [%s], maxImages = %i", redirectedImageFormat_.get(), maxRedirectedImages_);
}

// Return next filename for image redirection
const char* Aten::nextRedirectedFilename()
{
	if (redirectedImageCount_ == maxRedirectedImages_) return NULL;
	static Dnchar filename;
	filename.sprintf(redirectedImageFormat_.get(), redirectedImageCount_++);
	return filename.get();
}

// Cancel image redirection
int Aten::cancelImageRedirect()
{
	redirectedImagesActive_ = FALSE;
	return redirectedImageCount_;
}

/*
// Locations
*/

// Set location of users's home directory
void Aten::setHomeDir(const char* path)
{
	homeDir_ = path;
}

// Return the home directory path
const char* Aten::homeDir() const
{
	return homeDir_.get();
}

// Set working directory
void Aten::setWorkDir(const char* path)
{
	workDir_ = path;
}

// Return the working directory path
const char* Aten::workDir() const
{
	return workDir_.get();
}

// Set data directory
void Aten::setDataDir(const char* path)
{
	dataDir_ = path;
	dataDirSet_ = TRUE;
}

// Return the data directory path
const char* Aten::dataDir() const
{
	return dataDir_.get();
}

// Return whether the data dir has already been set
bool Aten::dataDirSet() const
{
	return dataDirSet_;
}

// Return the aten directory
const char* Aten::atenDir() const
{
	return atenDir_.get();
}

/*
// Grid clipboard functions
*/

// Copy specified grid
void Aten::copyGrid(Grid* g)
{
	// If there is an old grid here, delete it first
	if (gridClipboard_ != NULL) delete gridClipboard_;
	gridClipboard_ = NULL;
	if (g != NULL)
	{
		gridClipboard_ = new Grid;
		*gridClipboard_ = *g;
	}
}

// Return grid on clipboard (if any)
Grid* Aten::gridClipboard()
{
	return gridClipboard_;
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

/*
 * Current Objects
 */

// Return current object Bundle
Bundle& Aten::current()
{
	return current_;
}
