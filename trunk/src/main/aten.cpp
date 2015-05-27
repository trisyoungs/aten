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
	typeExportMapping_ = false;
	redirectedImagesActive_ = false;
	redirectedImageFormat_ = "";
	redirectedImageCount_ = 0;
	maxRedirectedImages_ = 100;

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
QString Aten::typeExportConvert(QString oldName) const
{
	if (!typeExportMapping_) return oldName;
	KVPair* kvp = typeExportMap.search(oldName);
	return (kvp == NULL ? oldName : kvp->value());
}

// Return whether saveImage redirect is active (for scripted movie making)
bool Aten::redirectedImagesActive()
{
	return redirectedImagesActive_;
}

// Initialise image redirection
void Aten::initialiseImageRedirect(QString filenameFormat, int maxImages)
{
	redirectedImageCount_ = 0;
	maxRedirectedImages_ = maxImages;
	redirectedImageFormat_ = filenameFormat;
	redirectedImagesActive_ = true;
	Messenger::print(Messenger::Verbose, "Image redirect active - name format = [%s], maxImages = %i", qPrintable(redirectedImageFormat_), maxRedirectedImages_);
}

// Return next filename for image redirection
QString Aten::nextRedirectedFilename()
{
	if (redirectedImageCount_ == maxRedirectedImages_) return NULL;
	QString filename;
	filename.sprintf(qPrintable(redirectedImageFormat_), redirectedImageCount_++);
	return filename;
}

// Cancel image redirection
int Aten::cancelImageRedirect()
{
	redirectedImagesActive_ = false;
	return redirectedImageCount_;
}

/*
 * Locations
 */

// Return the home directory path
QDir Aten::homeDir() const
{
	return homeDir_;
}

// Return the working directory path
QDir Aten::workDir() const
{
	return workDir_;
}

// Return the data directory path
QDir Aten::dataDir() const
{
	return dataDir_;
}

// Return full path of file in data directory
QString Aten::dataDirectoryFile(QString filename)
{
	return QDir::toNativeSeparators(dataDir_.absoluteFilePath(filename));
}

// Return full path of file in user's Aten directory
QString Aten::atenDirectoryFile(QString filename)
{
	QDir atenDir = homeDir_.absoluteFilePath(atenDirName_);
	return QDir::toNativeSeparators(atenDir.absoluteFilePath(filename));
}

// Set/get necessary directories
void Aten::setDirectories()
{
	Messenger::enter("Aten::setDirectories()");

	// User's home directory
#ifdef _WIN32
	if (getenv("USERPROFILE") != '\0') homeDir_ = getenv("USERPROFILE");
	else homeDir_ = "C:\\";
#else
	if (getenv("HOME") != '\0') homeDir_ = getenv("HOME");	
	else homeDir_ = "/tmp";
#endif

	// Name of user's aten directory in user's home directory
#ifdef _WIN32
	atenDirName_ = "aten";
#else
	atenDirName_ = ".aten";
#endif

	// Working directory
	workDir_ = getenv("PWD");


	// Construct a list of possible paths, including the current value of dataDir_
	QStringList paths;
	if (dataDir_ != QDir()) paths << dataDir_.path();
	else if (getenv("ATENDATA") != '\0') paths << getenv("ATENDATA");
	paths << "/usr/share/aten";
	paths << "/usr/local/share/aten";
	paths << "../share/aten";
	paths << QApplication::applicationDirPath() + "/../share/aten";
	paths << QApplication::applicationDirPath() + "/../SharedSupport";

	// Check each one until we find one that exists
	for (int i=0; i < paths.size(); ++i)
	{
		QDir path = paths.at(i);
		Messenger::print("Checking for data directory '%s'...", qPrintable(path.absolutePath()));
		if (path.exists())
		{
			Messenger::print("Data directory exists at '%s' - using this path...", qPrintable(path.absolutePath()));
			dataDir_ = path;

			Messenger::enter("Aten::setDataDir()");
			return;
		}
	}
	Messenger::print("No valid data directory found in any location.");

	Messenger::enter("Aten::setDirectories()");
}

/*
 * Grid clipboard functions
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
