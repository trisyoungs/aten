/*
        *** Method Plugin Interface
        *** src/plugins/interfaces/methodplugin.h
        Copyright T. Youngs 2007-2018

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

#ifndef ATEN_METHODPLUGININTERFACE_H
#define ATEN_METHODPLUGININTERFACE_H

#include "plugins/interfaces/baseplugin.h"
#include "parser/returnvalue.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "base/grid.h"
#include "base/kvmap.h"
#include "base/forcefieldatom.h"
#include "base/messenger.h"
#include "base/fileparser.h"
#include "base/namespace.h"
#include "templates/reflist.h"
#include <QStringList>
#include <QtPlugin>
#include <QFileInfo>
#include <QCoreApplication>

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class Model;

// Method Plugin Interface
class MethodPluginInterface : public BasePluginInterface
{
	public:
	// Constructor
	MethodPluginInterface()
	{
	}
	// Destructor
	virtual ~MethodPluginInterface() {}


	/*
	 * Instance Handlind
	 */
	private:
	// Return a copy of the plugin object (provided by main plugin)
	virtual BasePluginInterface* makeCopy() const = 0;

	public:
	// Return a duplicate of the plugin object, including options etc.
	BasePluginInterface* duplicate() const
	{
		MethodPluginInterface* copy = (MethodPluginInterface*) makeCopy();
		copy->setPluginStore(pluginStore_); 
		copy->setOptions(pluginOptions_);
		return copy;
	}


	/*
	 * Object Handling
	 */
	private:
	// Target model for method, if any
	Model* targetModel_;

	public:
	// Set target model
	void setTargetModel(Model* model)
	{
		targetModel_ = model;
	}
	// Return target model
	Model* targetModel() const
	{
		return targetModel_;
	}


	/*
	 * Method
	 */
	private:
	// List of specific temporary files owned / used by this method
	QStringList temporaryFiles_;
	// List of specific temporary file prefixes used by this method
	QStringList temporaryFilePrefixes_;

	protected:
	// Return name of suitable temporary file prefix for use by the method
	QString addTemporaryFilePrefix(QString startsWith = "AtenPluginFile")
	{
		QString temporaryFilePrefix;
		QStringList entryList = prefs.tempDir().entryList();
		bool match;
		do
		{
			match = false;
			temporaryFilePrefix = prefs.tempDir().absoluteFilePath(QString("%1-%2-%3").arg(startsWith).arg(QCoreApplication::applicationPid()).arg(AtenMath::randomimax()));
			for (int n=0; n<entryList.count(); ++n)
			{
				if (entryList.at(n).startsWith(temporaryFilePrefix))
				{
					match = true;
					break;
				}
			}
		} while (match);

		temporaryFilePrefixes_ << temporaryFilePrefix;

		return temporaryFilePrefix;
	}
	// Return name of suitable temporary file for use by the method
	QString addTemporaryFile(QString startsWith = "AtenPluginFile", QString extension = "dat")
	{
		QString temporaryFile;
		do
		{
			temporaryFile = prefs.tempDir().absoluteFilePath(QString("%1-%2-%3.%4").arg(startsWith).arg(QCoreApplication::applicationPid()).arg(AtenMath::randomimax()).arg(extension));
		} while (QFile::exists(temporaryFile));

		temporaryFiles_ << temporaryFile;

		return temporaryFile;
	}
	// Run the method
	virtual bool runMethod() = 0;

	public:
	// Execute method
	bool executeMethod(bool keepTemporaryFiles = false)
	{
		bool result = runMethod();

		// Clean up all temporary files we know about
		if (!keepTemporaryFiles)
		{
			// Remove specific temporary files
			for (int n=0; n<temporaryFiles_.count(); ++n)
			{
				QFile temporaryFile(temporaryFiles_.at(n));
				if (temporaryFile.exists()) temporaryFile.remove();
				else Messenger::warn("Temporary file '%s' could not be removed - perhaps it was never used?", qPrintable(temporaryFiles_.at(n)));
			}

			// Remove prefixes
			for (int n=0; n<temporaryFilePrefixes_.count(); ++n)
			{
				QStringList entryList = prefs.tempDir().entryList();
				QString filePrefix = temporaryFilePrefixes_.at(n);

				// Find all files in the current dir starting with this string, and delete them....
				for (int n=0; n<entryList.count(); ++n)
				{
					if (entryList.at(n).startsWith(filePrefix))
					{
						Messenger::print("Cleanup - Removing file '%s'...", qPrintable(entryList.at(n)));
						QFile::remove(prefs.tempDir().absoluteFilePath(entryList.at(n)));
					}
				}
			}
		}

		return result;
	}


	/*
	 * Options
	 */
	public:
	// Return whether the plugin has options
	virtual bool hasOptions() = 0;
	// Show options dialog
	virtual bool showOptionsDialog() = 0;
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define MethodPluginInterface_iid "com.projectaten.Aten.MethodPluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::MethodPluginInterface, MethodPluginInterface_iid)

#endif
