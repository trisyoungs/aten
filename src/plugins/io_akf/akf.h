/*
        *** AKF (Aten Keyword Forma) Plugin
        *** src/plugins/io_akf/akf.h
        Copyright T. Youngs 2016-2016

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

#ifndef ATEN_AKFPLUGIN_H
#define ATEN_AKFPLUGIN_H

#include "plugins/interfaces.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// AKF Model Import / Export Plugin
class AKFModelPlugin : public QObject, public FilePluginInterface
{
	Q_OBJECT
	Q_PLUGIN_METADATA(IID "com.projectaten.Aten.FilePluginInterface.v1")
	Q_INTERFACES(AtenSpace::FilePluginInterface)


	/*
	 * Core
	 */
	public:
	// Constructor
	AKFModelPlugin();
	// Destructor
	~AKFModelPlugin();
	// Return a copy of the plugin object
	FilePluginInterface* duplicate();
	// Keyword Enum
	enum AKFKeyword { AngleKeyword, AtomKeyword, BondKeyword, CellKeyword, CellMatrixKeyword, DistanceKeyword, GlyphKeyword, GridKeyword, TitleKeyword, TorsionKeyword, nKeywords };
	static AKFKeyword akfKeyword(QString s);
	static QString akfKeyword(AKFKeyword keyword);
	// Glyph Keyword Enum
	enum AKFGlyphKeyword { EndGlyphKeyword, InvisibleKeyword, RotationKeyword, TextKeyword, WireFrameKeyword, nGlyphKeywords };
	static AKFGlyphKeyword akfGlyphKeyword(QString s);
	static QString akfGlyphKeyword(AKFGlyphKeyword keyword);
	// Grid Keyword Enum
	enum AKFGridKeyword { AxesKeyword, DataKeyword, EndGridKeyword, OriginKeyword, nGridKeywords };
	static AKFGridKeyword akfGridKeyword(QString s);
	static QString akfGridKeyword(AKFGridKeyword keyword);


	/*
	 * Definition
	 */
	public:
	// Return category of plugin
	PluginTypes::FilePluginCategory category() const;
	// Return name of plugin
	QString name() const;
	// Return nickname of plugin
	QString nickname() const;
	// Return description (long name) of plugin
	QString description() const;
	// Return related file extensions
	QStringList extensions() const;
	// Return exact names
	QStringList exactNames() const;


	/*
	 * Input / Output
	 */
	public:
	// Return whether this plugin can import data
	bool canImport();
	// Import data via the supplied parser
	bool importData(FileParser& parser, const KVMap standardOptions = KVMap());
	// Return whether this plugin can export data
	bool canExport();
	// Export data via the supplied parser
	bool exportData(FileParser& parser, const KVMap standardOptions = KVMap());
};

ATEN_END_NAMESPACE

#endif
