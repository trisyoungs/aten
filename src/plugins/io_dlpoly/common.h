/*
        *** Common functions for DL_POLY plugins
        *** src/plugins/io_dlpoly/common.h
        Copyright T. Youngs 2016-2016
        Copyright A. M. Elena 2016-2016

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

#ifndef ATEN_DLPOLYPLUGINCOMMON_H
#define ATEN_DLPOLYPLUGINCOMMON_H

#include "base/kvmap.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class Model;
class FileParser;

// DLP4 Plugin Common Functions
class DLPOLYPluginCommon
{
	public:
	// DL_POLY Version Enum
	enum DLPOLYVersion { DLPOLY2, DLPOLY4 };

	public:
	// Read single CONFIG model from file
	static bool readCONFIGModel(FilePluginInterface* plugin, FileParser& parser, Model* targetModel, DLPOLYVersion version, const bool inTraj=false);
	// Skip single frame model in file
	static bool skipFrameModel(FilePluginInterface* plugin, FileParser& parser, DLPOLYVersion version);
	// Write single CONFIG model to file
	static bool writeCONFIGModel(FilePluginInterface* plugin, FileParser& parser, Model* sourceModel, DLPOLYVersion version);
	// Determine whether trajectory file is unformatted
	static bool determineHISTORYFormat(FilePluginInterface* plugin, FileParser& parser, bool& isFormatted, bool& hasHeader, DLPOLYVersion version);
	// Read single unformatted frame from file
	static bool readUnformattedFrame(FilePluginInterface* plugin, FileParser& parser, Model* targetModel, DLPOLYVersion version, int integerSize, int realSize, Array<int> unformattedElements);
	// Skip unformatted frame model in file
	static bool skipUnformattedFrame(FilePluginInterface* plugin, FileParser& parser, DLPOLYVersion version, int integerSize, int realSize);
};

ATEN_END_NAMESPACE

#endif
