/*
        *** Common functions for MOPAC plugins
        *** src/plugins/io_mopac/common.cpp
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

#include "plugins/interfaces/fileplugin.h"
#include "plugins/io_mopac/common.h"
#include "model/model.h"
#include "base/fileparser.h"

ATEN_USING_NAMESPACE

// Read single XYZ model from file
bool MOPACFilePluginCommon::readMOPACModel(FilePluginInterface* interface, FileParser& parser, const FilePluginStandardImportOptions standardOptions, Model* targetModel)
{
	// Discard keyword lines.
	// A '+' in a line indicates that another line of keywords will follow.
	// A '&' in a line indicates that a title/description line is replaced by keywords.
	QString line;
	int nTitleLines = 2;
	do
	{
		if (!parser.readLine(line)) return false;
		if (line.contains("&") == 0)
		{
			--nTitleLines;
			continue;
		}
		if (line.contains("+") != 0)
		{
			// Read another line of keywords
			continue;	
		}
		break;
	} while (!parser.eofOrBlank());

	// Description lines follow, unless replaced by keyword lines with '&' in previous lines
	QString title = "Mopac Archive";
	if (nTitleLines > 0)
	{
		if (!parser.readLine(title)) return false;
	}
	if (nTitleLines > 1) parser.skipLines(1);
	targetModel->setName(title);

	// All remaining lines make up the geometry
	ParseFormat atomFormat("%3s  %13f   %13f   %13f   %f");
	Matrix cell;
	int cellCount = 0;
	Atom* i;
	while (!parser.eofOrBlank())
	{
		if (!parser.parseLine(atomFormat)) return false;

		// If the first argument is 'Tv' its part of the cell specification
		if (parser.argc(0) == " Tv")
		{
			if (cellCount < 3) cell.setColumn(cellCount, parser.arg3d(1), 0.0);
			else printf("Extra translation vector data found - ignored...\n");
			++cellCount;
		}
		else
		{
			i = interface->createAtom(targetModel, parser.argc(0), parser.arg3d(1));
			i->setCharge(parser.argd(4));
		}
	}

	// Were we supplied with a full cell description?
	if (cellCount == 3) targetModel->cell().set(cell);

	// Rebond atoms
	if (!standardOptions.preventRebonding()) targetModel->calculateBonding(true);

	return true;
}
