/*
	*** Aten Prefs Routines
	*** src/main/prefs.cpp
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
#include "parser/prefs.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Dump element information to specified LineParser
void Aten::dumpElementInfo(LineParser& parser)
{
	if (!parser.isFileGoodForWriting()) return;

	QString line;

	// Loop over all element data, comparing it to the stored default values
	parser.writeLine("\n// Element Data\n");
	for (int n=0; n<Elements().nElements(); ++n)
	{
		// Ambient Colour
		if (Elements().colourHasChanged(n))
		{
			double colour[4];
			Elements().copyColour(n, colour);
			line.sprintf("aten.elements[%s].colour = { %f, %f, %f, %f };\n", Elements().symbol(n), colour[0], colour[1], colour[2], colour[3]);
			parser.writeLine(line);
		}

		// Atomic radius
		if (Elements().radiusHasChanged(n))
		{
			line.sprintf("aten.elements[%s].radius = %f;\n", Elements().symbol(n), Elements().atomicRadius(n));
			parser.writeLine(line);
		}
	}
}

// Dump preferences to specified LineParser
void Aten::dumpPreferences(LineParser& parser)
{
	if (!parser.isFileGoodForWriting()) return;

	QString line;

	// For each accessor in PreferencesVariable compare the results to our local Prefs copy
	parser.writeLine("// Program Preferences\n");
	Prefs defaults;
	ReturnValue rv;
	QString newValue, defaultValue;
	bool success;
	for (int i = 0; i < PreferencesVariable::nAccessors; ++i)
	{

		// Treat some accessors as special cases
		if (i == PreferencesVariable::ColourScales)
		{
			// Loop over defined colourscales
			for (int n=0; n<10; ++n)
			{
				if (prefs.colourScale[n] == defaults.colourScale[n]) continue;

				// Write out colourscale creation data
				parser.writeLineF("aten.prefs.colourScales[%i].clear();\n", n);
				for (ColourScalePoint* csp = prefs.colourScale[n].firstPoint(); csp != NULL; csp = csp->next)
				{
					parser.writeLineF("aten.prefs.colourScales[%i].addPoint(%f, %f, %f, %f);\n", n, csp->value(), csp->colour()[0], csp->colour()[1], csp->colour()[2], csp->colour()[3]);
				}
			}
		}
		else
		{
			// Convert current prefs value to string representation
			rv.set(VTypes::PreferencesData, &prefs);
			if (!PreferencesVariable::retrieveAccessor(i, rv, false)) continue;
			newValue = rv.asString();

			// Convert default value to string representation
			rv.set(VTypes::PreferencesData, &defaults);
			if (!PreferencesVariable::retrieveAccessor(i, rv, false)) continue;
			defaultValue = rv.asString();

			// Compare the two strings - if different, write the prefs value to the file....
// 				printf("acc = %i [%s], default = '%s', new = '%s'\n", i, qPrintable(PreferencesVariable::accessorData[i].name), qPrintable(defaultValue), qPrintable(newValue));
			if (defaultValue == newValue) continue;
			if ((PreferencesVariable::accessorData[i].returnType == VTypes::StringData) && (PreferencesVariable::accessorData[i].arraySize == 0)) line.sprintf("aten.prefs.%s = \"%s\";\n", qPrintable(PreferencesVariable::accessorData[i].name), qPrintable(newValue));
			else line.sprintf("aten.prefs.%s = %s;\n", qPrintable(PreferencesVariable::accessorData[i].name), qPrintable(newValue));
			parser.writeLine(line);
		}
	}
}

// Load user preferences file
bool Aten::loadPrefs()
{
	Messenger::enter("Aten::loadPrefs");
	QFileInfo fileInfo;
	ReturnValue rv;
	bool result, found;

	// Aten's preferences
	found = false;
	QString filename = atenDirectoryFile("prefs.dat");
	fileInfo.setFile(filename);
	Messenger::print("Looking for user preferences file '%s'...", qPrintable(filename));
	if (fileInfo.exists())
	{
		Messenger::print("Program preferences file found at '%s'", qPrintable(filename));
		Program prefsCmds;
		result = prefsCmds.generateFromFile(filename, "Program Preferences");
		if (result) result = prefsCmds.execute(rv);
	}
	else Messenger::print("User preferences file not found.");

	// Program preferences
	found = false;
	filename = atenDirectoryFile("user.dat");
	fileInfo.setFile(filename);
	Messenger::print("Looking for user preferences file '%s'...", qPrintable(filename));
	if (fileInfo.exists()) found = true;
	else
	{
		// Try .txt extension instead
		filename = atenDirectoryFile("user.txt");
		fileInfo.setFile(filename);
		Messenger::print("Looking for user preferences file '%s'...", qPrintable(filename));
		if (fileInfo.exists()) found = true;
	}
	if (found)
	{
		Messenger::print("User preferences file found in '%s'", qPrintable(filename));
		Program prefsCmds;
		result = prefsCmds.generateFromFile(filename, "User Preferences");
		if (result) result = prefsCmds.execute(rv);
	}
	else Messenger::print("User preferences file not found.");

	Messenger::exit("Aten::loadPrefs");
	return true;
}

// Save user preferences file#
bool Aten::savePrefs(QString fileName)
{
	Messenger::enter("Aten::savePrefs");

	bool result = true;
	QString line;
	int n, i;
	LineParser prefsfile;
	prefsfile.openOutput(fileName, true);
	if (prefsfile.isFileGoodForWriting())
	{
		dumpElementInfo(prefsfile);

		dumpPreferences(prefsfile);
	}
	else result = false;
	prefsfile.closeFiles();

	Messenger::exit("Aten::savePrefs");
	return result;
}
