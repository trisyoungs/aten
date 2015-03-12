/*
	*** Aten Prefs Routines
	*** src/main/prefs.cpp
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
#include "parser/prefs.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Load user preferences file
bool Aten::loadPrefs()
{
	Messenger::enter("Aten::loadPrefs");
	Dnchar fileName;
	ReturnValue rv;
	bool result, found;

	// Program preferences
	found = FALSE;
	fileName.sprintf("%s%c%s%cprefs.dat", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP);
	Messenger::print("Looking for program preferences file '%s'...", fileName.get());
	if (fileExists(fileName)) found = TRUE;
	else
	{
		// Try .txt extension instead
		fileName.sprintf("%s%c%s%cprefs.txt", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP);
		Messenger::print("Looking for program preferences file '%s'...", fileName.get());
		if (fileExists(fileName)) found = TRUE;
	}
	if (found)
	{
		Messenger::print("Program preferences file found in '%s'", fileName.get());
		Program prefscmds;
		result = prefscmds.generateFromFile(fileName, "Program Preferences");
		if (result) result = prefscmds.execute(rv);
	}
	else Messenger::print("Program preferences file not found.");

	// User preferences
	found = FALSE;
	fileName.sprintf("%s%c%s%cuser.dat", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP);
	Messenger::print("Looking for user preferences file '%s'...", fileName.get());
	if (fileExists(fileName)) found = TRUE;
	else
	{
		// Try .txt extension instead
		fileName.sprintf("%s%c%s%cuser.txt", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP);
		Messenger::print("Looking for user preferences file '%s'...", fileName.get());
		if (fileExists(fileName)) found = TRUE;
	}
	if (found)
	{
		Messenger::print("User preferences file found in '%s'", fileName.get());
		Program prefscmds;
		result = prefscmds.generateFromFile(fileName, "User Preferences");
		if (result) result = prefscmds.execute(rv);
	}
	else Messenger::print("User preferences file not found.");
	Messenger::exit("Aten::loadPrefs");
	return TRUE;
}

// Save user preferences file
bool Aten::savePrefs(const char* fileName)
{
	Messenger::enter("Aten::savePrefs");
	bool result = TRUE;
	Dnchar line;
	int n, i;
	LineParser prefsfile;
	prefsfile.openOutput(fileName, TRUE);
	if (prefsfile.isFileGoodForWriting())
	{
		// First - loop over all element data, comparing it to the stored default values
		prefsfile.writeLine("// Element Data\n");
		for (n=0; n<Elements().nElements(); ++n)
		{
			// Ambient Colour
			for (i = 0; i<4; ++i) if (Elements().defaultEl[n].colour[i] != Elements().el[n].colour[i]) break;
			if (i != 4)
			{
				line.sprintf("aten.elements[%s].colour = { %f, %f, %f, %f };\n", Elements().el[n].symbol, Elements().el[n].colour[0], Elements().el[n].colour[1], Elements().el[n].colour[2], Elements().el[n].colour[3]);
				prefsfile.writeLine(line);
			}
			// Atomic radius
			if (Elements().defaultEl[n].atomicRadius != Elements().el[n].atomicRadius)
			{
				line.sprintf("aten.elements[%s].radius = %f;\n", Elements().el[n].symbol, Elements().el[n].atomicRadius);
				prefsfile.writeLine(line);
			}
		}
		// Next - for each accessor in PreferencesVariable compare the results to our local Prefs copy
		prefsfile.writeLine("// Program Preferences\n");
		Prefs defaults;
		ReturnValue rv;
		Dnchar newvalue, defaultvalue;		
		for (i = 0; i < PreferencesVariable::nAccessors; ++i)
		{
			rv.set(VTypes::PreferencesData, this);
			// Convert original new value to string representation
			if (!PreferencesVariable::retrieveAccessor(i, rv, FALSE)) continue;
			if (rv.type() == VTypes::DoubleData) newvalue.sprintf("%10.5e", rv.asDouble());
			else newvalue = rv.asString();
			
			// Convert default value to string representation
			rv.set(VTypes::PreferencesData, &defaults);
			if (!PreferencesVariable::retrieveAccessor(i, rv, FALSE)) continue;
			if (rv.type() == VTypes::DoubleData) defaultvalue.sprintf("%10.5e", rv.asDouble());
			else defaultvalue = rv.asString();

			// Compare the two strings - if different, write the prefs value to the file....
// 			printf("acc = %i [%s], default = '%s', new = '%s'\n", i, PreferencesVariable::accessorData[i].name, defaultvalue.get(), newvalue.get());
			if (strcmp(defaultvalue.get(), newvalue.get()) == 0) continue;
			if ((PreferencesVariable::accessorData[i].returnType == VTypes::StringData) && (PreferencesVariable::accessorData[i].arraySize == 0)) line.sprintf("aten.prefs.%s = \"%s\";\n", PreferencesVariable::accessorData[i].name, newvalue.get());
			else line.sprintf("aten.prefs.%s = %s;\n", PreferencesVariable::accessorData[i].name, newvalue.get());
			prefsfile.writeLine(line);
		}
	}
	else result = FALSE;
	prefsfile.closeFiles();
	Messenger::exit("Aten::savePrefs");
	return result;
}
