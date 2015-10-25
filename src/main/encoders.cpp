/*
	*** Aten Encoder-Specific Routines
	*** src/main/encoders.cpp
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
#include "base/lineparser.h"

ATEN_USING_NAMESPACE

// Load includes
void Aten::loadEncoderDefinitions()
{
	Messenger::enter("Aten::loadEncoderDefinitions");

	QString encoderDefsFile = dataDirectoryFile("external/encoders.txt");
	Messenger::print(Messenger::Verbose, "Looking for encoder definitions (%s)...", qPrintable(encoderDefsFile));
	
	LineParser parser;
	parser.openInput(encoderDefsFile);
	if (!parser.isFileGoodForReading())
	{
		Messenger::print("Unable to open encoder definitions file.\n");
		return;
	}

	// Read in encoder definitions from file
	QString keyword, arg;
	QStringList searchPaths;
	EncoderDefinition::EncoderDefinitionKeyword edk;
	EncoderDefinition* encoder = NULL;
	ExternalCommand* command = NULL;
	while (!parser.eofOrBlank())
	{
		// Read first argument from file, which is our keyword
		parser.readNextLine(LineParser::SkipBlanks + LineParser::StripComments);
		parser.getNextArg(LineParser::SkipBlanks + LineParser::StripComments, keyword);
		parser.getRestDelim(arg);
		printf("Kwd = [%s], arg=[%s]\n", qPrintable(keyword), qPrintable(arg));
		edk = EncoderDefinition::encoderDefinitionKeyword(keyword);
		if (edk == EncoderDefinition::nEncoderDefinitionKeywords)
		{
			Messenger::print("Unrecognised encoder definition keyword '%s'. Ignoring...\n", qPrintable(keyword));
			continue;
		}

		// Deal with remaining keywords
		switch (edk)
		{
			// Command
			case (EncoderDefinition::CommandKeyword):
				if (!encoder)
				{
					Messenger::error("No encoder definition yet created in which to set data (use 'Name' keyword before all others).\n");
					continue;
				}
				command = encoder->addCommand();
				command->setArguments(arg);
				break;
			// Command arguments
			case (EncoderDefinition::CommandArgumentsKeyword):
				if (!command)
				{
					Messenger::error("No command definition yet created in which to set data (use 'Command' keyword before its siblings).\n");
					continue;
				}
				command->setArguments(arg);
				break;
			case (EncoderDefinition::CommandSearchPathsKeyword):
				if (!command)
				{
					Messenger::error("No command definition yet created in which to set data (use 'Command' keyword before its siblings).\n");
					continue;
				}
				searchPaths = arg.split(" ", QString::SkipEmptyParts);
				for (int n=0; n<searchPaths.count(); ++n) command->addSearchPath(searchPaths.at(n));
				break;
			// Name (create new object)
			case (EncoderDefinition::NameKeyword):
				encoder = encoders_.add();
				encoder->setName(arg);
				break;
			case (EncoderDefinition::NicknameKeyword):
				if (!encoder)
				{
					Messenger::error("No encoder definition yet created in which to set data (use 'Name' keyword before all others).\n");
					continue;
				}
				encoder->setNickname(arg);
				break;
		}
	}
	parser.closeFiles();

	Messenger::exit("Aten::loadEncoderDefinitions");
}

// Return list of encoder definitions
EncoderDefinition* Aten::encoders()
{
	return encoders_.first();
}