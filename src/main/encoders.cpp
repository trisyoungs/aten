/*
	*** Aten Encoder-Specific Routines
	*** src/main/encoders.cpp
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
	QString keyword;
	EncoderDefinition::EncoderDefinitionKeyword edk;
	EncoderDefinition* encoder = NULL;
	ExternalCommand* command = NULL;
	while (!parser.eofOrBlank())
	{
		// Read first argument from file, which is our keyword
		parser.getArgsDelim(Parser::SkipBlanks + Parser::StripComments + Parser::UseQuotes);
		keyword = parser.argc(0);
		edk = EncoderDefinition::encoderDefinitionKeyword(keyword);
		if (edk == EncoderDefinition::nEncoderDefinitionKeywords)
		{
			Messenger::print("Unrecognised encoder definition keyword '%s'. Ignoring...\n", qPrintable(keyword));
			continue;
		}

		// Deal with remaining keywords
		switch (edk)
		{
			// Command Name
			case (EncoderDefinition::CommandNameKeyword):
				if (!encoder)
				{
					Messenger::error("No encoder definition yet created in which to set name data (use 'Name' keyword before all others).\n");
					continue;
				}
				command = encoder->addCommand();
				command->setName(parser.argc(1));
				break;
			// Command
			case (EncoderDefinition::CommandKeyword):
				if (!command)
				{
					Messenger::error("No command definition yet created in which to set command data (use 'CommandName' keyword before its siblings).\n");
					continue;
				}
				command->setExecutable(parser.argc(1));
				break;
			// Command arguments
			case (EncoderDefinition::CommandArgumentsKeyword):
				if (!command)
				{
					Messenger::error("No command definition yet created in which to set argument data (use 'CommandName' keyword before its siblings).\n");
					continue;
				}
				command->setArguments(parser.argc(1));
				break;
			case (EncoderDefinition::CommandSearchPathsKeyword):
				if (!command)
				{
					Messenger::error("No command definition yet created in which to set searchpath data (use 'CommandName' keyword before its siblings).\n");
					continue;
				}
				for (int n=1; n<parser.nArgs(); ++n) command->addSearchPath(parser.argc(n));
				break;
			// Name (create new object)
			case (EncoderDefinition::NameKeyword):
				encoder = encoders_.add();
				encoder->setName(parser.argc(1));
				break;
			case (EncoderDefinition::NicknameKeyword):
				if (!encoder)
				{
					Messenger::error("No encoder definition yet created in which to set data (use 'Name' keyword before all others).\n");
					continue;
				}
				encoder->setNickname(parser.argc(1));
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
