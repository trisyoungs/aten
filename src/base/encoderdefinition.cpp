/*
	*** Encoder Definition
	*** src/base/encoderdefinition.cpp
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

#include "base/encoderdefinition.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Encoder Definition Keywords
const char* EncoderDefinitionKeywords[] = { "Command", "CommandArguments", "CommandName", "CommandSearchPaths", "Name", "Nickname" };
EncoderDefinition::EncoderDefinitionKeyword EncoderDefinition::encoderDefinitionKeyword(QString s, bool reportError)
{
	EncoderDefinition::EncoderDefinitionKeyword edk = (EncoderDefinition::EncoderDefinitionKeyword) enumSearch("encoder definition keyword", EncoderDefinition::nEncoderDefinitionKeywords, EncoderDefinitionKeywords, s, reportError);
	if ((edk == EncoderDefinition::nEncoderDefinitionKeywords) && reportError) enumPrintValid(EncoderDefinition::nEncoderDefinitionKeywords,EncoderDefinitionKeywords);
	return edk;
}
const char* EncoderDefinition::encoderDefinitionKeyword(EncoderDefinition::EncoderDefinitionKeyword edk)
{
	return EncoderDefinitionKeywords[edk];
}

// Constructor
EncoderDefinition::EncoderDefinition() : ListItem<EncoderDefinition>()
{
}

/*
 * Definition
 */

// Set name
void EncoderDefinition::setName(QString name)
{
	name_ = name;
}

// Return name
QString EncoderDefinition::name()
{
	return name_;
}

// Set nickname
void EncoderDefinition::setNickname(QString nickname)
{
	nickname_ = nickname;
}

// Nickname
QString EncoderDefinition::nickname()
{
	return nickname_;
}

 /*
  * Commands
  */

// Add new command definition to encoder
ExternalCommand* EncoderDefinition::addCommand()
{
	return commands_.add();
}

// Return first defined command
ExternalCommand* EncoderDefinition::commands()
{
	return commands_.first();
}
