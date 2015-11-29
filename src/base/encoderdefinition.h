/*
	*** Encoder Definition
	*** src/base/encoderdefinition.h
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

#ifndef ATEN_ENCODERDEFINITION_H
#define ATEN_ENCODERDEFINITION_H

#include <QString>
#include "base/externalcommand.h"
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Encoder Definition
class EncoderDefinition : public ListItem<EncoderDefinition>
{
	public:
	// Constructor / Destructor
	EncoderDefinition();
	// Definition Keywords
	enum EncoderDefinitionKeyword { CommandKeyword, CommandArgumentsKeyword, CommandNameKeyword, CommandSearchPathsKeyword, NameKeyword, NicknameKeyword, nEncoderDefinitionKeywords };
	static EncoderDefinition::EncoderDefinitionKeyword encoderDefinitionKeyword(QString s, bool reportError = false);
	static const char* encoderDefinitionKeyword(EncoderDefinition::EncoderDefinitionKeyword edk);


	/*
	 * Definition
	 */
	private:
	// Name
	QString name_;
	// Nickname
	QString nickname_;

	public:
	// Set name
	void setName(QString name);
	// Return name
	QString name();
	// Set nickname
	void setNickname(QString nickname);
	// Nickname
	QString nickname();


	/*
	 * Commands
	 */
	private:
	// Command list
	List<ExternalCommand> commands_;

	public:
	// Add new command definition to encoder
	ExternalCommand* addCommand();
	// Return first defined command
	ExternalCommand* commands();
	// Return number of defined commands
	int nCommands() const;
};

ATEN_END_NAMESPACE

#endif

