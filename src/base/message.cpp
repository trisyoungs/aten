/*
	*** Message
	*** src/base/message.cpp
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

#include "base/message.h"
#include "base/prefs.h"

ATEN_USING_NAMESPACE

// Message type colours
QColor MessageTypeColours[] = { QColor(0, 0, 0), QColor(200, 120, 0), QColor(200, 0, 0) };
QColor Message::messageColour(Message::MessageType mt)
{
	return MessageTypeColours[mt];
}
void Message::setNormalMessageColour(double colour[4])
{
	QColor newColour;
	newColour.setRedF(colour[0]);
	newColour.setGreenF(colour[1]);
	newColour.setBlueF(colour[2]);
	newColour.setAlphaF(colour[3]);
	MessageTypeColours[Message::NormalMessage] = newColour;
}

// Constructor
Message::Message(QString text, Message::MessageType type)
{
	text_ = text;
	type_ = type;
}

// Set message text and type
void Message::set(QString text, Message::MessageType type)
{
	text_ = text;
	type_ = type;
}

// Return message text
const QString& Message::text() const
{
	return text_;
}

// Return message colour
QColor Message::colour() const
{
	return messageColour(type_);
}

