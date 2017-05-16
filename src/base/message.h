/*
	*** Message
	*** src/base/message.h
	Copyright T. Youngs 2007-2017

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

#ifndef ATEN_MESSAGE_H
#define ATEN_MESSAGE_H

#include "base/namespace.h"
#include <QString>
#include <QColor>

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// Message in message list
class Message
{
	public:
	// Message types
	enum MessageType { NormalMessage, WarningMessage, ErrorMessage, nMessageTypes };
	// Return colour associated to message type
	static QColor messageColour(MessageType type);
	// Set normal message colour
	static void setNormalMessageColour(double colour[4]);
	// Constructor
	Message(QString text, Message::MessageType = Message::NormalMessage);

	private:
	// Message text
	QString text_;
	// Message type
	MessageType type_;

	public:
	// Set message text and type
	void set(QString text, Message::MessageType type = Message::NormalMessage);
	// Return message text
	const QString& text() const;
	// Return message colour
	QColor colour() const;
};

ATEN_END_NAMESPACE

#endif
