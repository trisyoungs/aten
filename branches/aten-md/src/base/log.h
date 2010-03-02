/*
	*** Log Class
	*** src/base/log.h
	Copyright T. Youngs 2007-2009

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ATEN_LOG_H
#define ATEN_LOG_H

// Log Class
class Log
{
	public:
	// Constructor
	Log();
	// Operator == (test equivalence of all log values except Total)
	bool operator==(Log &l) const;
	// Log types
	enum LogType { Structure, Coordinates, Visual, Selection, Camera, Glyphs, Misc, Total, nLogTypes };
	// Structure  : create/destroy atoms/bonds, change elements
	// Coordinate : atomic coordinates
	// Visual     : visual changes that require re-rendering
	// Selection  : atom selection
	// Camera     : view (mainly used to flag reprojection)
	// Glyph      : glyphs
	// Misc       : miscellaneous changes (e.g. title changes)
	// Total      : sum of all changes

	private:
	// Integer 'logs' of model changes
	int logs_[Log::nLogTypes];
	// Log point of the last save / point on load
	int savePoint_;
	// Return current 'save' point of the model (sum of specific logs)
	int currentSavePoint() const;

	public:
	// Reset all logs to zero
	void reset();
	// Increment specified log
	void add(Log::LogType);
	// Set a log to a specific value
	void setLog(Log::LogType lt, int value);
	// Return the log quantity specified
	int log(Log::LogType cl) const;
	// Set the save point log for the model
	void updateSavePoint();
	// Return if the model has been modified since last being saved
	bool isModified() const;
	// Print logs
	void print() const;
};

#endif
