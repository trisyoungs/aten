/*
	*** Qt GUI: Glyphs Window
	*** src/gui/glyphs.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_GLYPHSWINDOW_H
#define ATEN_GLYPHSWINDOW_H

#include "gui/ui_glyphs.h"

// Program preferences window
class AtenGlyphs : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	private slots:
	void dialogFinished(int result);

	/*
	// Local variables
	*/
	private:
	// Whether widget is refreshing
	bool refreshing_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenGlyphs(QWidget *parent = 0);
	~AtenGlyphs();
	// Main form declaration
	Ui::GlyphsDialog ui;
};

#endif
