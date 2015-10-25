/*
	*** Export Film Dialog
	*** src/gui/exportfilm.h
	Copyright T. Youngs 2013-2015

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

#ifndef ATEN_ATENEXPORTFILM_H
#define ATEN_ATENEXPORTFILM_H

#include "gui/ui_exportfilm.h"
#include <QDialog>
#include <QDir>

// Forward Declarations (Qt)
class AtenWindow;

class AtenExportFilm : public QDialog
{
	// All Qt declarations must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenExportFilm(AtenWindow& parent);
	~AtenExportFilm();
	// Main form declaration
	Ui::AtenExportFilm ui;
	// Reference to main window
	AtenWindow& atenWindow_;

	private:
	// Whether the dialog is still awaiting it's first show
	bool firstShow_;
	// Current aspect ratio
	double aspectRatio_;

	public:
	// Call dialog to get/update image save information
	bool getFilmDetails();
// 	// Return selected filename
// 	QString fileName();


	/*
	 * Slots
	 */
	private slots:
// 	void on_SelectFileNameButton_clicked(bool checked);
// 	void on_FilmWidthSpin_valueChanged(int value);
// 	void on_MaintainAspectRatioCheck_toggled(bool checked);
// 	void on_SaveFilmButton_clicked(bool checked);
// 	void on_CancelButton_clicked(bool checked);
};

#endif
