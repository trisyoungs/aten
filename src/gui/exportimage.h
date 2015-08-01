/*
	*** Export Image Dialog
	*** src/gui/exportimage.h
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

#ifndef ATEN_ATENEXPORTIMAGE_H
#define ATEN_ATENEXPORTIMAGE_H

#include "gui/ui_exportimage.h"
#include <QDialog>
#include <QDir>

// Forward Declarations (Qt)
class AtenWindow;

class AtenExportImage : public QDialog
{
	// All Qt declarations must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenExportImage(AtenWindow& parent);
	~AtenExportImage();
	// Main form declaration
	Ui::AtenExportImage ui;
	// Reference to main window
	AtenWindow& atenWindow_;

	private:
	// Whether the dialog is still awaiting it's first show
	bool firstShow_;
	// Current aspect ratio
	double aspectRatio_;

	public:
	// Call dialog to get/update image save information
	bool getImageDetails();
	// Return selected filename
	QString fileName();


	/*
	 * Slots
	 */
	private slots:
	void on_SelectFileNameButton_clicked(bool checked);
	void on_ImageWidthSpin_valueChanged(int value);
	void on_MaintainAspectRatioCheck_toggled(bool checked);
	void on_SaveImageButton_clicked(bool checked);
	void on_CancelButton_clicked(bool checked);
};

#endif
