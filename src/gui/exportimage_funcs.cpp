/*
	*** Export Image Dialog
	*** src/gui/exportimage_funcs.cpp
	Copyright T. Youngs 2013-2017

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

#include "gui/exportimage.h"
#include "gui/mainwindow.h"
#include <main/aten.h>
#include <QtWidgets/QFileDialog>
#include <QMessageBox>

ATEN_USING_NAMESPACE

// Constructor
AtenExportImage::AtenExportImage(AtenWindow& parent) : atenWindow_(parent), QDialog(&parent)
{
	ui.setupUi(this);

	firstShow_ = true;

	// Populate format combo
	for (int n=0; n<AtenWindow::nBitmapFormats; ++n) ui.ImageFormatCombo->addItem( AtenWindow::bitmapFormatFilter((AtenWindow::BitmapFormat) n) );
	ui.ImageFormatCombo->setCurrentIndex(AtenWindow::BitmapPNG);
}

// Destructor
AtenExportImage::~AtenExportImage()
{
}

// Call dialog to get image save information
bool AtenExportImage::getImageDetails()
{
	// If this is the first show, set the defaults in the controls
	if (firstShow_)
	{
		ui.FileNameEdit->setText(atenWindow_.aten().workDir().absoluteFilePath("image.png"));
		ui.ImageWidthSpin->setValue(atenWindow_.contextWidth());
		ui.ImageHeightSpin->setValue(atenWindow_.contextHeight());
		ui.ImageHeightSpin->setValue(ui.MaintainAspectRatioCheck->checkState() == Qt::Checked ? ui.ImageWidthSpin->value() / aspectRatio_ : ui.ImageHeightSpin->value());
		ui.ImageFormatCombo->setCurrentIndex(AtenWindow::BitmapPNG);
	}

	aspectRatio_ = double(ui.ImageWidthSpin->value()) / double(ui.ImageHeightSpin->value());

	firstShow_ = false;

	int result = exec();
	return (result == 1);
}

// Return selected image width
double AtenExportImage::imageWidth()
{
	return (firstShow_ ? atenWindow_.contextWidth() : ui.ImageWidthSpin->value());
}

// Return selected image height
double AtenExportImage::imageHeight()
{
	return (firstShow_ ? atenWindow_.contextHeight() : ui.ImageHeightSpin->value());
}

// Return selected image format
const char* AtenExportImage::imageFormat()
{
	return AtenWindow::bitmapFormatExtension((AtenWindow::BitmapFormat) ui.ImageFormatCombo->currentIndex());
}

// Return whether image should be transparent
bool AtenExportImage::imageTransparent()
{
	return ui.TransparentCheck->isChecked();
}

// Return selected filename
QString AtenExportImage::fileName()
{
	return ui.FileNameEdit->text();
}

/*
 * Slots
 */

void AtenExportImage::on_SelectFileNameButton_clicked(bool checked)
{
	QString newFile = QFileDialog::getSaveFileName(this, "Choose image save file name", ui.FileNameEdit->text(), QString(AtenWindow::bitmapFormatFilter((AtenWindow::BitmapFormat) ui.ImageFormatCombo->currentIndex())) + ";;All files (*.*)");
	if (!newFile.isEmpty()) ui.FileNameEdit->setText(newFile);
}

void AtenExportImage::on_ResetToCurrentButton_clicked(bool checked)
{
	ui.ImageHeightSpin->setValue(atenWindow_.contextHeight());
	ui.ImageWidthSpin->setValue(atenWindow_.contextWidth());
}

void AtenExportImage::on_ImageWidthSpin_valueChanged(int value)
{
	if (ui.MaintainAspectRatioCheck->isChecked()) ui.ImageHeightSpin->setValue(value / aspectRatio_);
}

void AtenExportImage::on_MaintainAspectRatioCheck_toggled(bool checked)
{
	ui.ImageHeightSpin->setDisabled(checked);
	if (checked) ui.ImageHeightSpin->setValue(ui.ImageWidthSpin->value() / aspectRatio_);
}

void AtenExportImage::on_SaveImageButton_clicked(bool checked)
{
	// Check if specified filename already exists
	QFile file(ui.FileNameEdit->text());
	if (file.exists())
	{
		if (QMessageBox::warning(this, "File Exists", "The specified file already exists. Overwrite it?", QMessageBox::Ok | QMessageBox::Cancel, QMessageBox::Cancel) == QMessageBox::Cancel) return;
	}

	accept();
}

void AtenExportImage::on_CancelButton_clicked(bool checked)
{
	reject();
}
