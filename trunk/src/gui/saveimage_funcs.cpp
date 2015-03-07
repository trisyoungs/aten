/*
	*** Save Image Dialog
	*** src/gui/saveimage_funcs.cpp
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

#include "gui/saveimage.h"
#include "main/aten.h"
#include <QtGui/QFileDialog>

ATEN_USING_NAMESPACE

// Static Members
QDir SaveImageDialog::currentDirectory_;

// Constructor
SaveImageDialog::SaveImageDialog(QWidget* parent) : QDialog(parent)
{
	ui.setupUi(this);
	
	// Populate format combo
	for (int n=0; n<Aten::nBitmapFormats; ++n) ui.ImageFormatCombo->addItem( QString(Aten::bitmapFormatExtension((Aten::BitmapFormat) n)).toUpper());
}

// Destructor
SaveImageDialog::~SaveImageDialog()
{
}

// Call dialog to get image save information
bool SaveImageDialog::getImageDetails(int startWidth, int startHeight)
{
	ui.FileNameEdit->setText(currentDirectory_.absolutePath());
	aspectRatio_ = double(startWidth) / double(startHeight);
// 	ui.MaintainAspectRatioCheck->setChecked(Aten::bitmapExportMaintainAspect());
	ui.ImageWidthSpin->setValue(startWidth);
	ui.ImageHeightSpin->setValue(ui.MaintainAspectRatioCheck->checkState() == Qt::Checked ? startWidth / aspectRatio_ : startHeight);
// 	ui.ImageFormatCombo->setCurrentIndex(Aten::bitmapExportFormat());

	int result = exec();
	return (result == 1);
}

/*
 * Slots
 */

void SaveImageDialog::on_SelectFileNameButton_clicked(bool checked)
{
	QString newFile = QFileDialog::getSaveFileName(this, "Choose image save file name", currentDirectory_.absolutePath(), QString(Aten::bitmapFormatFilter((Aten::BitmapFormat) ui.ImageFormatCombo->currentIndex())) + ";;All files (*.*)");
	if (!newFile.isEmpty()) ui.FileNameEdit->setText(newFile);
}

void SaveImageDialog::on_ImageWidthSpin_valueChanged(int value)
{
	if (ui.MaintainAspectRatioCheck->isChecked()) ui.ImageHeightSpin->setValue(value / aspectRatio_);
}

void SaveImageDialog::on_MaintainAspectRatioCheck_toggled(bool checked)
{
	ui.ImageHeightSpin->setDisabled(checked);
	if (checked) ui.ImageHeightSpin->setValue(ui.ImageWidthSpin->value() / aspectRatio_);
}

void SaveImageDialog::on_SaveImageButton_clicked(bool checked)
{
	accept();
}

void SaveImageDialog::on_CancelButton_clicked(bool checked)
{
	reject();
}
