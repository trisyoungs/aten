/*
	*** Qt loadmodel functions interface
	*** src/gui/loadmodel_funcs.cpp
	Copyright T. Youngs 2007-2010

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

#include "gui/loadmodel.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "main/aten.h"

// Constructor
AtenLoadModel::AtenLoadModel(QWidget *parent) : QDialog(parent)
{
	refreshing_ = FALSE;
	ui.setupUi(this);
}

// Set controls
void AtenLoadModel::setControls()
{
	refreshing_ = TRUE;
	ui.RebondCombo->setCurrentIndex( prefs.bondOnLoad() );
	ui.CentreCombo->setCurrentIndex( prefs.centreOnLoad() );
	ui.FoldCombo->setCurrentIndex( prefs.foldOnLoad() );
	ui.PackCombo->setCurrentIndex( prefs.packOnLoad() );
	ui.ZMappingCombo->setCurrentIndex( prefs.zMapType() );
	ui.FormatCombo->clear();
	ui.FormatCombo->addItem("<Auto Detect>");
	Refitem<Tree,int> *ri;
	for (ri = aten.filters(FilterData::ModelImport); ri != NULL; ri = ri->next) 	ui.FormatCombo->addItem(ri->item->filter.description());
	refreshing_ = FALSE;
}

// Finalise GUI
void AtenLoadModel::finaliseUi()
{
}

// Edit box finished editing
void AtenLoadModel::on_FilenameEdit_editingFinished()
{
	selectedFilename_ = qPrintable(ui.FilenameEdit->text());
}

// Edit box finished editing
void AtenLoadModel::on_FilenameEdit_returnPressed()
{
	selectedFilename_ = qPrintable(ui.FilenameEdit->text());
	this->accept();
}

// Call a file dialog
void AtenLoadModel::on_BrowseButton_clicked(bool checked)
{
	static QDir currentDirectory_(aten.workDir());
	Dnchar s;
	QString selFilter;
	selectedFilename_ = qPrintable(QFileDialog::getOpenFileName(this, "Select Model File", currentDirectory_.path(), gui.mainWindow->loadModelFilters, &selFilter));
	s = qPrintable(QDir::fromNativeSeparators(selectedFilename_.get()));
	int pos = s.rFind('/');
	if (pos != -1) s.eraseFrom(pos);
	currentDirectory_ = qPrintable(QDir::toNativeSeparators(selectedFilename_.get()));
	ui.FilenameEdit->setText(selectedFilename_.get());
}

// Return the selected filter
Tree *AtenLoadModel::selectedFormat()
{
	// Return the filter selected in the combo (or NULL if <Auto Detect> was selected)
	int i = ui.FormatCombo->currentIndex();
	Refitem<Tree,int> *filter = (i == 0 ? NULL : aten.filter(FilterData::ModelImport, i-1));
	return (filter != NULL ? filter->item : NULL);
}

// Return filename
const char *AtenLoadModel::selectedFilename()
{
	return selectedFilename_.get();
}

void AtenLoadModel::on_RebondCombo_activated(int index)
{
	if (refreshing_) return;
	prefs.setBondOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_FoldCombo_activated(int index)
{
	if (refreshing_) return;
	prefs.setFoldOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_PackCombo_activated(int index)
{
	if (refreshing_) return;
	prefs.setPackOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_CentreCombo_activated(int index)
{
	if (refreshing_) return;
	prefs.setCentreOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_ZMappingCombo_activated(int index)
{
	if (refreshing_) return;
	prefs.setZMapType( (ElementMap::ZMapType) index );
}

void AtenLoadModel::on_BohrCheck_clicked(bool checked)
{
	if (refreshing_) return;
	prefs.setCoordsInBohr(checked);
}

void AtenLoadModel::on_KeepNamesCheck_clicked(bool checked)
{
	if (refreshing_) return;
	prefs.setKeepNames(checked);
}
