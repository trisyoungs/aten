/*
	*** Qt loadmodel functions interface
	*** src/gui/loadmodel_funcs.cpp
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

#include "gui/loadmodel.h"
#include "gui/mainwindow.h"
#include "base/sysfunc.h"
#include "main/aten.h"

// Constructor
AtenLoadModel::AtenLoadModel(AtenWindow& parent) : QDialog(&parent), parent_(parent)
{
	refreshing_ = FALSE;
	ui.setupUi(this);
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
	static QDir currentDirectory_(parent_.aten().workDir());
	QString selFilter;
	selectedFilename_ = qPrintable(QFileDialog::getOpenFileName(this, "Select Model File", currentDirectory_.path(), parent_.aten().fileDialogFilters(FilterData::ModelImport), &selFilter));
	// Store path for next use
	currentDirectory_.setPath(selectedFilename_.get());
	ui.FilenameEdit->setText(selectedFilename_.get());
}

void AtenLoadModel::on_RebondCombo_activated(int index)
{
	if (refreshing_) return;
	if (index == 0) prefs.setBondOnLoad(Choice::Default);
	else if (index == 1) prefs.setBondOnLoad(Choice::No);
	else if (index == 2) prefs.setBondOnLoad(Choice::Yes);
}

void AtenLoadModel::on_FoldCombo_activated(int index)
{
	if (refreshing_) return;
	if (index == 0) prefs.setFoldOnLoad(Choice::Default);
	else if (index == 1) prefs.setFoldOnLoad(Choice::No);
	else if (index == 2) prefs.setFoldOnLoad(Choice::Yes);
}

void AtenLoadModel::on_PackCombo_activated(int index)
{
	if (refreshing_) return;
	if (index == 0) prefs.setPackOnLoad(Choice::Default);
	else if (index == 1) prefs.setPackOnLoad(Choice::No);
	else if (index == 2) prefs.setPackOnLoad(Choice::Yes);
}

void AtenLoadModel::on_CentreCombo_activated(int index)
{
	if (refreshing_) return;
	if (index == 0) prefs.setCentreOnLoad(Choice::Default);
	else if (index == 1) prefs.setCentreOnLoad(Choice::No);
	else if (index == 2) prefs.setCentreOnLoad(Choice::Yes);
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

void AtenLoadModel::on_KeepTypesCheck_clicked(bool checked)
{
	if (refreshing_) return;
	prefs.setKeepTypes(checked);
}

// Update controls and show dialog
void AtenLoadModel::updateAndShow()
{
	refreshing_ = TRUE;

	if (prefs.bondOnLoad() == Choice::Default) ui.RebondCombo->setCurrentIndex(0);
	else if (prefs.bondOnLoad() == Choice::No) ui.RebondCombo->setCurrentIndex(1);
	else ui.RebondCombo->setCurrentIndex(2);

	if (prefs.centreOnLoad() == Choice::Default) ui.CentreCombo->setCurrentIndex(0);
	else if (prefs.centreOnLoad() == Choice::No) ui.CentreCombo->setCurrentIndex(1);
	else ui.CentreCombo->setCurrentIndex(2);
	
	if (prefs.foldOnLoad() == Choice::Default) ui.FoldCombo->setCurrentIndex(0);
	else if (prefs.foldOnLoad() == Choice::No) ui.FoldCombo->setCurrentIndex(1);
	else ui.FoldCombo->setCurrentIndex(2);

	if (prefs.packOnLoad() == Choice::Default) ui.PackCombo->setCurrentIndex(0);
	else if (prefs.packOnLoad() == Choice::No) ui.PackCombo->setCurrentIndex(1);
	else ui.PackCombo->setCurrentIndex(2);

	if (prefs.bondOnLoad() == Choice::Default) ui.RebondCombo->setCurrentIndex(0);
	else if (prefs.bondOnLoad() == Choice::No) ui.RebondCombo->setCurrentIndex(1);
	else ui.RebondCombo->setCurrentIndex(2);

	ui.ZMappingCombo->setCurrentIndex( prefs.zMapType() );
	ui.FormatCombo->clear();
	ui.FormatCombo->addItem("<Auto Detect>");
	Refitem<Tree,int> *ri;
	for (ri = parent_.aten().filters(FilterData::ModelImport); ri != NULL; ri = ri->next) ui.FormatCombo->addItem(ri->item->filter.description());

	ui.KeepNamesCheck->setChecked(prefs.keepNames());
	ui.KeepTypesCheck->setChecked(prefs.keepTypes());

	refreshing_ = FALSE;
}

/*
 * Data
 */

// Return the selected filter
Tree* AtenLoadModel::selectedFormat()
{
	// Return the filter selected in the combo (or NULL if <Auto Detect> was selected)
	int i = ui.FormatCombo->currentIndex();
	Refitem<Tree,int> *filter = (i == 0 ? NULL : parent_.aten().filter(FilterData::ModelImport, i-1));
	return (filter != NULL ? filter->item : NULL);
}

// Return filename
const char *AtenLoadModel::selectedFilename()
{
	return selectedFilename_.get();
}
