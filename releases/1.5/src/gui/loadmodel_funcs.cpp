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
	ui.setupUi(this);
	selectedFilter_ = NULL;
}

// Set controls
void AtenLoadModel::setControls()
{
	ui.LoadModelRebondCombo->setCurrentIndex( prefs.bondOnLoad() );
	ui.LoadModelCentreCombo->setCurrentIndex( prefs.centreOnLoad() );
	ui.LoadModelFoldCombo->setCurrentIndex( prefs.foldOnLoad() );
	ui.LoadModelPackCombo->setCurrentIndex( prefs.packOnLoad() );
	ui.LoadModelZMappingCombo->setCurrentIndex( prefs.zMapType() );
}

// Finalise GUI
void AtenLoadModel::finaliseUi()
{
}

// Edit box finished editing
void AtenLoadModel::on_LoadModelEdit_editingFinished()
{
	selectedFilename_ = qPrintable(ui.LoadModelEdit->text());
}

// Edit box finished editing
void AtenLoadModel::on_LoadModelEdit_returnPressed()
{
	selectedFilename_ = qPrintable(ui.LoadModelEdit->text());
	selectedFilter_ = NULL;
	this->accept();
}

// Call a file dialog
void AtenLoadModel::on_LoadModelBrowseButton_clicked(bool checked)
{
	static QDir currentDirectory_(aten.workDir());
	static char s[512], *c;
	QString selFilter;
	selectedFilename_ = qPrintable(QFileDialog::getOpenFileName(this, "Select Model File", currentDirectory_.path(), gui.mainWindow->loadModelFilters, &selFilter));
	strcpy(s, selectedFilename_.get());
	c = strrchr(s, '/');
	if (c == NULL) s[0] = '\0';
	else *c = '\0';
	currentDirectory_ = s;
	ui.LoadModelEdit->setText(selectedFilename_.get());
	// Find the corresponding Aten filter that was selected
	selectedFilter_ = aten.findFilterByDescription(FilterData::ModelImport, qPrintable(selFilter));
}

// Return the selected filter
Tree *AtenLoadModel::selectedFilter()
{
	return selectedFilter_;
}

// Return filename
const char *AtenLoadModel::selectedFilename()
{
	return selectedFilename_.get();
}

void AtenLoadModel::on_LoadModelRebondCombo_activated(int index)
{
	prefs.setBondOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_LoadModelFoldCombo_activated(int index)
{
	prefs.setFoldOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_LoadModelPackCombo_activated(int index)
{
	prefs.setPackOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_LoadModelCentreCombo_activated(int index)
{
	prefs.setCentreOnLoad( (Prefs::FilterSwitch) index );
}

void AtenLoadModel::on_LoadModelZMappingCombo_activated(int index)
{
	prefs.setZMapType( (ElementMap::ZMapType) index );
}
