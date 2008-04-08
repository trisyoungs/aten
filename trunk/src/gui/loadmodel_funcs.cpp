/*
	*** Qt loadmodel functions interface
	*** src/gui/loadmodel_funcs.cpp
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

#include "gui/loadmodel.h"
#include "gui/gui.h"
#include "base/master.h"

// Constructor
AtenLoadModel::AtenLoadModel(QDialog *parent) : QDialog(parent)
{
	ui.setupUi(this);
	currentDirectory_ = master.workDir.get();
	selectedFilter_ = NULL;
}

// Set controls
void AtenLoadModel::setControls()
{
	ui.LoadModelRebondCombo->setCurrentIndex( prefs.bondOnLoad() );
	ui.LoadModelCentreCombo->setCurrentIndex( prefs.centreOnLoad() );
	ui.LoadModelFoldCombo->setCurrentIndex( prefs.foldOnLoad() );
	ui.LoadModelPackCombo->setCurrentIndex( prefs.packOnLoad() );
}

// Finalise GUI
void AtenLoadModel::finaliseUi()
{
}

// Call a file dialog
void AtenLoadModel::on_LoadModelBrowseButton_clicked(bool checked)
{
	// Create list of filters
	static char s[512], *c;
	QString filters, selFilter;
	Filter *f;
	filters += "All files (*)";
	for (f = master.filters(FT_MODEL_IMPORT); f != NULL; f = f->next)
	{
		filters += ";;";
		filters += f->description();
	}
	selectedFilename_ = qPrintable(QFileDialog::getOpenFileName(this, "Select Model File", currentDirectory_.get(), filters, &selFilter));
	strcpy(s,selectedFilename_.get());
	c = strrchr(s, '/');
	if (c == NULL) s[0] = '\0';
	else *c = '\0';
	currentDirectory_ = s;
	ui.LoadModelEdit->setText(selectedFilename_.get());
	// Find the corresponding Aten filter that was selected
	for (f = master.filters(FT_MODEL_IMPORT); f != NULL; f = f->next)
		if (strcmp(f->description(),qPrintable(selFilter)) == 0) break;
	printf("SLECAEKJHSSDKJ %li\n",selectedFilter_);
	selectedFilter_ = f;
}

// Return the selected filter
Filter *AtenLoadModel::selectedFilter()
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
