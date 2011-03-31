/*
	*** Glyphs Dock Widget
	*** src/gui/glyphs_funcs.cpp
	Copyright T. Youngs 2007-2011

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

#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/glyphs.h"
#include "gui/toolbox.h"
#include "gui/tlistwidgetitem.h"
#include "model/model.h"
#include "main/aten.h"
#include "base/sysfunc.h"

// Constructor
GlyphsWidget::GlyphsWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);

	// Set allowable glyph types in combo box
	refreshing_ = TRUE;
	for (int n=0; n<Glyph::nGlyphTypes; ++n) ui.GlyphTypeCombo->addItem( Glyph::glyphTypeName( (Glyph::GlyphType) n) );
	refreshing_ = FALSE;
	// Grab pointers to controls for ease of use later on
	dataAtomRadio[0] = ui.Data1AtomRadio;
	dataAtomRadio[1] = ui.Data2AtomRadio;
	dataAtomRadio[2] = ui.Data3AtomRadio;
	dataAtomRadio[3] = ui.Data4AtomRadio;
	dataValueRadio[0] = ui.Data1ValueRadio;
	dataValueRadio[1] = ui.Data2ValueRadio;
	dataValueRadio[2] = ui.Data3ValueRadio;
	dataValueRadio[3] = ui.Data4ValueRadio;
	dataAtomIdSpin[0] = ui.Data1AtomIdSpin;
	dataAtomIdSpin[1] = ui.Data2AtomIdSpin;
	dataAtomIdSpin[2] = ui.Data3AtomIdSpin;
	dataAtomIdSpin[3] = ui.Data4AtomIdSpin;
	dataValueXSpin[0] = ui.Data1ValueXSpin;
	dataValueXSpin[1] = ui.Data2ValueXSpin;
	dataValueXSpin[2] = ui.Data3ValueXSpin;
	dataValueXSpin[3] = ui.Data4ValueXSpin;
	dataValueYSpin[0] = ui.Data1ValueYSpin;
	dataValueYSpin[1] = ui.Data2ValueYSpin;
	dataValueYSpin[2] = ui.Data3ValueYSpin;
	dataValueYSpin[3] = ui.Data4ValueYSpin;
	dataValueZSpin[0] = ui.Data1ValueZSpin;
	dataValueZSpin[1] = ui.Data2ValueZSpin;
	dataValueZSpin[2] = ui.Data3ValueZSpin;
	dataValueZSpin[3] = ui.Data4ValueZSpin;
	dataAtomDataCombo[0] = ui.Data1AtomCombo;
	dataAtomDataCombo[1] = ui.Data2AtomCombo;
	dataAtomDataCombo[2] = ui.Data3AtomCombo;
	dataAtomDataCombo[3] = ui.Data4AtomCombo;
	dataColourFrame[0] = ui.Data1ColourFrame;
	dataColourFrame[1] = ui.Data2ColourFrame;
	dataColourFrame[2] = ui.Data3ColourFrame;
	dataColourFrame[3] = ui.Data4ColourFrame;
	dataGroupBox[0] = ui.Data1Group;
	dataGroupBox[1] = ui.Data2Group;
	dataGroupBox[2] = ui.Data3Group;
	dataGroupBox[3] = ui.Data4Group;
	dataAtomWidget[0] = ui.Data1AtomGroup;
	dataAtomWidget[1] = ui.Data2AtomGroup;
	dataAtomWidget[2] = ui.Data3AtomGroup;
	dataAtomWidget[3] = ui.Data4AtomGroup;
	dataValueWidget[0] = ui.Data1ValueGroup;
	dataValueWidget[1] = ui.Data2ValueGroup;
	dataValueWidget[2] = ui.Data3ValueGroup;
	dataValueWidget[3] = ui.Data4ValueGroup;
}

// Show window
void GlyphsWidget::showWidget()
{
	show();
	if (shouldRefresh_) refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.GlyphsButton->setChecked(TRUE);
}

// Add item to list
void GlyphsWidget::addItemToList(Glyph *g)
{
	QString s;
	s = itoa(ui.GlyphList->count()+1);
	s += ". ";
	s += Glyph::glyphType(g->type());
	TListWidgetItem *item = new TListWidgetItem(ui.GlyphList);
	item->setText(s);
	item->data.set(VTypes::GlyphData, g);
	if (g->isSelected()) item->setSelected(TRUE);
}

// Update glyph list
void GlyphsWidget::refresh()
{
	// If the page isn't visible, don't do anything
	if (!gui.glyphsWidget>isVisible())
	{
		shouldRefresh_ = TRUE;
		return;
	}
	refreshing_ = TRUE;
	// Clear any existing items
	ui.GlyphList->clear();
	Model *m = aten.currentModelOrFrame();
	for (Glyph *g = m->glyphs(); g != NULL; g = g->next) addItemToList(g);
	// Set initial selection
	ui.GlyphList->setCurrentRow(0);
	// Update control data
	updateData(m->glyphs());
	// Update controls
	updateControls(m->glyphs());
	// Set flags
	shouldRefresh_ = FALSE;
	refreshing_ = FALSE;
}

// Update current glyph data
void GlyphsWidget::updateData(Glyph *g)
{
	if (g == NULL) return;
	// Set data in widgets
	ui.GlyphTypeCombo->setCurrentIndex(g->type());
	ui.GlyphLineEdit->setText(g->text());
	ui.GlyphVisibleCheck->setChecked(g->isVisible());
	// Set individual data groups
	for (int n=0; n<g->nData(); ++n)
	{
		GlyphData *gd = g->data(n);
		if (gd->atomSetLast())
		{
			dataAtomIdSpin[n]->setValue(gd->atom() == NULL ? 0 : gd->atom()->id()+1);
			dataAtomDataCombo[n]->setCurrentIndex(gd->atomData());
		}
		else
		{
			dataValueXSpin[n]->setValue(gd->vector().x);
			dataValueYSpin[n]->setValue(gd->vector().y);
			dataValueZSpin[n]->setValue(gd->vector().z);
		}
		dataColourFrame[n]->setColour(gd->colour());
		dataColourFrame[n]->update();
	}
}

// Update controls (i.e. enable/disable widgets in data groups)
void GlyphsWidget::updateControls(Glyph *g)
{
	if (g == NULL)
	{
		ui.PropertiesBox->setEnabled(FALSE);
		for (int n=0; n<MAXGLYPHDATA; ++n) dataGroupBox[n]->setEnabled(FALSE);
	}
	else
	{
		ui.PropertiesBox->setEnabled(TRUE);
		// Set individual data groups
		for (int n=0; n<g->nData(); ++n)
		{
			GlyphData *gd = g->data(n);
			dataGroupBox[n]->setEnabled(TRUE);
			if (gd->atomSetLast())
			{
				dataAtomWidget[n]->setEnabled(TRUE);
				dataValueWidget[n]->setEnabled(FALSE);
				dataAtomRadio[n]->setChecked(TRUE);
				dataValueRadio[n]->setChecked(FALSE);
			}
			else
			{
				dataAtomWidget[n]->setEnabled(FALSE);
				dataValueWidget[n]->setEnabled(TRUE);
				dataValueRadio[n]->setChecked(TRUE);
				dataAtomRadio[n]->setChecked(FALSE);
			}
// 			dataColourFrame[n]->setColour(gd->colour());
// 			dataColourFrame[n]->update();
		}
		for (int n=g->nData(); n<MAXGLYPHDATA; ++n) dataGroupBox[n]->setEnabled(FALSE);
	}
}

void GlyphsWidget::on_GlyphList_currentRowChanged(int row)
{
	if (refreshing_) return;
	Glyph *g = (row == -1 ? NULL : aten.currentModelOrFrame()->glyph(row));
	refreshing_ = TRUE;
	updateData(g);
	updateControls(g);
	refreshing_ = FALSE;
	for (int i = 0; i<ui.GlyphList->count(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) ui.GlyphList->item(i))->data.asPointer(VTypes::GlyphData);
		g->setSelected(ui.GlyphList->item(i)->isSelected());
	}
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphList_itemSelectionChanged()
{
	// Extra check to deactivate controls when no glyph in the list is selected
	if (refreshing_) return;
	if (ui.GlyphList->currentRow() == -1)
	{
		refreshing_ = TRUE;
		updateData(NULL);
		updateControls(NULL);
		refreshing_ = FALSE;
	}
	Glyph *g;
	for (int i = 0; i<ui.GlyphList->count(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) ui.GlyphList->item(i))->data.asPointer(VTypes::GlyphData);
		g->setSelected(ui.GlyphList->item(i)->isSelected());
	}
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphAddButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	if (m == NULL) return;
	for (Glyph *g = m->glyphs(); g != NULL; g = g->next) g->setSelected(FALSE);
	Glyph *g = m->addGlyph(Glyph::ArrowGlyph);
	addItemToList(g);
	// Deselect any previously-selected items - easiest way is to temporarily change selection mode
	ui.GlyphList->setSelectionMode(QAbstractItemView::SingleSelection);
	ui.GlyphList->setCurrentRow(ui.GlyphList->count()-1);
	ui.GlyphList->setSelectionMode(QAbstractItemView::ExtendedSelection);
	g->setSelected(TRUE);
	refreshing_ = TRUE;
	updateData(g);
	updateControls(g);
	refreshing_ = FALSE;
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphDeleteSelectedButton_clicked(bool checked)
{
	// Loop over list of selected items and set new colour
	int row = ui.GlyphList->currentRow();
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Model *m = aten.currentModelOrFrame();
	Glyph *g;
	for (int i = items.size()-1; i>=0; --i)
	{
		g = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		m->removeGlyph(g);
	}
	// Refresh list
	refresh();
	// Reselect item now at the previous selection position
	ui.GlyphList->setCurrentRow(row < ui.GlyphList->count() ? row : ui.GlyphList->count()-1);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphSelectAllButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	Glyph *g;
	refreshing_ = TRUE;
	for (int i = 0; i<ui.GlyphList->count(); ++i)
	{
		ui.GlyphList->item(i)->setSelected(TRUE);
		g = (Glyph*) ((TListWidgetItem*) ui.GlyphList->item(i))->data.asPointer(VTypes::GlyphData);
		g->setSelected(TRUE);
	}
	refreshing_ = FALSE;
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphSelectNoneButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	Glyph *g = m->glyphs();
	refreshing_ = TRUE;
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	for (int i = items.size()-1; i>=0; --i)
	{
		items.at(i)->setSelected(FALSE);
		g->setSelected(FALSE);
		g = g->next;
	}
	refreshing_ = FALSE;
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphInvertSelectionButton_clicked(bool checked)
{
	Glyph *g;
	refreshing_ = TRUE;
	for (int i = 0; i<ui.GlyphList->count(); ++i)
	{
		ui.GlyphList->item(i)->setSelected( ui.GlyphList->item(i)->isSelected() ? FALSE : TRUE );
		g = (Glyph*) ((TListWidgetItem*) ui.GlyphList->item(i))->data.asPointer(VTypes::GlyphData);
		g->setSelected( ui.GlyphList->item(i)->isSelected());
	}
	refreshing_ = FALSE;
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphHideAllButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	for (Glyph *g = m->glyphs(); g != NULL; g = g->next) g->setVisible(FALSE);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphHideNoneButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	for (Glyph *g = m->glyphs(); g != NULL; g = g->next) g->setVisible(TRUE);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphHideSelectedButton_clicked(bool checked)
{
	Glyph *g;
	for (int i = 0; i<ui.GlyphList->count(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) ui.GlyphList->item(i))->data.asPointer(VTypes::GlyphData);
		g->setVisible(FALSE);
	}
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphTypeCombo_currentIndexChanged(int row)
{
	if (refreshing_) return;
	// Loop over list of selected items and set new atom id
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *g;
	QString s;
	TListWidgetItem *item;
	Glyph::GlyphType gt = (Glyph::GlyphType) row;
	for (int i = 0; i < items.size(); ++i)
	{
		item = (TListWidgetItem*) items.at(i);
		g = (Glyph*) item->data.asPointer(VTypes::GlyphData);
		g->setType(gt);
		s = itoa(ui.GlyphList->row(item)+1);
		s += ". ";
		s += Glyph::glyphType(gt);
		item->setText(s);
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphLineEdit_returnPressed()
{
	if (refreshing_) return;
	// Loop over list of selected items and set new atom id
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *g;
	for (int i = 0; i < items.size(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		g->setText(qPrintable(ui.GlyphLineEdit->text()));
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_GlyphVisibleCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Loop over list of selected items and set new atom id
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *g;
	for (int i = 0; i < items.size(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		g->setVisible(checked);
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::on_Data1AtomIdSpin_valueChanged(int i)
{
	dataAtomIdChanged(0, i-1);
}

void GlyphsWidget::on_Data2AtomIdSpin_valueChanged(int i)
{
	dataAtomIdChanged(1, i-1);
}

void GlyphsWidget::on_Data3AtomIdSpin_valueChanged(int i)
{
	dataAtomIdChanged(2, i-1);
}

void GlyphsWidget::on_Data4AtomIdSpin_valueChanged(int i)
{
	dataAtomIdChanged(3, i-1);
}

void GlyphsWidget::on_Data1AtomRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[0]->setEnabled(TRUE);
	dataValueWidget[0]->setEnabled(FALSE);
	dataValueRadio[0]->setChecked(FALSE);
	dataAtomIdChanged(0, dataAtomIdSpin[0]->value()-1);
}

void GlyphsWidget::on_Data2AtomRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[1]->setEnabled(TRUE);
	dataValueWidget[1]->setEnabled(FALSE);
	dataValueRadio[1]->setChecked(FALSE);
	dataAtomIdChanged(1, dataAtomIdSpin[1]->value()-1);
}

void GlyphsWidget::on_Data3AtomRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[2]->setEnabled(TRUE);
	dataValueWidget[2]->setEnabled(FALSE);
	dataValueRadio[2]->setChecked(FALSE);
	dataAtomIdChanged(2, dataAtomIdSpin[2]->value()-1);
}

void GlyphsWidget::on_Data4AtomRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[3]->setEnabled(TRUE);
	dataValueWidget[3]->setEnabled(FALSE);
	dataValueRadio[3]->setChecked(FALSE);
	dataAtomIdChanged(3, dataAtomIdSpin[3]->value()-1);
}

void GlyphsWidget::on_Data1ValueRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[0]->setEnabled(FALSE);
	dataValueWidget[0]->setEnabled(TRUE);
	dataValueRadio[0]->setChecked(FALSE);
	dataValueChanged(0, dataValueXSpin[0]->value(), dataValueYSpin[0]->value(), dataValueZSpin[0]->value());
}

void GlyphsWidget::on_Data2ValueRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[1]->setEnabled(FALSE);
	dataValueWidget[1]->setEnabled(TRUE);
	dataValueRadio[1]->setChecked(FALSE);
	dataValueChanged(1, dataValueXSpin[1]->value(), dataValueYSpin[1]->value(), dataValueZSpin[1]->value());
}

void GlyphsWidget::on_Data3ValueRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[2]->setEnabled(FALSE);
	dataValueWidget[2]->setEnabled(TRUE);
	dataValueRadio[2]->setChecked(FALSE);
	dataValueChanged(2, dataValueXSpin[2]->value(), dataValueYSpin[2]->value(), dataValueZSpin[2]->value());
}

void GlyphsWidget::on_Data4ValueRadio_clicked(bool checked)
{
	if (refreshing_) return;
	dataAtomWidget[3]->setEnabled(FALSE);
	dataValueWidget[3]->setEnabled(TRUE);
	dataValueRadio[3]->setChecked(FALSE);
	dataValueChanged(3, dataValueXSpin[3]->value(), dataValueYSpin[3]->value(), dataValueZSpin[3]->value());
}

void GlyphsWidget::on_Data1ValueXSpin_valueChanged(double d)
{
	dataValueChanged(0, 0, d);
}

void GlyphsWidget::on_Data2ValueXSpin_valueChanged(double d)
{
	dataValueChanged(1, 0, d);
}

void GlyphsWidget::on_Data3ValueXSpin_valueChanged(double d)
{
	dataValueChanged(2, 0, d);
}

void GlyphsWidget::on_Data4ValueXSpin_valueChanged(double d)
{
	dataValueChanged(3, 0, d);
}

void GlyphsWidget::on_Data1ValueYSpin_valueChanged(double d)
{
	dataValueChanged(0, 1, d);
}

void GlyphsWidget::on_Data2ValueYSpin_valueChanged(double d)
{
	dataValueChanged(1, 1, d);
}

void GlyphsWidget::on_Data3ValueYSpin_valueChanged(double d)
{
	dataValueChanged(2, 1, d);
}

void GlyphsWidget::on_Data4ValueYSpin_valueChanged(double d)
{
	dataValueChanged(3, 1, d);
}

void GlyphsWidget::on_Data1ValueZSpin_valueChanged(double d)
{
	dataValueChanged(0, 2, d);
}

void GlyphsWidget::on_Data2ValueZSpin_valueChanged(double d)
{
	dataValueChanged(1, 2, d);
}

void GlyphsWidget::on_Data3ValueZSpin_valueChanged(double d)
{
	dataValueChanged(2, 2, d);
}

void GlyphsWidget::on_Data4ValueZSpin_valueChanged(double d)
{
	dataValueChanged(3, 2, d);
}

void GlyphsWidget::on_Data1ColourButton_clicked(bool checked)
{
	dataColourChanged(0);
}

void GlyphsWidget::on_Data2ColourButton_clicked(bool checked)
{
	dataColourChanged(1);
}

void GlyphsWidget::on_Data3ColourButton_clicked(bool checked)
{
	dataColourChanged(2);
}

void GlyphsWidget::on_Data4ColourButton_clicked(bool checked)
{
	dataColourChanged(3);
}

void GlyphsWidget::dataAtomIdChanged(int id, int value)
{
	if (refreshing_) return;
	// Loop over list of selected items and set new atom id
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *g;
	for (int i = 0; i < items.size(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		if (id >= g->nData()) msg.print("Can't set atom id in data %i for a '%s' glyph (out of range)...\n", id+1, Glyph::glyphType(g->type()));
		else g->data(id)->setAtom( g->parent()->atom(value) );
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::dataValueChanged(int id, int component, double value)
{
	if (refreshing_) return;
	// Loop over list of selected items and set new atom id
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *g;
	for (int i = 0; i < items.size(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		if (id >= g->nData()) msg.print("Can't set literal value data %i for a '%s' glyph (out of range)...\n", id+1, Glyph::glyphType(g->type()));
		else g->data(id)->setVector(component, value);
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::dataValueChanged(int id, double x, double y, double z)
{
	if (refreshing_) return;
	// Loop over list of selected items and set new atom id
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *g;
	for (int i = 0; i < items.size(); ++i)
	{
		g = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		if (id >= g->nData()) msg.print("Can't set literal value data %i for a '%s' glyph (out of range)...\n", id+1, Glyph::glyphType(g->type()));
		else g->data(id)->setVector(x, y, z);
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::dataColourChanged(int id)
{
	if (refreshing_) return;
	// Get current colour from frame and convert into a QColor
	QColor oldcol = dataColourFrame[id]->colour(), newcol;
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour in frame
	dataColourFrame[id]->setColour(newcol);
	dataColourFrame[id]->update();

	// Loop over list of selected items and set new colour
	QList<QListWidgetItem*> items = ui.GlyphList->selectedItems();
	Glyph *gl;
	for (int i = 0; i < items.size(); ++i)
	{
		gl = (Glyph*) ((TListWidgetItem*) items.at(i))->data.asPointer(VTypes::GlyphData);
		if (id >= gl->nData()) msg.print("Can't set colour for data %i for a '%s' glyph (out of range)...\n", id+1, Glyph::glyphType(gl->type()));
		else gl->data(id)->setColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	}
	aten.currentModelOrFrame()->changeLog.add(Log::Glyphs);
	gui.mainWidget->postRedisplay();
}

void GlyphsWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.GlyphsButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget->postRedisplay();
	event->accept();
}
