/*
	*** Qt prefs window functions
	*** src/gui/prefs_funcs.cpp
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

#include "base/master.h"
#include "base/elements.h"
//#include "gui/gui.h"
#include "gui/prefs.h"
//#include <QtGui/QDialog>
#include <QtGui/QListWidgetItem>
#include <QtGui/QColorDialog>
#include "model/model.h"

// Constructor
AtenPrefs::AtenPrefs(QDialog *parent) : QDialog(parent)
{
	//for (int i=0; i<SP_NITEMS; i++) stackbuttons[i];
	ui.setupUi(this);
}

// Finalise GUI
void AtenPrefs::finaliseUi()
{
	dbgBegin(Debug::Calls,"AtenPrefs::finaliseUi");
	int i;
	// Add elements to element list and select first item
	QListWidgetItem *item;
	for (i=0; i<elements.nElements(); i++)
	{
		item = new QListWidgetItem(ui.ElementList);
		//item->setText(0, itoa(i));
		item->setText(elements.name(i));
	}
	ui.ElementList->setCurrentRow(0);
	dbgEnd(Debug::Calls,"AtenPrefs::finaliseUi");
}

// Set controls
void AtenPrefs::setControls()
{
	dbgBegin(Debug::Calls,"AtenPrefs::setControls");
	// Select the first element in the elements list
	ui.ElementList->setCurrentRow(0);

	// Set controls in view page
	ui.StickRadiusSpin->setValue(prefs.atomSize(Atom::StickStyle));
	ui.TubeRadiusSpin->setValue(prefs.atomSize(Atom::TubeStyle));
	ui.SphereRadiusSpin->setValue(prefs.atomSize(Atom::SphereStyle));
	ui.ScaledRadiusSpin->setValue(prefs.atomSize(Atom::ScaledStyle));
	ui.TubeBondSizeSpin->setValue(prefs.tubeSize());
	ui.SelectionScaleSpin->setValue(prefs.selectionScale());
	ui.AtomQualitySpin->setValue(prefs.atomDetail());
	ui.BondQualitySpin->setValue(prefs.bondDetail());
	ui.GlobeVisibleCheck->setChecked(prefs.shouldRender(Prefs::ViewGlobe));
	ui.CellVisibleCheck->setChecked(prefs.shouldRender(Prefs::ViewCell));
	ui.AxesVisibleCheck->setChecked(prefs.shouldRender(Prefs::ViewCellAxes));
	ui.AtomsVisibleCheck->setChecked(prefs.shouldRender(Prefs::ViewAtoms));
	ui.ShininessSpin->setValue(prefs.shininess());

	// Set controls in Lighting page
	ui.SpotlightAmbientColourFrame->setColour(prefs.spotlightColour(Prefs::AmbientComponent));
	ui.SpotlightDiffuseColourFrame->setColour(prefs.spotlightColour(Prefs::DiffuseComponent));
	ui.SpotlightSpecularColourFrame->setColour(prefs.spotlightColour(Prefs::SpecularComponent));
	GLfloat *pos = prefs.spotlightPosition();
	ui.SpotlightPositionXSpin->setValue(pos[0]);
	ui.SpotlightPositionYSpin->setValue(pos[1]);
	ui.SpotlightPositionZSpin->setValue(pos[2]);

	// Set controls in interaction page
	ui.LeftMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::LeftButton));
	ui.MiddleMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::MiddleButton));
	ui.RightMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::RightButton));
	ui.WheelMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::WheelButton));
	ui.ShiftButtonCombo->setCurrentIndex(prefs.keyAction(Prefs::ShiftKey));
	ui.CtrlButtonCombo->setCurrentIndex(prefs.keyAction(Prefs::CtrlKey));
	ui.AltButtonCombo->setCurrentIndex(prefs.keyAction(Prefs::AltKey));
	dbgBegin(Debug::Calls,"AtenPrefs::setControls");
}

/*
// Element Page
*/

void AtenPrefs::on_ElementList_currentRowChanged(int row)
{
	// Update the info for the current element
	ui.ElementNameLabel->setText(elements.name(row));
	ui.ElementSymbolLabel->setText(elements.symbol(row));
	ui.ElementMassLabel->setText(ftoa(elements.atomicMass(row)));
	ui.ElementAmbientColourFrame->setColour(elements.ambientColour(row));
	ui.ElementDiffuseColourFrame->setColour(elements.diffuseColour(row));
	ui.ElementRadiusSpin->setValue(elements.atomicRadius(row));
}

void AtenPrefs::on_ElementAmbientColourButton_clicked(bool checked)
{
	// Get current row
	int el = ui.ElementList->currentRow();
	if (el == -1) return;
	// Get element's current ambient colour and convert into a QColor
	GLfloat *col = elements.ambientColour(el);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	elements.setAmbientColour(el, newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.ElementAmbientColourFrame->setColour(newcol);
	ui.ElementAmbientColourFrame->update();
	// Re-set atom colours in model(s)
	master.currentModel()->logChange(LOG_VISUAL);
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_ElementDiffuseColourButton_clicked(bool checked)
{
	// Get current row
	int el = ui.ElementList->currentRow();
	if (el == -1) return;
	// Get element's current diffuse colour and convert into a QColor
	GLfloat *col = elements.diffuseColour(el);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	elements.setDiffuseColour(el, newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.ElementDiffuseColourFrame->setColour(newcol);
	ui.ElementDiffuseColourFrame->update();
	// Re-set atom colours in model(s)
	master.currentModel()->logChange(LOG_VISUAL);
	gui.mainView.postRedisplay();
}

/*
// View Page
*/

void AtenPrefs::updateAfterViewPrefs()
{
	gui.mainView.createLists();
	master.currentModel()->renderSource()->projectAll();
	master.currentModel()->renderSource()->logChange(LOG_VISUAL);
	gui.mainView.postRedisplay();
}

void AtenPrefs::setRadiusChanged(Atom::DrawStyle ds, double value)
{
	prefs.setAtomSize(ds, value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_StickRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::StickStyle, value);
}

void AtenPrefs::on_TubeRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::TubeStyle, value);
}

void AtenPrefs::on_SphereRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::SphereStyle, value);
}

void AtenPrefs::on_ScaledRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::ScaledStyle, value);
}

void AtenPrefs::on_TubeBondSizeSpin_valueChanged(double value)
{
	prefs.setTubeSize(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_SelectionScaleSpin_valueChanged(double value)
{
	prefs.setSelectionScale(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_AtomQualitySpin_valueChanged(int value)
{
	prefs.setAtomDetail(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_BondQualitySpin_valueChanged(int value)
{
	prefs.setBondDetail(value);
	updateAfterViewPrefs();
}

void AtenPrefs::setVisibleObject(Prefs::ViewObject vo, int state)
{
	prefs.setVisible(vo, (state == Qt::Checked ? TRUE : FALSE));
	master.currentModel()->logChange(LOG_VISUAL);
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_AtomsVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewAtoms, state);
}

void AtenPrefs::on_CellVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewCell, state);
}

void AtenPrefs::on_AxesVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewCellAxes, state);
}

void AtenPrefs::on_GlobeVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewGlobe, state);
}

/*
// Lighting Page
*/

void AtenPrefs::on_SpotlightGroup_clicked(bool checked)
{
	prefs.setSpotlightActive(checked);
	gui.mainView.initGl();
	gui.mainView.postRedisplay();
}

void AtenPrefs::spotlightPosChanged(int i, double value)
{
	prefs.setSpotlightPosition(i, (GLfloat) value);
	gui.mainView.initGl();
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_SpotlightPositionXSpin_valueChanged(double value)
{
	spotlightPosChanged(0, value);
}

void AtenPrefs::on_SpotlightPositionYSpin_valueChanged(double value)
{
	spotlightPosChanged(1, value);
}

void AtenPrefs::on_SpotlightPositionZSpin_valueChanged(double value)
{
	spotlightPosChanged(2, value);
}

void AtenPrefs::spotlightColourChanged(Prefs::ColourComponent sc)
{
	// Get current component colour and convert it to a QColor
	GLfloat *col = prefs.spotlightColour(sc);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setSpotlightColour(sc, newcol.redF(), newcol.greenF(), newcol.blueF());
	TColourFrame *colframe;
	if (sc == Prefs::AmbientComponent) colframe = ui.SpotlightAmbientColourFrame;
	else if (sc == Prefs::DiffuseComponent) colframe = ui.SpotlightDiffuseColourFrame;
	else if (sc == Prefs::SpecularComponent) colframe = ui.SpotlightSpecularColourFrame;	
	colframe->setColour(newcol);
	colframe->update();
	// Update display
	gui.mainView.initGl();
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_SpotlightAmbientColourButton_clicked(bool checked)
{
	spotlightColourChanged(Prefs::AmbientComponent);
}

void AtenPrefs::on_SpotlightDiffuseColourButton_clicked(bool checked)
{
	spotlightColourChanged(Prefs::DiffuseComponent);
}

void AtenPrefs::on_SpotlightSpecularColourButton_clicked(bool checked)
{
	spotlightColourChanged(Prefs::SpecularComponent);
}

void AtenPrefs::on_ShininessSpin_valueChanged(int value)
{
	prefs.setShininess(value);
	master.currentModel()->logChange(LOG_VISUAL);
	gui.mainView.postRedisplay();
}

/*
// Interact Page
*/

void AtenPrefs::on_LeftMouseCombo_currentIndexChanged(int ma)
{
	prefs.setMouseAction(Prefs::LeftButton, (Prefs::MouseAction) ma);
}

void AtenPrefs::on_MiddleMouseCombo_currentIndexChanged(int ma)
{
	prefs.setMouseAction(Prefs::MiddleButton, (Prefs::MouseAction) ma);
}

void AtenPrefs::on_RightMouseCombo_currentIndexChanged(int ma)
{
	prefs.setMouseAction(Prefs::RightButton, (Prefs::MouseAction) ma);
}

void AtenPrefs::on_WheelMouseCombo_currentIndexChanged(int ma)
{
	prefs.setMouseAction(Prefs::WheelButton, (Prefs::MouseAction) ma);
}

void AtenPrefs::on_ShiftButtonCombo_currentIndexChanged(int ka)
{
	prefs.setKeyAction(Prefs::ShiftKey, (Prefs::KeyAction) ka);
}

void AtenPrefs::on_CtrlButtonCombo_currentIndexChanged(int ka)
{
	prefs.setKeyAction(Prefs::CtrlKey, (Prefs::KeyAction) ka);
}

void AtenPrefs::on_AltButtonCombo_currentIndexChanged(int ka)
{
	prefs.setKeyAction(Prefs::AltKey, (Prefs::KeyAction) ka);
}
