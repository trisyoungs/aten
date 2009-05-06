/*
	*** Qt prefs window functions
	*** src/gui/prefs_funcs.cpp
	Copyright T. Youngs 2007-2009

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

#include "main/aten.h"
#include "gui/prefs.h"
#include "model/model.h"
#include "base/sysfunc.h"

// Constructor
AtenPrefs::AtenPrefs(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);
	elementsBackup_ = NULL;
	refreshing_ = FALSE;
}

// Destructor
AtenPrefs::~AtenPrefs()
{
	// Free element backup
	if (elementsBackup_ != NULL) delete[] elementsBackup_;
}

// Finalise GUI
void AtenPrefs::finaliseUi()
{
	msg.enter("AtenPrefs::finaliseUi");
	int i;
	// Add elements to element list and select first item
	QListWidgetItem *item;
	for (i=0; i<elements().nElements(); i++)
	{
		item = new QListWidgetItem(ui.ElementList);
		item->setText(elements().name(i));
	}
	ui.ElementList->setCurrentRow(0);
	msg.exit("AtenPrefs::finaliseUi");
}

// Set controls
void AtenPrefs::setControls()
{
	msg.enter("AtenPrefs::setControls");
	refreshing_ = TRUE;

	// Select the first element in the elements list
	ui.ElementList->setCurrentRow(0);

	// Set controls in view page
	ui.StickRadiusSpin->setValue(prefs.atomStyleRadius(Atom::StickStyle));
	ui.TubeRadiusSpin->setValue(prefs.atomStyleRadius(Atom::TubeStyle));
	ui.SphereRadiusSpin->setValue(prefs.atomStyleRadius(Atom::SphereStyle));
	ui.ScaledRadiusSpin->setValue(prefs.atomStyleRadius(Atom::ScaledStyle));
// 	ui.BondRadiusSpin->setValue(prefs.bondRadius());       TGAY
	ui.SelectionScaleSpin->setValue(prefs.selectionScale());
	ui.AtomQualitySpin->setValue(prefs.atomDetail());
	ui.BondQualitySpin->setValue(prefs.bondDetail());
	ui.GlobeVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewGlobe));
	ui.CellVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewCell));
	ui.AxesVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewCellAxes));
	ui.AtomsVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewAtoms));
	ui.ShininessSpin->setValue(prefs.shininess());

	// Set controls in Lighting page
	ui.SpotlightAmbientColourFrame->setColour(prefs.spotlightColour(Prefs::AmbientComponent));
	ui.SpotlightDiffuseColourFrame->setColour(prefs.spotlightColour(Prefs::DiffuseComponent));
	ui.SpotlightSpecularColourFrame->setColour(prefs.spotlightColour(Prefs::SpecularComponent));
	double *pos = prefs.spotlightPosition();
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

	// Set pen colours and colourscale names and checks
        ui.ForegroundColourFrame->setColour(prefs.colour(Prefs::ForegroundColour));
        ui.ForegroundColourFrame->update();
        ui.BackgroundColourFrame->setColour(prefs.colour(Prefs::BackgroundColour));
        ui.BackgroundColourFrame->update();
        ui.SpecularColourFrame->setColour(prefs.colour(Prefs::SpecularColour));
        ui.SpecularColourFrame->update();
	QListWidgetItem *item;
	char name[128];
	for (int n=0; n<10; n++)
	{
		item = ui.ScaleList->item(n);
		sprintf(name, "%i. %s", n+1, prefs.colourScale[n].name());
		item->setCheckState( prefs.colourScale[n].visible() ? Qt::Checked : Qt::Unchecked);
	}
	updateScalePointsList();

	// Store current values in the Prefs structure...
	prefsBackup_ = prefs;
	// If this is the first time, create the elements backup array
	if (elementsBackup_ == NULL) elementsBackup_ = new Element[elements().nElements()];
	int n,i;
	for (i=0; i<elements().nElements(); i++)
	{
		elementsBackup_[i].atomicRadius = elements().atomicRadius(i);
		for (n=0; n<4; ++n)
		{
			elementsBackup_[i].ambientColour[n] = elements().el[i].ambientColour[n];
			elementsBackup_[i].diffuseColour[n] = elements().el[i].diffuseColour[n];
		}
	}
	refreshing_ = FALSE;
	msg.exit("AtenPrefs::setControls");
}

// Close window
void AtenPrefs::on_PrefsCancelButton_clicked(bool checked)
{
	// Copy old preferences values back into main structure, update view and close window
	prefs = prefsBackup_;
	int i,n;
	for (i=0; i<elements().nElements(); i++)
	{
		elements().setAtomicRadius(i,elementsBackup_[i].atomicRadius);
		for (n=0; n<4; ++n)
		{
			elements().el[i].ambientColour[n] = elementsBackup_[i].ambientColour[n];
			elements().el[i].diffuseColour[n] = elementsBackup_[i].ambientColour[n];
		}
	}
	aten.currentModel()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	reject();
}

/*
// Element Page
*/

void AtenPrefs::on_ElementList_currentRowChanged(int row)
{
	// Update the info for the current element
	ui.ElementNameLabel->setText(elements().name(row));
	ui.ElementSymbolLabel->setText(elements().symbol(row));
	ui.ElementMassLabel->setText(ftoa(elements().atomicMass(row)));
	ui.ElementAmbientColourFrame->setColour(elements().ambientColour(row));
	ui.ElementDiffuseColourFrame->setColour(elements().diffuseColour(row));
	ui.ElementRadiusSpin->setValue(elements().atomicRadius(row));
}

void AtenPrefs::on_ElementAmbientColourButton_clicked(bool checked)
{
	// Get current row
	int el = ui.ElementList->currentRow();
	if (el == -1) return;
	// Get element's current ambient colour and convert into a QColor
	double *col = elements().ambientColour(el);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	elements().setAmbientColour(el, newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.ElementAmbientColourFrame->setColour(newcol);
	ui.ElementAmbientColourFrame->update();
	// Re-set atom colours in model(s)
	aten.currentModel()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_ElementDiffuseColourButton_clicked(bool checked)
{
	// Get current row
	int el = ui.ElementList->currentRow();
	if (el == -1) return;
	// Get element's current diffuse colour and convert into a QColor
	double *col = elements().diffuseColour(el);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	elements().setDiffuseColour(el, newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.ElementDiffuseColourFrame->setColour(newcol);
	ui.ElementDiffuseColourFrame->update();
	// Re-set atom colours in model(s)
	aten.currentModel()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}

/*
// View Page
*/

void AtenPrefs::updateAfterViewPrefs()
{
	if (refreshing_) return;
	gui.mainView.createLists();
	aten.currentModel()->renderSource()->projectAll();
	aten.currentModel()->renderSource()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}

void AtenPrefs::setRadiusChanged(Atom::DrawStyle ds, double value)
{
	if (refreshing_) return;
	prefs.setAtomStyleRadius(ds, value);
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

void AtenPrefs::on_BondRadiusSpin_valueChanged(double value)
{
	// TGAY
// 	prefs.setBondRadius(value);
	printf("Bond radius setting from GUI needs to be expanded.\n");
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
	prefs.setVisibleOnScreen(vo, (state == Qt::Checked ? TRUE : FALSE));
	aten.currentModel()->changeLog.add(Log::Visual);
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
	double *col = prefs.spotlightColour(sc);
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
	aten.currentModel()->changeLog.add(Log::Visual);
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

/*
// Colours Page
*/

void AtenPrefs::on_ForegroundColourButton_clicked(bool checked)
{
	double *col = prefs.colour(Prefs::ForegroundColour);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setColour(Prefs::ForegroundColour, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.ForegroundColourFrame->setColour(newcol);
	ui.ForegroundColourFrame->update();
	// Update display
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_BackgroundColourButton_clicked(bool checked)
{
	double *col = prefs.colour(Prefs::BackgroundColour);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setColour(Prefs::BackgroundColour, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.BackgroundColourFrame->setColour(newcol);
	ui.BackgroundColourFrame->update();
	// Update display
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_SpecularColourButton_clicked(bool checked)
{
	double *col = prefs.colour(Prefs::SpecularColour);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setColour(Prefs::SpecularColour, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.SpecularColourFrame->setColour(newcol);
	ui.SpecularColourFrame->update();
	// Update display
	gui.mainView.postRedisplay();

}

void AtenPrefs::updateScalePointsList()
{
	// Clear current list items
	ui.ScalePointsList->clear();
	// Get the id of the currently selected point and scale
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	// Cycle over scale points and add the items
	GLfloat colour[4];
	QListWidgetItem *item;
	for (ColourScalePoint *csp = prefs.colourScale[scale].firstPoint(); csp != NULL; csp = csp->next)
	{
		item = new QListWidgetItem(ftoa(csp->value()), ui.ScalePointsList);
		csp->copyColour(colour);
		item->setBackgroundColor(QColor(int(colour[0]*255),int(colour[1]*255),int(colour[2]*255)));
	}
	// Select first item in list
	ui.ScalePointsList->setCurrentItem(0);
}

void AtenPrefs::on_ScaleList_currentRowChanged(int id)
{
	if (id == -1) return;
	// Scale selection has changed, so update points list
	updateScalePointsList();
	// Set name in lineedit
	ui.ScaleNameEdit->setText(prefs.colourScale[id].name());
}

void AtenPrefs::on_ScalePointsList_currentRowChanged(int id)
{
	// Get the id of the currently selected point and scale
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	if (id == -1) return;
	// Set colour frame and value spin
	ColourScalePoint *csp = prefs.colourScale[scale].point(id);
	ui.PointColourFrame->setColour(csp->colour());
	ui.PointColourFrame->update();
	ui.PointValueSpin->setValue(csp->value());
}

void AtenPrefs::on_ScaleNameEdit_returnPressed()
{
	// Get the id of the currently selected point and scale
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	prefs.colourScale[scale].setName( qPrintable(ui.ScaleNameEdit->text()) );
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_PointValueSpin_valueChanged(double d)
{
	// Get the id of the currently selected point and scale
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	int id = ui.ScalePointsList->currentRow();
	if (id == -1) return;
	// Set value in colourscale
	prefs.colourScale[scale].setPointValue(id, d);
	ui.ScalePointsList->item(id)->setText(ftoa(d));
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_PointColourButton_clicked(bool checked)
{
	// Get the id of the currently selected point and scale
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	int id = ui.ScalePointsList->currentRow();
	if (id == -1) return;
	// Get new colour
	ColourScalePoint *csp = prefs.colourScale[scale].point(id);
	double *col = csp->colour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour, and set colours in frame and pointlist
	prefs.colourScale[scale].setPointColour(id, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.PointColourFrame->setColour(newcol);
	ui.PointColourFrame->update();
	ui.ScalePointsList->item(id)->setBackgroundColor(newcol);
	// Update display
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_AddPointButton_clicked(bool checked)
{
	// Get the id of the currently selected scale
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	// Add a new point to the end of the scale and refresh the list
	double value = (prefs.colourScale[scale].nPoints() == 0 ? 0.0 : prefs.colourScale[scale].lastPoint()->value() + 1.0);
	prefs.colourScale[scale].addPointAtEnd(value, 0.5f, 0.5f, 0.5f);
	updateScalePointsList();
}

void AtenPrefs::on_RemovePointButton_clicked(bool checked)
{
	// Get the id of the currently selected scale and point
	int scale = ui.ScaleList->currentRow();
	if (scale == -1) return;
	int id = ui.ScalePointsList->currentRow();
	if (id == -1) return;
	// Remove selected point
	prefs.colourScale[scale].removePoint(id);
	updateScalePointsList();
}

void AtenPrefs::on_ScaleList_itemClicked(QListWidgetItem *item)
{
	// Get row number associated with item
	int row = ui.ScaleList->row(item);
	if (row == -1) return;
	// Look at checked state
	prefs.colourScale[row].setVisible( (item->checkState() == Qt::Checked ? TRUE : FALSE) );
	gui.mainView.postRedisplay();
}
