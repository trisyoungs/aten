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
#include "gui/selectelement.h"
#include "model/model.h"
#include "base/sysfunc.h"

// Constructor
AtenPrefs::AtenPrefs(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);
	refreshing_ = FALSE;
}

// Destructor
AtenPrefs::~AtenPrefs()
{
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

	// Set controls in view page - Radii
	ui.StickRadiusSpin->setValue(prefs.atomStyleRadius(Atom::StickStyle));
	ui.TubeRadiusSpin->setValue(prefs.atomStyleRadius(Atom::TubeStyle));
	ui.SphereRadiusSpin->setValue(prefs.atomStyleRadius(Atom::SphereStyle));
	ui.ScaledRadiusSpin->setValue(prefs.atomStyleRadius(Atom::ScaledStyle));
	ui.StickBondRadiusSpin->setValue(prefs.bondStyleRadius(Atom::StickStyle));
	ui.TubeBondRadiusSpin->setValue(prefs.bondStyleRadius(Atom::TubeStyle));
	ui.SphereBondRadiusSpin->setValue(prefs.bondStyleRadius(Atom::SphereStyle));
	ui.ScaledBondRadiusSpin->setValue(prefs.bondStyleRadius(Atom::ScaledStyle));
	ui.SelectionScaleSpin->setValue(prefs.selectionScale());
	// ... scene
	ui.GlobeVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewGlobe));
	ui.CellVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewCell));
	ui.AxesVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewCellAxes));
	ui.AtomsVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewAtoms));
	ui.LabelsVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewLabels));
	ui.MeasurementsVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewMeasurements));
	ui.RegionsVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewRegions));
	ui.SurfacesVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewSurfaces));
	ui.GlobeVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewGlobe));
	ui.CellVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewCell));
	ui.AxesVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewCellAxes));
	ui.AtomsVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewAtoms));
	ui.LabelsVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewLabels));
	ui.MeasurementsVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewMeasurements));
	ui.RegionsVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewRegions));
	ui.SurfacesVisibleImageCheck->setChecked(prefs.isVisibleOffScreen(Prefs::ViewSurfaces));
	ui.AngleLabelEdit->setText(prefs.angleLabel());
	ui.DistanceLabelEdit->setText(prefs.distanceLabel());
	// ... lighting
	ui.SpotlightAmbientColourFrame->setColour(prefs.spotlightColour(Prefs::AmbientComponent));
	ui.SpotlightDiffuseColourFrame->setColour(prefs.spotlightColour(Prefs::DiffuseComponent));
	ui.SpotlightSpecularColourFrame->setColour(prefs.spotlightColour(Prefs::SpecularComponent));
	double *pos = prefs.spotlightPosition();
	ui.SpotlightPositionXSpin->setValue(pos[0]);
	ui.SpotlightPositionYSpin->setValue(pos[1]);
	ui.SpotlightPositionZSpin->setValue(pos[2]);
	ui.ShininessSpin->setValue(prefs.shininess());
	// ... GL
	ui.AtomQualitySpin->setValue(prefs.atomDetail());
	ui.BondQualitySpin->setValue(prefs.bondDetail());
	ui.NearClipSpin->setValue(prefs.clipNear());
	ui.FarClipSpin->setValue(prefs.clipFar());
	ui.NearDepthSpin->setValue(prefs.depthNear());
	ui.FarDepthSpin->setValue(prefs.depthFar());

	// Set controls in interaction page
	ui.LeftMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::LeftButton));
	ui.MiddleMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::MiddleButton));
	ui.RightMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::RightButton));
	ui.WheelMouseCombo->setCurrentIndex(prefs.mouseAction(Prefs::WheelButton));
	ui.ShiftButtonCombo->setCurrentIndex(prefs.keyAction(Prefs::ShiftKey));
	ui.CtrlButtonCombo->setCurrentIndex(prefs.keyAction(Prefs::CtrlKey));
	ui.AltButtonCombo->setCurrentIndex(prefs.keyAction(Prefs::AltKey));
	ui.ZoomThrottleSpin->setValue(prefs.zoomThrottle());

	// Set controls in Program page
	ui.CommonElementsEdit->setText(prefs.commonElements());
	ui.DensityUnitCombo->setCurrentIndex(prefs.densityUnit());
	ui.EnergyUnitCombo->setCurrentIndex(prefs.energyUnit());
	ui.EnergyUpdateSpin->setValue(prefs.energyUpdate());
	ui.HAddDistanceSpin->setValue(prefs.hydrogenDistance());
	ui.MaxRingSizeSpin->setValue(prefs.maxRingSize());
	ui.MaxUndoLevelsSpin->setValue(prefs.maxUndoLevels());
	ui.ModelUpdateSpin->setValue(prefs.modelUpdate());

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
	elements().backupData();

	refreshing_ = FALSE;
	msg.exit("AtenPrefs::setControls");
}

// Close window
void AtenPrefs::on_PrefsCancelButton_clicked(bool checked)
{
	// Copy old preferences values back into main structure, update view and close window
	prefs = prefsBackup_;
	elements().restoreData();

	aten.currentModel()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	reject();
}

// Store current prefs values as defaults
void AtenPrefs::on_PrefsSetDefaultButton_clicked(bool checked)
{
	char filename[512];
	sprintf(filename, "%s%s", aten.homeDir(), "/.aten/prefs.dat");
	// Temporarily disable prefs window
	gui.prefsDialog->setEnabled(FALSE);
	bool result = prefs.save(filename);
	gui.prefsDialog->setEnabled(TRUE);
	if (!result) QMessageBox::warning(NULL, "Aten", "User preferences file could not be saved.\n", QMessageBox::Ok, QMessageBox::Ok);
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

void AtenPrefs::setRadiusChanged(Atom::DrawStyle ds, double value, bool foratom)
{
	if (refreshing_) return;
	if (foratom) prefs.setAtomStyleRadius(ds, value);
	else prefs.setBondStyleRadius(ds, value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_StickRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::StickStyle, value, TRUE);
}

void AtenPrefs::on_TubeRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::TubeStyle, value, TRUE);
}

void AtenPrefs::on_SphereRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::SphereStyle, value, TRUE);
}

void AtenPrefs::on_ScaledRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::ScaledStyle, value, TRUE);
}

void AtenPrefs::on_StickBondRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::StickStyle, value, FALSE);
}

void AtenPrefs::on_TubeBondRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::TubeStyle, value, FALSE);
}

void AtenPrefs::on_SphereBondRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::SphereStyle, value, FALSE);
}

void AtenPrefs::on_ScaledBondRadiusSpin_valueChanged(double value)
{
	setRadiusChanged(Atom::ScaledStyle, value, FALSE);
}

void AtenPrefs::on_SelectionScaleSpin_valueChanged(double value)
{
	prefs.setSelectionScale(value);
	updateAfterViewPrefs();
}

/*
// View [GL] page
*/

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

void AtenPrefs::on_FarClipSpin_valueChanged(double value)
{
	prefs.setClipFar(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_FarDepthSpin_valueChanged(int value)
{
	prefs.setDepthFar(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_NearClipSpin_valueChanged(double value)
{
	prefs.setClipNear(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_NearDepthSpin_valueChanged(int value)
{
	prefs.setDepthNear(value);
	updateAfterViewPrefs();
}

void AtenPrefs::on_LineAliasingCheck_stateChanged(int state)
{
	prefs.setLineAliasing(state == Qt::Checked);
	updateAfterViewPrefs();
}

void AtenPrefs::on_PolygonAliasingCheck_stateChanged(int state)
{
	prefs.setPolygonAliasing(state == Qt::Checked);
	updateAfterViewPrefs();
}

/*
// View [Scene] page
*/

void AtenPrefs::setVisibleObject(Prefs::ViewObject vo, int state, bool onscreen)
{
	if (onscreen)
	{
		prefs.setVisibleOnScreen(vo, (state == Qt::Checked ? TRUE : FALSE));
		aten.currentModel()->changeLog.add(Log::Visual);
	}
	else prefs.setVisibleOffScreen(vo, (state == Qt::Checked ? TRUE : FALSE));
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_AtomsVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewAtoms, state, TRUE);
}

void AtenPrefs::on_CellVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewCell, state, TRUE);
}

void AtenPrefs::on_AxesVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewCellAxes, state, TRUE);
}

void AtenPrefs::on_GlobeVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewGlobe, state, TRUE);
}

void AtenPrefs::on_LabelsVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewLabels, state, TRUE);
}

void AtenPrefs::on_MeasurementsVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewMeasurements, state, TRUE);
}

void AtenPrefs::on_RegionsVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewRegions, state, TRUE);
}

void AtenPrefs::on_SurfacesVisibleCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewSurfaces, state, TRUE);
}

void AtenPrefs::on_AtomsVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewAtoms, state, FALSE);
}

void AtenPrefs::on_CellVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewCell, state, FALSE);
}

void AtenPrefs::on_AxesVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewCellAxes, state, FALSE);
}

void AtenPrefs::on_GlobeVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewGlobe, state, FALSE);
}

void AtenPrefs::on_LabelsVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewLabels, state, FALSE);
}

void AtenPrefs::on_MeasurementsVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewMeasurements, state, FALSE);
}

void AtenPrefs::on_RegionsVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewRegions, state, FALSE);
}

void AtenPrefs::on_SurfacesVisibleImageCheck_stateChanged(int state)
{
	setVisibleObject(Prefs::ViewSurfaces, state, FALSE);
}

void AtenPrefs::on_AngleLabelEdit_textEdited(const QString &text)
{
	prefs.setAngleLabel( qPrintable(text) );
}

void AtenPrefs::on_DistanceLabelEdit_textEdited(const QString &text)
{
	prefs.setDistanceLabel( qPrintable(text) );
}

/*
// View [Lighting] Page
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
// Interaction Page
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

void AtenPrefs::on_ZoomThrottleSpin_valueChanged(double value)
{
	prefs.setZoomThrottle(value);
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

/*
// Program Page
*/

void AtenPrefs::on_CommonElementsEdit_textEdited(const QString &text)
{
	prefs.setCommonElements( qPrintable(text) );
	gui.selectElementDialog->addCommonButtons(prefs.commonElements());
}

void AtenPrefs::on_DensityUnitCombo_currentIndexChanged(int index)
{
	prefs.setDensityUnit( (Prefs::DensityUnit) index );
}

void AtenPrefs::on_EnergyUnitCombo_currentIndexChanged(int index)
{
	prefs.setEnergyUnit( (Prefs::EnergyUnit) index );
}

void AtenPrefs::on_EnergyUpdateSpin_valueChanged(int value)
{
	prefs.setEnergyUpdate(value);
}

void AtenPrefs::on_HAddDistanceSpin_valueChanged(double value)
{
	prefs.setHydrogenDistance(value);
}

void AtenPrefs::on_MaxRingSizeSpin_valueChanged(int value)
{
	prefs.setMaxRingSize(value);
}

void AtenPrefs::on_MaxUndoLevelsSpin_valueChanged(int value)
{
	prefs.setMaxUndoLevels(value);
}

void AtenPrefs::on_ModelUpdateSpin_valueChanged(int value)
{
	prefs.setModelUpdate(value);
}
