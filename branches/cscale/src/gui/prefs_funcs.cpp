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
#include "gui/prefs.h"
#include <QtGui/QListWidgetItem>
#include <QtGui/QColorDialog>
#include "model/model.h"

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
	// Create grid layout for ColourScalesWidget
	QGridLayout *gl = new QGridLayout;
	gl->setMargin(0);
	gl->setSpacing(2);
	// Create widgets in grid
	gl->setColumnStretch(1,1);
	for (i=0; i<10; i++)
	{
		gl->setRowMinimumHeight(i,30);
		scaleNameEdit_[i] = new QLineEdit(this);
		scaleNameEdit_[i]->setText("Name");
		QObject::connect(scaleNameEdit_[i], SIGNAL(returnPressed()), this, SLOT(colourScale_NameChanged()));
		gl->addWidget(scaleNameEdit_[i], i*2, 0);
		scaleLinksLabel_[i] = new QLabel(this);
		scaleLinksLabel_[i]->setText("[0]");
		gl->addWidget(scaleLinksLabel_[i], i*2, 8);

		scaleThreeCheck_[i] = new QCheckBox(this);
		scaleThreeCheck_[i]->setText("3-Point");
		QObject::connect(scaleThreeCheck_[i], SIGNAL(clicked(bool)), this, SLOT(colourScale_TypeChanged(bool)));
		gl->addWidget(scaleThreeCheck_[i], i*2+1, 0);

		scaleMinColourFrame_[i] = new TColourFrame(this);
		scaleMinColourFrame_[i]->setMinimumSize(30,30);
		scaleMinColourFrame_[i]->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		gl->addWidget(scaleMinColourFrame_[i], i*2, 1);

		scaleMinColourButton_[i] = new QPushButton(this);
		scaleMinColourButton_[i]->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
		scaleMinColourButton_[i]->setMinimumSize(30,30);
		scaleMinColourButton_[i]->setMaximumSize(30,30);
		QObject::connect(scaleMinColourButton_[i], SIGNAL(clicked(bool)), this, SLOT(colourScale_ColourChanged(bool)));
		gl->addWidget(scaleMinColourButton_[i], i*2, 2);

		scaleMidColourFrame_[i] = new TColourFrame(this);
		scaleMidColourFrame_[i]->setMinimumSize(30,30);
		scaleMidColourFrame_[i]->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		gl->addWidget(scaleMidColourFrame_[i], i*2, 3);

		scaleMidColourButton_[i] = new QPushButton(this);
		scaleMidColourButton_[i]->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
		scaleMidColourButton_[i]->setMinimumSize(30,30);
		scaleMidColourButton_[i]->setMaximumSize(30,30);
		QObject::connect(scaleMidColourButton_[i], SIGNAL(clicked(bool)), this, SLOT(colourScale_ColourChanged(bool)));
		gl->addWidget(scaleMidColourButton_[i], i*2, 4);

		scaleMaxColourFrame_[i] = new TColourFrame(this);
		scaleMaxColourFrame_[i]->setMinimumSize(30,30);
		scaleMaxColourFrame_[i]->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		gl->addWidget(scaleMaxColourFrame_[i], i*2, 5);

		scaleMaxColourButton_[i] = new QPushButton(this);
		scaleMaxColourButton_[i]->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
		scaleMaxColourButton_[i]->setMinimumSize(30,30);
		scaleMaxColourButton_[i]->setMaximumSize(30,30);
		QObject::connect(scaleMaxColourButton_[i], SIGNAL(clicked(bool)), this, SLOT(colourScale_ColourChanged(bool)));
		gl->addWidget(scaleMaxColourButton_[i], i*2, 6);

		scaleMinSpin_[i] = new QDoubleSpinBox(this);
		scaleMinSpin_[i]->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		scaleMinSpin_[i]->setMinimumSize(64,30);
		scaleMinSpin_[i]->setDecimals(4);
		scaleMinSpin_[i]->setRange(-1.0e6,1.0e6);
		QObject::connect(scaleMinSpin_[i], SIGNAL(valueChanged(double)), this, SLOT(colourScale_RangeChanged(double)));
		gl->addWidget(scaleMinSpin_[i], i*2+1, 1, 1, 2);

		scaleMidSpin_[i] = new QDoubleSpinBox(this);
		scaleMidSpin_[i]->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		scaleMidSpin_[i]->setMinimumSize(64,30);
		scaleMidSpin_[i]->setDecimals(4);
		scaleMidSpin_[i]->setRange(-1.0e6,1.0e6);
		QObject::connect(scaleMidSpin_[i], SIGNAL(valueChanged(double)), this, SLOT(colourScale_RangeChanged(double)));
		gl->addWidget(scaleMidSpin_[i], i*2+1, 3, 1, 2);

		scaleMaxSpin_[i] = new QDoubleSpinBox(this);
		scaleMaxSpin_[i]->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		scaleMaxSpin_[i]->setMinimumSize(64,30);
		scaleMaxSpin_[i]->setDecimals(4);
		scaleMaxSpin_[i]->setRange(-1.0e6,1.0e6);
		QObject::connect(scaleMaxSpin_[i], SIGNAL(valueChanged(double)), this, SLOT(colourScale_RangeChanged(double)));
		gl->addWidget(scaleMaxSpin_[i], i*2+1, 5, 1, 2);
	}
	// Put grid layout into frame
	QVBoxLayout *vbox = new QVBoxLayout;
	vbox->insertLayout(0,gl);
	QFrame *frame = new QFrame;
	frame->setParent(0);
	frame->setLayout(vbox);
	// Add QScrollArea to colour scales widget
	QScrollArea *qsa = new QScrollArea;
	qsa->setWidget(frame);
	vbox = new QVBoxLayout;
	vbox->setSizeConstraint(QLayout::SetNoConstraint);
	vbox->insertWidget(0,qsa);
	ui.ColourScaleGroup->setLayout(vbox);
	dbgEnd(Debug::Calls,"AtenPrefs::finaliseUi");
}

// Set controls
void AtenPrefs::setControls()
{
	dbgBegin(Debug::Calls,"AtenPrefs::setControls");
	refreshing_ = TRUE;
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
	ui.GlobeVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewGlobe));
	ui.CellVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewCell));
	ui.AxesVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewCellAxes));
	ui.AtomsVisibleCheck->setChecked(prefs.isVisibleOnScreen(Prefs::ViewAtoms));
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

	// Set colourframe colours etc.
	GLfloat col[4];
	char links[32];
	ui.ForegroundColourFrame->setColour(prefs.penColour(Prefs::ForegroundColour));
	ui.ForegroundColourFrame->update();
	ui.BackgroundColourFrame->setColour(prefs.penColour(Prefs::BackgroundColour));
	ui.BackgroundColourFrame->update();
	ui.SpecularColourFrame->setColour(prefs.penColour(Prefs::SpecularColour));
	ui.SpecularColourFrame->update();
	for (int n=0; n<10; n++)
	{
		// Name / links
		scaleNameEdit_[n]->setText(prefs.colourScale[n].name());
		sprintf(links,"[%i]",prefs.colourScale[n].nLinks());
		scaleLinksLabel_[n]->setText(links);
		// Colours
		prefs.colourScale[n].copyColour(ColourScale::MinColour, col);
		scaleMinColourFrame_[n]->setColour(col);
		scaleMinColourFrame_[n]->update();
		prefs.colourScale[n].copyColour(ColourScale::MidColour, col);
		scaleMidColourFrame_[n]->setColour(col);
		scaleMidColourFrame_[n]->update();
		prefs.colourScale[n].copyColour(ColourScale::MaxColour, col);
		scaleMaxColourFrame_[n]->setColour(col);
		scaleMaxColourFrame_[n]->update();
		// Numerical ranges
		scaleMinSpin_[n]->setValue(prefs.colourScale[n].minimum());
		scaleMidSpin_[n]->setValue(prefs.colourScale[n].middle());
		scaleMaxSpin_[n]->setValue(prefs.colourScale[n].maximum());
		// Type
		if (prefs.colourScale[n].type() == ColourScale::ThreePoint)
		{
			scaleThreeCheck_[n]->setChecked(TRUE);
			scaleMidSpin_[n]->setEnabled(TRUE);
			scaleMidColourButton_[n]->setEnabled(TRUE);
		}
		else
		{
			scaleThreeCheck_[n]->setChecked(FALSE);
			scaleMidSpin_[n]->setEnabled(FALSE);
			scaleMidColourButton_[n]->setEnabled(FALSE);
		}
	}
	// Store current values in the Prefs structure...
	prefsBackup_ = prefs;
	// If this is the first time, create the elements backup array
	if (elementsBackup_ == NULL) elementsBackup_ = new Element[elements.nElements()];
	for (int i=0; i<elements.nElements(); i++)
	{
		elementsBackup_[i].atomicRadius = elements.atomicRadius(i);
		elements.copyAmbientColour(i, elementsBackup_[i].ambientColour);
		elements.copyDiffuseColour(i, elementsBackup_[i].diffuseColour);
	}
	refreshing_ = FALSE;
	dbgEnd(Debug::Calls,"AtenPrefs::setControls");
}

// Close window
void AtenPrefs::on_PrefsCancelButton_clicked(bool checked)
{
	// Copy old preferences values back into main structure, update view and close window
	prefs = prefsBackup_;
	for (int i=0; i<elements.nElements(); i++)
	{
		elements.setAtomicRadius(i,elementsBackup_[i].atomicRadius);
		elements.setAmbientColour(i, elementsBackup_[i].ambientColour[0], elementsBackup_[i].ambientColour[1], elementsBackup_[i].ambientColour[2]);
		elements.setDiffuseColour(i, elementsBackup_[i].diffuseColour[0], elementsBackup_[i].diffuseColour[1], elementsBackup_[i].diffuseColour[2]);
	}
	master.currentModel()->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	reject();
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
	master.currentModel()->logChange(Change::VisualLog);
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
	master.currentModel()->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
}

/*
// View Page
*/

void AtenPrefs::updateAfterViewPrefs()
{
	if (refreshing_) return;
	gui.mainView.createLists();
	master.currentModel()->renderSource()->projectAll();
	master.currentModel()->renderSource()->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
}

void AtenPrefs::setRadiusChanged(Atom::DrawStyle ds, double value)
{
	if (refreshing_) return;
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
	prefs.setVisibleOnScreen(vo, (state == Qt::Checked ? TRUE : FALSE));
	master.currentModel()->logChange(Change::VisualLog);
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
	master.currentModel()->logChange(Change::VisualLog);
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
	GLfloat *col = prefs.penColour(Prefs::ForegroundColour);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setPenColour(Prefs::ForegroundColour, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.ForegroundColourFrame->setColour(newcol);
	ui.ForegroundColourFrame->update();
	// Update display
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_BackgroundColourButton_clicked(bool checked)
{
	GLfloat *col = prefs.penColour(Prefs::BackgroundColour);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setPenColour(Prefs::BackgroundColour, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
printf("%f %f %f %f\n", newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.BackgroundColourFrame->setColour(newcol);
	ui.BackgroundColourFrame->update();
	// Update display
	gui.mainView.postRedisplay();
}

void AtenPrefs::on_SpecularColourButton_clicked(bool checked)
{
	GLfloat *col = prefs.penColour(Prefs::SpecularColour);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	prefs.setPenColour(Prefs::SpecularColour, newcol.redF(), newcol.greenF(), newcol.blueF(), 1.0);
	ui.SpecularColourFrame->setColour(newcol);
	ui.SpecularColourFrame->update();
	// Update display
	gui.mainView.postRedisplay();
}

void AtenPrefs::colourScale_ColourChanged(bool checked)
{
	// Cast sender
	QPushButton *button = qobject_cast<QPushButton*> (sender());
	if (!button)
	{
		printf("AtenPrefs::colourScale_ColourChanged - Sender was not a QPushButton.\n");
		return;
	}
	// Find which button sent the signal
	int n;
	ColourScale::ScaleColour type;
	for (n=0; n<10; n++)
	{
		if (scaleMinColourButton_[n] == button) { type = ColourScale::MinColour; break; }
		else if (scaleMidColourButton_[n] == button) { type = ColourScale::MidColour; break; }
		else if (scaleMaxColourButton_[n] == button) { type = ColourScale::MaxColour; break; }
	}
	if (n == 10) return;
	// Get current scale colour and convert it to a QColor
	GLfloat col[4];
	prefs.colourScale[n].copyColour(type, col);
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	if (!oldcol.isValid()) return;
	// Store new colour
	prefs.colourScale[n].setColour(type, newcol.redF(), newcol.greenF(), newcol.blueF());
	TColourFrame *colframe;
	if (type == ColourScale::MinColour) colframe = scaleMinColourFrame_[n];
	else if (type == ColourScale::MidColour) colframe = scaleMidColourFrame_[n];
	else if (type == ColourScale::MaxColour) colframe = scaleMaxColourFrame_[n];
	colframe->setColour(newcol);
	colframe->update();
	// Update display
	master.currentModel()->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
}

void AtenPrefs::colourScale_TypeChanged(bool checkedex)
{
	if (refreshing_) return;
	// Cast sender
	QCheckBox *check = qobject_cast<QCheckBox*> (sender());
	if (!check)
	{
		printf("AtenPrefs::colourScale_TypeChanged - Sender was not a QCheckBox.\n");
		return;
	}
	// Find which checkbox sent the signal
	int n;
	for (n=0; n<10; n++) if (scaleThreeCheck_[n] == check) break;
	if (n == 10) return;
	prefs.colourScale[n].setType( scaleThreeCheck_[n]->checkState() == Qt::Checked ? ColourScale::ThreePoint : ColourScale::TwoPoint );
	setControls();
	// Update display
	master.currentModel()->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
}

void AtenPrefs::colourScale_RangeChanged(double d)
{
	if (refreshing_) return;
	refreshing_ = TRUE;
	// Cast sender
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (!spin)
	{
		printf("AtenPrefs::colourScale_RangeChanged - Sender was not a QDoubleSpinBox.\n");
		return;
	}
	// Find which button sent the signal
	int n;
	ColourScale::ScaleColour type;
	for (n=0; n<10; n++)
	{
		if (scaleMinSpin_[n] == spin) { type = ColourScale::MinColour; break; }
		else if (scaleMidSpin_[n] == spin) { type = ColourScale::MidColour; break; }
		else if (scaleMaxSpin_[n] == spin) { type = ColourScale::MaxColour; break; }
	}
	if (n == 10) return;
	// Set range for the scale
	if (type != ColourScale::MidColour)
	{
		prefs.colourScale[n].setRange(scaleMinSpin_[n]->value(), scaleMaxSpin_[n]->value());
		scaleMidSpin_[n]->setRange(scaleMinSpin_[n]->value(), scaleMaxSpin_[n]->value());
		scaleMidSpin_[n]->setValue(prefs.colourScale[n].middle());
	}
	else prefs.colourScale[n].setMiddle(scaleMidSpin_[n]->value());
	// Update display
	master.currentModel()->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	refreshing_ = FALSE;
}

void AtenPrefs::colourScale_NameChanged()
{
	if (refreshing_) return;
	// Cast sender
	QLineEdit *lineedit = qobject_cast<QLineEdit*> (sender());
	if (!lineedit)
	{
		printf("AtenPrefs::colourScale_NameChanged - Sender was not a QLineEdit.\n");
		return;
	}
	// Find which lineedit sent the signal
	int n;
	for (n=0; n<10; n++) if (scaleNameEdit_[n] == lineedit) break;
	if (n == 10) return;
	prefs.colourScale[n].setName( qPrintable(scaleNameEdit_[n]->text()) );
	gui.mainView.postRedisplay();
}