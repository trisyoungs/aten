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
#include "gui/gui.h"
#include "gui/prefs.h"
#include <QtGui/QDialog>
#include <QtGui/QListWidgetItem>

// Constructor
AtenPrefs::AtenPrefs(QDialog *parent) : QDialog(parent)
{
	//for (int i=0; i<SP_NITEMS; i++) stackbuttons[i];
	ui.setupUi(this);
}

// Finalise GUI
void AtenPrefs::finalise_ui()
{
	dbg_begin(DM_CALLS,"AtenPrefs::finalise_ui");
	int i;

	// Add elements to element list and select first item
	QListWidgetItem *item;
	for (i=0; i<NELEMENTS; i++)
	{
		item = new QListWidgetItem(ui.ElementList);
		//item->setText(0, itoa(i));
		item->setText(elements.name(i));
	}
	ui.ElementList->setCurrentRow(0);
}

// Set controls
void AtenPrefs::set_controls()
{
	dbg_begin(DM_CALLS,"AtenPrefs::set_controls");
	// Set controls in view page
	ui.StickRadiusSpin->setValue(prefs.get_atom_size(DS_STICK));
	ui.TubeRadiusSpin->setValue(prefs.get_atom_size(DS_TUBE));
	ui.SphereRadiusSpin->setValue(prefs.get_atom_size(DS_SPHERE));
	ui.ScaledRadiusSpin->setValue(prefs.get_atom_size(DS_SCALED));
	ui.TubeBondSizeSpin->setValue(prefs.get_tube_size());
	ui.SelectionScaleSpin->setValue(prefs.get_selection_scale());
	ui.AtomQualitySpin->setValue(prefs.get_atom_detail());
	ui.BondQualitySpin->setValue(prefs.get_bond_detail());
	ui.GlobeVisibleCheck->setChecked(prefs.should_render(VO_GLOBE));
	ui.CellVisibleCheck->setChecked(prefs.should_render(VO_CELL));
	ui.AxesVisibleCheck->setChecked(prefs.should_render(VO_CELLAXES));
	ui.AtomsVisibleCheck->setChecked(prefs.should_render(VO_ATOMS));

	// Set controls in Lighting page
	GLint *c;
	c = prefs.get_spotlight(SL_AMBIENT);
	ui.AmbientRedSpin->setValue( double(c[0]) / INT_MAX);
	ui.AmbientGreenSpin->setValue( double(c[1]) / INT_MAX);
	ui.AmbientBlueSpin->setValue( double(c[2]) / INT_MAX);
	c = prefs.get_spotlight(SL_DIFFUSE);
	ui.DiffuseRedSpin->setValue( double(c[0]) / INT_MAX);
	ui.DiffuseGreenSpin->setValue( double(c[1]) / INT_MAX);
	ui.DiffuseBlueSpin->setValue( double(c[2]) / INT_MAX);
	c = prefs.get_spotlight(SL_SPECULAR);
	ui.SpecularRedSpin->setValue( double(c[0]) / INT_MAX);
	ui.SpecularGreenSpin->setValue( double(c[1]) / INT_MAX);
	ui.SpecularBlueSpin->setValue( double(c[2]) / INT_MAX);
	c = prefs.get_spotlight(SL_POSITION);
	ui.LightPositionXSpin->setValue(c[0]);
	ui.LightPositionYSpin->setValue(c[1]);
	ui.LightPositionZSpin->setValue(c[2]);
	ui.ShininessSpin->setValue(prefs.get_shininess());

	// Set controls in interaction page
	ui.LeftMouseCombo->setCurrentIndex(prefs.get_mb_action(MB_LEFT));
	ui.MiddleMouseCombo->setCurrentIndex(prefs.get_mb_action(MB_MIDDLE));
	ui.RightMouseCombo->setCurrentIndex(prefs.get_mb_action(MB_RIGHT));
	ui.WheelMouseCombo->setCurrentIndex(prefs.get_mb_action(MB_WHEEL));
	ui.ShiftButtonCombo->setCurrentIndex(prefs.get_keymod_action(MK_SHIFT));
	ui.CtrlButtonCombo->setCurrentIndex(prefs.get_keymod_action(MK_CTRL));
	ui.AltButtonCombo->setCurrentIndex(prefs.get_keymod_action(MK_ALT));
	dbg_begin(DM_CALLS,"AtenPrefs::set_controls");
}

/*
// Element Page
*/

void AtenPrefs::set_element_colour(int type, int component, int value)
{
	int el = ui.ElementList->currentRow();
	if (type == 0) elements.set_ambient(el, component, int((double(value) / 255.0) * INT_MAX));
	else elements.set_diffuse(el, component, int((double(value) / 255.0) * INT_MAX));
	// Re-set atom colours in model(s)
	for (model *m = master.get_models(); m != NULL; m = m->next)
	{
		//m->set_atom_colours(NULL);
		m->log_change(LOG_VISUAL);
	}
	gui.mainview.postredisplay();
}

void AtenPrefs::on_ElementList_currentRowChanged(int row)
{
	// Update the info for the current element
	ui.ElementNameLabel->setText(elements.name(row));
	ui.ElementSymbolLabel->setText(elements.symbol(row));
	ui.ElementMassLabel->setText(ftoa(elements.mass(row)));
	GLint *colour;
	colour = elements.ambient(row);
	ui.ElementARedSpin->setValue( int((double(colour[0]) / INT_MAX) * 255) );
	ui.ElementAGreenSpin->setValue( int((double(colour[1]) / INT_MAX) * 255) );
	ui.ElementABlueSpin->setValue( int((double(colour[2]) / INT_MAX) * 255) );
	ui.ElementAAlphaSpin->setValue( int((double(colour[3]) / INT_MAX) * 255) );
	colour = elements.diffuse(row);
	ui.ElementDRedSpin->setValue( int((double(colour[0]) / INT_MAX) * 255) );
	ui.ElementDGreenSpin->setValue( int((double(colour[1]) / INT_MAX) * 255) );
	ui.ElementDBlueSpin->setValue( int((double(colour[2]) / INT_MAX) * 255) );
	ui.ElementDAlphaSpin->setValue( int((double(colour[3]) / INT_MAX) * 255) );
	ui.ElementRadiusSpin->setValue(elements.radius(row));
}

/*
// View Page
*/

void update_after_viewprefs()
{
	gui.mainview.create_lists();
	master.get_currentmodel()->project_all();
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void AtenPrefs::set_radius_changed(draw_style ds, double value)
{
	prefs.set_atom_size(ds, value);
	update_after_viewprefs();
}

void AtenPrefs::on_TubeBondSizeSpin_valueChanged(double value)
{
	prefs.set_tube_size(value);
	update_after_viewprefs();
}

void AtenPrefs::on_SelectionScaleSpin_valueChanged(double value)
{
	prefs.set_selection_scale(value);
	update_after_viewprefs();
}

void AtenPrefs::on_AtomQualitySpin_valueChanged(int value)
{
	prefs.set_atom_detail(value);
	update_after_viewprefs();
}

void AtenPrefs::on_BondQualitySpin_valueChanged(int value)
{
	prefs.set_bond_detail(value);
	update_after_viewprefs();
}

void AtenPrefs::set_visible_object(view_object vo, int state)
{
	prefs.set_visible(vo, (state == Qt::Checked ? TRUE : FALSE));
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

/*
// Lighting Page
*/

void AtenPrefs::on_SpotlightGroup_clicked(bool checked)
{
	prefs.set_spotlight_on(checked);
	gui.mainview.init_gl();
	gui.refresh();
}

void AtenPrefs::spotlight_changed(spotlight_component so, int i, double value)
{
	prefs.set_spotlight(so, i, (GLint) (value * INT_MAX));
	gui.mainview.init_gl();
	gui.refresh();	
}

void AtenPrefs::on_ShininessSpin_valueChanged(int value)
{
	prefs.set_shininess(value);
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

/*
// Interact Page
*/

void AtenPrefs::mouse_action_changed(mouse_button mb, mouse_action ma)
{
	prefs.set_mb_action(mb, ma);
}

void AtenPrefs::key_modifier_changed(modifier_key km, key_action ka)
{
	prefs.set_keymod_action(km, ka);
}
