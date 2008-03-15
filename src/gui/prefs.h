/*
	*** Qt prefs window declaration
	*** src/gui/prefs.h
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

#ifndef ATEN_PREFSWINDOW_H
#define ATEN_PREFSWINDOW_H

#include "gui/gui.h"
#include "gui/ui_prefs.h"

// Program preferences window
class AtenPrefs : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Element Page
	*/
	private slots:
	void on_ElementList_currentRowChanged(int row);
	void on_ElementAmbientColourButton_clicked(bool checked);
	void on_ElementDiffuseColourButton_clicked(bool checked);

	/*
	// View Page
	*/
	private:
	void set_radius_changed(draw_style ds, double value);
	void set_visible_object(view_object vo, int state);
	private slots:
	void on_StickRadiusSpin_valueChanged(double value) { set_radius_changed(DS_STICK, value); }
	void on_TubeRadiusSpin_valueChanged(double value) { set_radius_changed(DS_TUBE, value); }
	void on_SphereRadiusSpin_valueChanged(double value) { set_radius_changed(DS_SPHERE, value); }
	void on_ScaledRadiusSpin_valueChanged(double value) { set_radius_changed(DS_SCALED, value); }
	void on_TubeBondSizeSpin_valueChanged(double value);
	void on_SelectionScaleSpin_valueChanged(double value);
	void on_AtomQualitySpin_valueChanged(int value);
	void on_BondQualitySpin_valueChanged(int value);
	void on_AtomsVisibleCheck_stateChanged(int state) { set_visible_object(VO_ATOMS, state); }
	void on_CellVisibleCheck_stateChanged(int state) { set_visible_object(VO_CELL, state); }
	void on_AxesVisibleCheck_stateChanged(int state) { set_visible_object(VO_CELLAXES, state); }
	void on_GlobeVisibleCheck_stateChanged(int state) { set_visible_object(VO_GLOBE, state); }

	/*
	// Lighting Page
	*/
	private:
	void spotlightpos_changed(int i, double value);
	private slots:
	void on_SpotlightGroup_clicked(bool checked);
	void on_SpotlightAmbientColourButton_clicked(bool checked);
	void on_SpotlightDiffuseColourButton_clicked(bool checked);
	void on_SpotlightSpecularColourButton_clicked(bool checked);
	void on_SpotlightPositionXSpin_valueChanged(double value) { spotlightpos_changed(0, value); }
	void on_SpotlightPositionYSpin_valueChanged(double value) { spotlightpos_changed(1, value); }
	void on_SpotlightPositionZSpin_valueChanged(double value) { spotlightpos_changed(2, value); }
	void on_ShininessSpin_valueChanged(int value);

	/*
	// Interaction page
	*/
	private:
	void mouse_action_changed(mouse_button mb, mouse_action ma);
	void key_modifier_changed(modifier_key km, key_action ka);
	private slots:
	void on_LeftMouseCombo_currentIndexChanged(int ma) { mouse_action_changed(MB_LEFT, (mouse_action) ma); }
	void on_MiddleMouseCombo_currentIndexChanged(int ma) { mouse_action_changed(MB_MIDDLE, (mouse_action) ma); }
	void on_RightMouseCombo_currentIndexChanged(int ma) { mouse_action_changed(MB_RIGHT, (mouse_action) ma); }
	void on_WheelMouseCombo_currentIndexChanged(int ma) { mouse_action_changed(MB_WHEEL, (mouse_action) ma); }
	void on_ShiftButtonCombo_currentIndexChanged(int ka) { key_modifier_changed(MK_SHIFT, (key_action) ka); }
	void on_CtrlButtonCombo_currentIndexChanged(int ka) { key_modifier_changed(MK_CTRL, (key_action) ka); }
	void on_AltButtonCombo_currentIndexChanged(int ka) { key_modifier_changed(MK_ALT, (key_action) ka); }

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenPrefs(QDialog *parent = 0);
	// Main form declaration
	Ui::PrefsDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finalise_ui();
	// Set controls to reflect program variables
	void set_controls();
};

#endif
