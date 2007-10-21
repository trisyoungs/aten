/*
	*** Qt prefs window declaration
	*** src/gui-qt/prefs.h
	Copyright T. Youngs 2007

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

#ifndef H_PREFSWINDOW_H
#define H_PREFSWINDOW_H

#include "gui/gui.h"
#include "gui-qt/ui_prefs.h"

// Stack Pages
enum prefstack_page { PSP_NITEMS };

class AtenPrefs : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// List / Stack Functions
	*/
	private slots:
	void on_CategoryList_currentRowChanged(int row) { ui.CategoryStack->setCurrentIndex(row); }

	/*
	// Element Page
	*/
	private:
	void set_element_colour(int type, int component, int value);
	private slots:
	void on_ElementList_currentRowChanged(int row);
	void on_ElementARedSpin_valueChanged(int value) { set_element_colour(0, 0, value); }
	void on_ElementAGreenSpin_valueChanged(int value) { set_element_colour(0, 1, value); }
	void on_ElementABlueSpin_valueChanged(int value) { set_element_colour(0, 2, value); }
	void on_ElementAAlphaSpin_valueChanged(int value) { set_element_colour(0, 3, value); }
	void on_ElementDRedSpin_valueChanged(int value) { set_element_colour(1, 0, value); }
	void on_ElementDGreenSpin_valueChanged(int value) { set_element_colour(1, 1, value); }
	void on_ElementDBlueSpin_valueChanged(int value) { set_element_colour(1, 2, value); }
	void on_ElementDAlphaSpin_valueChanged(int value) { set_element_colour(1, 3, value); }

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
	// Lighting PAge
	*/
	private:
	void spotlight_changed(spotlight_component so, int i, double value);
	public slots:
	void on_SpotlightGroup_clicked(bool checked);
	void on_AmbientRedSpin_valueChanged(double value) { spotlight_changed(SL_AMBIENT, 0, value); }
	void on_AmbientGreenSpin_valueChanged(double value) { spotlight_changed(SL_AMBIENT, 1, value); }
	void on_AmbientBlueSpin_valueChanged(double value) { spotlight_changed(SL_AMBIENT, 2, value); }
	void on_DiffuseRedSpin_valueChanged(double value) { spotlight_changed(SL_DIFFUSE, 0, value); }
	void on_DiffuseGreenSpin_valueChanged(double value) { spotlight_changed(SL_DIFFUSE, 1, value); }
	void on_DiffuseBlueSpin_valueChanged(double value) { spotlight_changed(SL_DIFFUSE, 2, value); }
	void on_SpecularRedSpin_valueChanged(double value) { spotlight_changed(SL_SPECULAR, 0, value); }
	void on_SpecularGreenSpin_valueChanged(double value) { spotlight_changed(SL_SPECULAR, 1, value); }
	void on_SpecularBlueSpin_valueChanged(double value) { spotlight_changed(SL_SPECULAR, 2, value); }
	void on_LightPositionXSpin_valueChanged(double value) { spotlight_changed(SL_POSITION, 0, value); }
	void on_LightPositionYSpin_valueChanged(double value) { spotlight_changed(SL_POSITION, 1, value); }
	void on_LightPositionZSpin_valueChanged(double value) { spotlight_changed(SL_POSITION, 2, value); }

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
};

#endif
