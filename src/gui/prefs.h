/*
	*** Prefs Window
	*** src/gui/prefs.h
	Copyright T. Youngs 2007-2016

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

#include "gui/ui_prefs.h"
#include "base/prefs.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forwads declarations
class Element;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Program preferences window
class AtenPrefs : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	AtenPrefs(AtenWindow& parent);
	// Main form declaration
	Ui::PrefsDialog ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables
	 */
	private:
	// Whether the window is currently refreshing
	bool refreshing_;
	// Local copy of prefs/elements data
	Prefs prefsBackup_;


	/*
	 * Window Functions
	 */
	private slots:
	void on_PrefsOkButton_clicked(bool checked);
	void on_PrefsCancelButton_clicked(bool checked);
	void on_PrefsSaveAsDefaultButton_clicked(bool checked);


	/*
	 * General Page
	 */
	private slots:
	void on_DensityUnitCombo_currentIndexChanged(int index);
	void on_EnergyUnitCombo_currentIndexChanged(int index);
	void on_HAddDistanceSpin_valueChanged(double value);
	void on_MaxCuboidsSpin_valueChanged(int value);
	void on_MaxRingsSpin_valueChanged(int value);
	void on_MaxRingSizeSpin_valueChanged(int value);
	void on_MaxUndoLevelsSpin_valueChanged(int value);


	/*
	 * Element Page
	 */
	private slots:
	void on_ElementList_currentRowChanged(int row);
	void on_ElementRadiusSpin_valueChanged(double value);
	void elementColourChanged();


	/*
	 * Interaction page
	 */
	private slots:
	void on_LeftMouseCombo_currentIndexChanged(int ma);
	void on_MiddleMouseCombo_currentIndexChanged(int ma);
	void on_RightMouseCombo_currentIndexChanged(int ma);
	void on_WheelMouseCombo_currentIndexChanged(int ma);
	void on_ShiftButtonCombo_currentIndexChanged(int ka);
	void on_CtrlButtonCombo_currentIndexChanged(int ka);
	void on_AltButtonCombo_currentIndexChanged(int ka);
	void on_ViewLockCombo_currentIndexChanged(int vl);
	void on_ZoomThrottleSpin_valueChanged(double value);
	void on_MouseMoveFilterSpin_valueChanged(int value);


	/*
	 * View Page
	 */
	private:
	void updateAfterViewPrefs();
	void setRadiusChanged(Prefs::DrawStyle ds, double value, bool foratom);
	void spotlightPosChanged(int i, double value);
	void spotlightColourChanged(Prefs::ColourComponent);

	private slots:
	// Style page
	void on_StickRadiusSpin_valueChanged(double value);
	void on_TubeRadiusSpin_valueChanged(double value);
	void on_SphereRadiusSpin_valueChanged(double value);
	void on_ScaledRadiusSpin_valueChanged(double value);
	void on_StickBondRadiusSpin_valueChanged(double value);
	void on_TubeBondRadiusSpin_valueChanged(double value);
	void on_SphereBondRadiusSpin_valueChanged(double value);
	void on_ScaledBondRadiusSpin_valueChanged(double value);
	void on_SelectionScaleSpin_valueChanged(double value);
	void on_AngleLabelFormatEdit_textEdited(const QString& text);
	void on_DistanceLabelFormatEdit_textEdited(const QString& text);
	void on_ChargeLabelFormatEdit_textEdited(const QString& text);
	void on_LabelSizeSpin_valueChanged(double value);
	void on_RenderDashedAromaticsCheck_clicked(bool checked);
	void on_DrawHydrogenBondsCheck_clicked(bool checked);
	void on_HydrogenBondDotRadiusSpin_valueChanged(double value);
	void on_StickLineNormalWidthSpin_valueChanged(double value);
	void on_StickLineSelectedWidthSpin_valueChanged(double value);
	// Colours page
	void on_ColoursTable_cellDoubleClicked(int row, int column);
	// Rendering / Quality page
	void on_PrimitiveQualitySpin_valueChanged(int value);
	void on_PrimitiveQualitySlider_valueChanged(int value);
	void on_ImagePrimitivesGroup_clicked(bool checked);
	void on_ImagePrimitiveQualitySpin_valueChanged(int value);
	void on_ImagePrimitiveQualitySlider_valueChanged(int value);
	void on_FarClipSpin_valueChanged(double value);
	void on_FarDepthSpin_valueChanged(int value);
	void on_NearClipSpin_valueChanged(double value);
	void on_NearDepthSpin_valueChanged(int value);
	void on_LineAliasingCheck_stateChanged(int state);
	void on_PolygonAliasingCheck_stateChanged(int state);
	void on_MultiSamplingCheck_stateChanged(int state);
	void on_SpotlightGroup_clicked(bool checked);
	void on_SpotlightPositionXSpin_valueChanged(double value);
	void on_SpotlightPositionYSpin_valueChanged(double value);
	void on_SpotlightPositionZSpin_valueChanged(double value);
	void spotlightAmbientChanged();
	void spotlightDiffuseChanged();
	void spotlightSpecularChanged();
	void on_ShininessSpin_valueChanged(int value);
	// Fonts page
	void on_ViewerFontEdit_textEdited(const QString &text);
	void on_ViewerFontButton_clicked(bool checked);
	void on_MessagesSizeSpin_valueChanged(int value);


	/*
	 * Colourscales page
	 */
	private:
	void updateScalePointsList();

	private slots:
	void on_ScaleList_currentRowChanged(int id);
	void on_ScalePointsTable_cellChanged(int row, int col);
	void on_ScalePointsTable_cellDoubleClicked(int row, int column);
	void on_AddPointButton_clicked(bool checked);
	void on_RemovePointButton_clicked(bool checked);
	void on_ScaleList_itemClicked(QListWidgetItem* item);
	void on_ScaleList_itemDoubleClicked(QListWidgetItem* item);


	/*
	 * Energy / FF Page
	 */
	private:
	void updateParameterTable();

	private slots:
	void on_CalculateIntraCheck_stateChanged(int state);
	void on_CalculateVdwCheck_stateChanged(int state);
	void on_ElectrostaticMethodCombo_currentIndexChanged(int index);
	void on_VdwCutoffSpin_valueChanged(double d);
	void on_ElecCutoffSpin_valueChanged(double d);
	void on_EwaldPrecisionMantissaSpin_valueChanged(double d);
	void on_EwaldPrecisionExponentSpin_valueChanged(int i);
	void on_EwaldManualAlphaSpin_valueChanged(double d);
	void on_EwaldManualKXSpin_valueChanged(int i);
	void on_EwaldManualKYSpin_valueChanged(int i);
	void on_EwaldManualKZSpin_valueChanged(int i);
	void on_FunctionalFormList_currentRowChanged(int row);
	void ParameterRuleChanged(int index);
	void on_ParameterTable_itemChanged(QTableWidgetItem *w);


	/*
	 * External Programs
	 */
	private slots:
	void on_TemporaryDirButton_clicked(bool checked);
	void on_TemporaryDirEdit_textEdited(const QString &text);
	void on_MopacExecutableEdit_textEdited(const QString &text);
	void on_MopacExecutableButton_clicked(bool checked);
};

#endif
