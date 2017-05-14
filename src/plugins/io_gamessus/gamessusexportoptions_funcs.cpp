/*
	*** GAMESS-US Export Options Functions
	*** src/plugins/io_gamessus/gamessexportoptions_funcs.cpp
	Copyright T. Youngs 2007-2017

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

#include "plugins/io_gamessus/gamessexportoptions.h"

// Constructor
GAMESSUSExportOptionsDialog::GAMESSUSExportOptionsDialog(KVMap& pluginOptions) : QDialog(NULL), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);
}

/*
 * Widget Functions
 */

void GAMESSUSExportOptionsDialog::on_CancelButton_clicked(bool checked)
{
	// Don't modify the stored pluginOptions_, just reject() the dialog
	reject(); 
}

void GAMESSUSExportOptionsDialog::on_OKButton_clicked(bool checked)
{
	// Set options before we accept() the dialog.
	pluginOptions_.add("contrl_runtyp", ui.RUNTYPCombo->currentText());
	pluginOptions_.add("contrl_scftyp", ui.SCFTYPCombo->currentText());
	pluginOptions_.add("contrl_mult", ui.MULTSpin->value());
	pluginOptions_.add("contrl_charge", ui.CHARGESpin->value());
	pluginOptions_.add("contrl_exetyp", ui.EXETYPCombo->currentText());
	pluginOptions_.add("contrl_relwfn", ui.RELWFNCombo->currentText());
	pluginOptions_.add("contrl_maxit", ui.MAXSCFSpin->value());
	pluginOptions_.add("contrl_zmt", ui.ZMTCheck->isChecked());
	pluginOptions_.add("cart_symbols", ui.CartesianSymbolsCheck->isChecked());

	// -- DFT options
	pluginOptions_.add("dftgrid", ui.GridDFTRadio->isChecked() ? "GRID" : "GRIDFREE");
	pluginOptions_.add("contrl_dfttyp", ui.GridDFTRadio->isChecked() ? ui.GridDFTCombo->currentText() : ui.GridFreeDFTCombo->currentText());
	pluginOptions_.add("contrl_tddft", ui.TDDFTCombo->currentText());

	// -- Post-HF Methods
	pluginOptions_.add("contrl_mplevl", ui.MPCombo->currentText());
	pluginOptions_.add("contrl_cityp", ui.CICombo->currentText());
	pluginOptions_.add("contrl_cctyp", ui.CCCombo->currentText());

	// -- Other options
	pluginOptions_.add("contrl_pp", ui.ECPTYPCombo->currentText());
	pluginOptions_.add("contrl_isphere", ui.ISPHERECheck->isChecked());

	// -- Basis set specification ($BASIS)
	pluginOptions_.add("basis_gbasis", ui.BASISCombo->currentText());
	pluginOptions_.add("basis_ngauss", ui.NGAUSSSpin->value());
	pluginOptions_.add("basis_ndfunc", ui.NDFUNCSpin->value());
	pluginOptions_.add("basis_npfunc", ui.NPFUNCSpin->value());
	pluginOptions_.add("basis_nffunc", ui.NFFUNCSpin->value());
	pluginOptions_.add("basis_diffsp", ui.DIFFSPCheck->isChecked());
	pluginOptions_.add("basis_diffs", ui.DIFFSCheck->isChecked());

	// -- System options ($SYSTEM)
	pluginOptions_.add("system_mwords", ui.MWORDSSpin->value());
	pluginOptions_.add("system_memddi", ui.MEMDDISpin->value());
	pluginOptions_.add("system_timlim", ui.TIMLIMSpin->value());

	// -- Stationary point location options ($STATPT)
	pluginOptions_.add("statpt_method", ui.STATPTMethodCombo->currentText());
	pluginOptions_.add("statpt_opttol", ui.STATPTOPTTOLSpin->value());
	pluginOptions_.add("statpt_nstep", ui.STATPTNSTEPSpin->value());
	pluginOptions_.add("statpt_ifolow", ui.STATPTIFOLOWSpin->value());
	pluginOptions_.add("statpt_hess", ui.STATPTHessianCombo->currentText());
	pluginOptions_.add("statpt_ihrep", ui.STATPTIHREPSpin->value());
	pluginOptions_.add("statpt_ststep", ui.STATPTSTSTEPSpin->value());
	pluginOptions_.add("statpt_hssend", ui.STATPTHSSENDCheck->isChecked());
	pluginOptions_.add("statpt_stpt", ui.STATPTStationaryPointCheck->isChecked());
	pluginOptions_.add("statpt_ifreez", ui.STATPTIFREEZEdit->text());

	// -- Intrinsic reaction coordinate run ($IRC)
	pluginOptions_.add("irc_forwrd", ui.IRCFORWRDCheck->isChecked());
	pluginOptions_.add("irc_saddle", ui.IRCSADDLECheck->isChecked());
	pluginOptions_.add("irc_stride", ui.IRCSTRIDESpin->value());
	pluginOptions_.add("irc_npoint", ui.IRCNPOINTSpin->value());
	pluginOptions_.add("irc_pace", ui.IRCPACECombo->currentText());

	// -- Electron density calculation ($ELDENS)
	pluginOptions_.add("eldens_ieden", ui.ELDENSCalculateCheck->isChecked());
	pluginOptions_.add("eldens_morb", ui.ELDENSMORBSpin->value());
	pluginOptions_.add("eldens_where", ui.ELDENSWHERECombo->currentText());
	pluginOptions_.add("eldens_output", ui.ELDENSOUTPUTCombo->currentText());

	// -- Electrostatic potential calculation ($ELPOT)
	pluginOptions_.add("elpot_iepot", ui.ELPOTCalculateCheck->isChecked());
	pluginOptions_.add("elpot_where", ui.ELPOTWHERECombo->currentText());
	pluginOptions_.add("elpot_output", ui.ELPOTOUTPUTCombo->currentText());

	// -- Point selection ($PDC)
	pluginOptions_.add("pdc_ptsel", ui.PDCPTSELCombo->currentText());
	pluginOptions_.add("pdc_constr", ui.PDCCONSTRCombo->currentText());

	// -- Grid specification ($GRID), placed in 'opts' tabs
	pluginOptions_.add("grid_modgrd", ui.GRIDMODGRDCheck->isChecked());
	pluginOptions_.add("grid_size", ui.GRIDIncrementSpin->value());
	pluginOptions_.add("grid_originx", ui.GRIDORIGINXSpin->value());
	pluginOptions_.add("grid_originy", ui.GRIDORIGINYSpin->value());
	pluginOptions_.add("grid_originz", ui.GRIDORIGINZSpin->value());
	pluginOptions_.add("grid_xvecx", ui.GRIDXVECXSpin->value());
	pluginOptions_.add("grid_xvecy", ui.GRIDXVECYSpin->value());
	pluginOptions_.add("grid_xvecz", ui.GRIDXVECZSpin->value());
	pluginOptions_.add("grid_yvecx", ui.GRIDYVECXSpin->value());
	pluginOptions_.add("grid_yvecy", ui.GRIDYVECYSpin->value());
	pluginOptions_.add("grid_yvecz", ui.GRIDYVECZSpin->value());
	pluginOptions_.add("grid_zvecx", ui.GRIDZVECXSpin->value());
	pluginOptions_.add("grid_zvecy", ui.GRIDZVECYSpin->value());
	pluginOptions_.add("grid_zvecz", ui.GRIDZVECZSpin->value());

	// -- PCM options ($PCM), placed in 'opts' tab
	pluginOptions_.add("pcm_solvnt", ui.PCMSolventCombo->currentText());
	pluginOptions_.add("pcm_icav", ui.PCMICAVCheck->isChecked());
	pluginOptions_.add("pcm_rsolv", ui.PCMRSOLVSpin->value());
	pluginOptions_.add("pcm_eps", ui.PCMEPSSpin->value());
	pluginOptions_.add("pcm_epsinf", ui.PCMEPSINFSpin->value());
	pluginOptions_.add("pcm_tce", ui.PCMTCESpin->value());
	pluginOptions_.add("pcm_vmol", ui.PCMVMOLSpin->value());
	pluginOptions_.add("pcm_sten", ui.PCMSTENSpin->value());
	pluginOptions_.add("pcm_dsten", ui.PCMDSTENSpin->value());
	pluginOptions_.add("pcm_cmf", ui.PCMCMFSpin->value());

	accept();
}

/*
 * Show Function
 */

// Update and show dialog, setting controls from pluginOptions
int GAMESSUSExportOptionsDialog::updateAndExecute()
{
	// Set controls to reflect current pluginOptions_
	ui.RUNTYPCombo->setCurrentIndex(ui.RUNTYPCombo->findText(pluginOptions_.value("contrl_runtyp")));
	ui.SCFTYPCombo->setCurrentIndex(ui.SCFTYPCombo->findText(pluginOptions_.value("contrl_scftyp")));
	ui.MULTSpin->setValue(pluginOptions_.value("contrl_mult").toInt());
	ui.CHARGESpin->setValue(pluginOptions_.value("contrl_charge").toInt());
	ui.EXETYPCombo->setCurrentIndex(ui.EXETYPCombo->findText(pluginOptions_.value("contrl_exetyp")));
	ui.RELWFNCombo->setCurrentIndex(ui.RELWFNCombo->findText(pluginOptions_.value("contrl_relwfn")));
	ui.MAXSCFSpin->setValue(pluginOptions_.value("contrl_maxit").toInt());
	ui.ZMTCheck->setChecked(pluginOptions_.value("contrl_zmt").toInt());
	ui.CartesianSymbolsCheck->setChecked(pluginOptions_.value("cart_symbols").toInt());

	// -- DFT options
	if (pluginOptions_.value("dftgrid") == "GRID")
	{
		ui.GridDFTRadio->setChecked(true);
		ui.GridDFTCombo->setCurrentIndex(ui.GridDFTCombo->findText(pluginOptions_.value("contrl_dfttyp")));
	}
	else
	{
		ui.GridFreeDFTRadio->setChecked(true);
		ui.GridFreeDFTCombo->setCurrentIndex(ui.GridFreeDFTCombo->findText(pluginOptions_.value("contrl_dfttyp")));
	}
	ui.TDDFTCombo->setCurrentIndex(ui.TDDFTCombo->findText(pluginOptions_.value("contrl_tddft")));

	// -- Post-HF Methods
	ui.MPCombo->setCurrentIndex(ui.MPCombo->findText(pluginOptions_.value("contrl_mplevl")));
	ui.CICombo->setCurrentIndex(ui.CICombo->findText(pluginOptions_.value("contrl_cityp")));
	ui.CCCombo->setCurrentIndex(ui.CCCombo->findText(pluginOptions_.value("contrl_cctyp")));
	
	// -- Other options
	ui.ECPTYPCombo->setCurrentIndex(ui.ECPTYPCombo->findText(pluginOptions_.value("contrl_pp")));
	ui.ISPHERECheck->setChecked(pluginOptions_.value("contrl_isphere").toInt());

	// -- Basis set specification ($BASIS)
	ui.BASISCombo->setCurrentIndex(ui.BASISCombo->findText(pluginOptions_.value("basis_gbasis")));
	ui.NGAUSSSpin->setValue(pluginOptions_.value("basis_ngauss").toInt());
	ui.NDFUNCSpin->setValue(pluginOptions_.value("basis_ndfunc").toInt());
	ui.NPFUNCSpin->setValue(pluginOptions_.value("basis_npfunc").toInt());
	ui.NFFUNCSpin->setValue(pluginOptions_.value("basis_nffunc").toInt());
	ui.DIFFSPCheck->setChecked(pluginOptions_.value("basis_diffsp").toInt());
	ui.DIFFSCheck->setChecked(pluginOptions_.value("basis_diffs").toInt());

	// -- System options ($SYSTEM)
	ui.MWORDSSpin->setValue(pluginOptions_.value("system_mwords").toInt());
	ui.MEMDDISpin->setValue(pluginOptions_.value("system_memddi").toInt());
	ui.TIMLIMSpin->setValue(pluginOptions_.value("system_timlim").toInt());

	// -- Stationary point location options ($STATPT)
	ui.STATPTMethodCombo->setCurrentIndex(ui.STATPTMethodCombo->findText(pluginOptions_.value("statpt_method")));
	ui.STATPTOPTTOLSpin->setValue(pluginOptions_.value("statpt_opttol").toDouble());
	ui.STATPTNSTEPSpin->setValue(pluginOptions_.value("statpt_nstep").toInt());
	ui.STATPTIFOLOWSpin->setValue(pluginOptions_.value("statpt_ifolow").toInt());
	ui.STATPTHessianCombo->setCurrentIndex(ui.STATPTHessianCombo->findText(pluginOptions_.value("statpt_hess")));
	ui.STATPTIHREPSpin->setValue(pluginOptions_.value("statpt_ihrep").toInt());
	ui.STATPTSTSTEPSpin->setValue(pluginOptions_.value("statpt_ststep").toDouble());
	ui.STATPTHSSENDCheck->setChecked(pluginOptions_.value("statpt_hssend").toInt());
	ui.STATPTStationaryPointCheck->setChecked(pluginOptions_.value("statpt_stpt").toInt());
	ui.STATPTIFREEZEdit->setText(pluginOptions_.value("statpt_ifreez"));

	// -- Intrinsic reaction coordinate run ($IRC)
	ui.IRCFORWRDCheck->setChecked(pluginOptions_.value("irc_forwrd").toInt());
	ui.IRCSADDLECheck->setChecked(pluginOptions_.value("irc_saddle").toInt());
	ui.IRCSTRIDESpin->setValue(pluginOptions_.value("irc_stride").toDouble());
	ui.IRCNPOINTSpin->setValue(pluginOptions_.value("irc_npoint").toInt());
	ui.IRCPACECombo->setCurrentIndex(ui.IRCPACECombo->findText(pluginOptions_.value("irc_pace")));

	// -- Electron density calculation ($ELDENS)
	ui.ELDENSCalculateCheck->setChecked(pluginOptions_.value("eldens_ieden").toInt());
	ui.ELDENSMORBSpin->setValue(pluginOptions_.value("eldens_morb").toInt());
	ui.ELDENSWHERECombo->setCurrentIndex(ui.ELDENSWHERECombo->findText(pluginOptions_.value("eldens_where")));
	ui.ELDENSOUTPUTCombo->setCurrentIndex(ui.ELDENSOUTPUTCombo->findText(pluginOptions_.value("eldens_output")));

	// -- Electrostatic potential calculation ($ELPOT)
	ui.ELPOTCalculateCheck->setChecked(pluginOptions_.value("elpot_iepot").toInt());
	ui.ELPOTWHERECombo->setCurrentIndex(ui.ELPOTWHERECombo->findText(pluginOptions_.value("elpot_where")));
	ui.ELPOTOUTPUTCombo->setCurrentIndex(ui.ELPOTOUTPUTCombo->findText(pluginOptions_.value("elpot_output")));

	// -- Point selection ($PDC)
	ui.PDCPTSELCombo->setCurrentIndex(ui.PDCPTSELCombo->findText(pluginOptions_.value("pdc_ptsel")));
	ui.PDCCONSTRCombo->setCurrentIndex(ui.PDCCONSTRCombo->findText(pluginOptions_.value("pdc_constr")));

	// -- Grid specification ($GRID)
	ui.GRIDMODGRDCheck->setChecked(pluginOptions_.value("grid_modgrd").toInt());
	ui.GRIDIncrementSpin->setValue(pluginOptions_.value("grid_size").toDouble());
	ui.GRIDORIGINXSpin->setValue(pluginOptions_.value("grid_originx").toDouble());
	ui.GRIDORIGINYSpin->setValue(pluginOptions_.value("grid_originy").toDouble());
	ui.GRIDORIGINZSpin->setValue(pluginOptions_.value("grid_originz").toDouble());
	ui.GRIDXVECXSpin->setValue(pluginOptions_.value("grid_xvecx").toDouble());
	ui.GRIDXVECYSpin->setValue(pluginOptions_.value("grid_xvecy").toDouble());
	ui.GRIDXVECZSpin->setValue(pluginOptions_.value("grid_xvecz").toDouble());
	ui.GRIDYVECXSpin->setValue(pluginOptions_.value("grid_yvecx").toDouble());
	ui.GRIDYVECYSpin->setValue(pluginOptions_.value("grid_yvecy").toDouble());
	ui.GRIDYVECZSpin->setValue(pluginOptions_.value("grid_yvecz").toDouble());
	ui.GRIDZVECXSpin->setValue(pluginOptions_.value("grid_zvecx").toDouble());
	ui.GRIDZVECYSpin->setValue(pluginOptions_.value("grid_zvecy").toDouble());
	ui.GRIDZVECZSpin->setValue(pluginOptions_.value("grid_zvecz").toDouble());

	// -- PCM options ($PCM)
	ui.PCMSolventCombo->setCurrentIndex(ui.PCMSolventCombo->findText(pluginOptions_.value("pcm_solvnt")));
	ui.PCMICAVCheck->setChecked(pluginOptions_.value("pcm_icav").toInt());
	ui.PCMRSOLVSpin->setValue(pluginOptions_.value("pcm_rsolv").toDouble());
	ui.PCMEPSSpin->setValue(pluginOptions_.value("pcm_eps").toDouble());
	ui.PCMEPSINFSpin->setValue(pluginOptions_.value("pcm_epsinf").toDouble());
	ui.PCMTCESpin->setValue(pluginOptions_.value("pcm_tce").toDouble());
	ui.PCMVMOLSpin->setValue(pluginOptions_.value("pcm_vmol").toDouble());
	ui.PCMSTENSpin->setValue(pluginOptions_.value("pcm_sten").toDouble());
	ui.PCMDSTENSpin->setValue(pluginOptions_.value("pcm_dsten").toDouble());
	ui.PCMCMFSpin->setValue(pluginOptions_.value("pcm_cmf").toDouble());

	// Execute the dialog - option setting will be handled in the OK button slot
	return exec();
}
