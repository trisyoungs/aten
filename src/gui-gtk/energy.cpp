/*
	*** GTK energy window
	*** src/gui-gtk/energy.cpp

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

#include <gtk/gtk.h>
#include "model/model.h"
#include "base/master.h"
#include "base/prefs.h"
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include <stdio.h>
#include <time.h>
#include "classes/fourier.h"

// Variables
GtkButton *energy_button, *energy_pat_button;
GtkRadioButton *charge_radio[QS_NITEMS];

gint energywin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
        // 'Delete Event' handler, called when we try to close the window.
        cs_hide(gui.windows[GW_ENERGYSETUP]);
	gui.refresh();
	return TRUE;
}

void energywin_click_closebtn(GtkButton *widget, gpointer data)
{
        cs_hide(gui.windows[GW_ENERGYSETUP]);
	gui.refresh();
}

void energywin_cutoff_edited(GtkSpinButton *widget, gpointer data)
{
        // One of the cutoff boxes has been edited
        // First, get the entry and convert it to a floating point value
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"energywin_cutoff_edited");
        double value = gtk_spin_button_get_value(widget);
	if (widget == gui.spin[W_SPN_VDWCUT]) prefs.set_vdw_cutoff(value);
	else
	{
		prefs.set_elec_cutoff(value);
		prefs.valid_ewaldauto = FALSE;
	}
	dbg_end(DM_CALLS,"energywin_cutoff_edited");
}

void energywin_ewald_edited(GtkSpinButton *widget, gpointer data)
{
        // An Ewald variable has been modified
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"energywin_ewald_edited");
	if (widget == gui.spin[W_SPN_EWALDKMAX])
	{
		prefs.set_ewald_kvec(gtk_spin_button_get_value_as_int(widget),0,0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gui.EMradios[EM_EWALD]),TRUE);
		//TODO Different directions for kvec!
	}
	else if (widget == GTK_SPIN_BUTTON(gui.spin[W_SPN_EWALDALPHA]))
	{
		prefs.set_ewald_alpha(gtk_spin_button_get_value(widget));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gui.EMradios[EM_EWALD]),TRUE);
	}
	else if (widget == GTK_SPIN_BUTTON(gui.spin[W_SPN_EWALDPRE]))
	{
		prefs.set_ewald_precision(gtk_spin_button_get_value(widget));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gui.EMradios[EM_EWALDAUTO]),TRUE);
	}
	prefs.valid_ewaldauto = FALSE;
	dbg_end(DM_CALLS,"energywin_ewald_edited");
}

void energywin_switched(GtkCheckButton *widget, gpointer data)
{
	if (!gui.exists()) return;
	// One of the expression components has been toggled
	if (widget == GTK_CHECK_BUTTON(gui.check[W_CHK_INTRAACTIVE]))
		prefs.set_calc_intra(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
	else if (widget == GTK_CHECK_BUTTON(gui.check[W_CHK_VDWACTIVE]))
		prefs.set_calc_vdw(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
	else if (widget == GTK_CHECK_BUTTON(gui.check[W_CHK_ELECACTIVE]))
		prefs.set_calc_elec(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void energywin_elec_changed(GtkRadioButton *widget, gpointer data)
{
	// The type of electrostatic calculation has been modified
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"energywin_elec_changed");
	int i;
	for (i=0; i<EM_NITEMS; i++) if (widget == GTK_RADIO_BUTTON(gui.EMradios[i])) break;
	prefs.set_electrostatics((elec_type) i);
	dbg_end(DM_CALLS,"energywin_elec_changed");
}

void energywin_charge_changed(GtkRadioButton *widget, gpointer data)
{
	// The source of charges has been modified
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"energywin_charge_changed");
	int i;
	for (i=0; i<QS_NITEMS; i++) if (widget == GTK_RADIO_BUTTON(charge_radio[i])) prefs.set_chargesource( (charge_source) i);
	dbg_end(DM_CALLS,"energywin_charge_changed");
}

void energywin_click_current_energy(GtkButton *widget, gpointer data)
{
        // Calculate the energy of the current model, with a quickconfig we make from it.
	dbg_begin(DM_CALLS,"energywin_click_current_energy");
	double energy;
	model *m = master.get_currentmodel();
	// Check pattern definition
	if (!m->autocreate_patterns())
	{
		dbg_end(DM_CALLS,"energywin_click_current_energy");
		return;
	}
	// Create the configuration node for the energy routines to work on
	if (m->create_expression())	// Check / delete / recreate energy expression
	{
		m->assign_charges(prefs.get_chargesource());
		// Calculate the energy of the system
		energy = m->total_energy(m);
		m->energy.print();
	}
	dbg_end(DM_CALLS,"energywin_click_current_energy");
}

void energywin_click_interaction_energy(GtkButton *widget, gpointer data)
{
        // Calculate the interaction energy of the current model, with a quickconfig we make from it.
	dbg_begin(DM_CALLS,"energywin_click_interaction_energy");
	double energy;
	model *m = master.get_currentmodel();
	// Check pattern definition
	if (!m->autocreate_patterns())
	{
		dbg_end(DM_CALLS,"energywin_click_interaction_energy");
		return;
	}
	// Do calculation
	energywin_click_current_energy(NULL,NULL);
	m->energy.print_intermatrix(m);
	dbg_end(DM_CALLS,"energywin_click_interaction_energy");
}

void energywin_click_current_forces(GtkButton *widget, gpointer data)
{
	// Calculate the current forces in the model
	dbg_begin(DM_CALLS,"energywin_click_current_forces");
	model *m = master.get_currentmodel();
	// Check pattern definition
	if (!m->autocreate_patterns())
	{
		dbg_end(DM_CALLS,"energywin_click_current_forces");
		return;
	}
	// Create the configuration node for the energy routines to work on

	if (m->create_expression())	// Check / delete / recreate energy expression
	{
		m->assign_charges(prefs.get_chargesource());
		// Finally, calculate the energy of the system
		m->calculate_forces(m);
		m->print_forces();
		// TEST
		time_t seconds;
		seconds = time(NULL);
		printf("Seconds = %ld\n",seconds);
		for (int n=0; n<0; n++)
		{
			fourier.calculate(m);
			m->calculate_forces(m);
			if (n%10 == 0) printf("Steps done : %i\n",n);
		}
		seconds = time(NULL);
		printf("Seconds = %ld\n",seconds);
		// END TEST
	}
	dbg_end(DM_CALLS,"energywin_click_current_forces");
}

void energywin_click_auto(GtkButton *widget, gpointer data)
{
	// Automatically set the VDW and electrostatic cutoffs based on the size of the cell
	dbg_begin(DM_CALLS,"energywin_click_auto");
	unitcell *cell = &master.get_currentmodel()->cell;
	mat3<double> tmat = cell->get_axes();
	switch (cell->get_type())
	{
		case (CT_NONE):
			// TODO Cutoff calculated from largest 'dimension' of system
			break;
		case (CT_CUBIC):
			// Set cutoffs to the integer part of half the cell length
			prefs.set_elec_cutoff(int(tmat.rows[0].x / 2.0));
			prefs.set_elec_cutoff(int(tmat.rows[0].x / 2.0));
			break;
	}
	dbg_end(DM_CALLS,"energywin_click_auto");
}

void gui_gtk::energywin_create()
{
	// Create the window for setting up (and testing) energy calculation.
	dbg_begin(DM_CALLS,"gui_gtk::energywin_create");
	GtkBox *box, *leftbox, *rightbox, *mainbox;
	GtkFrame *frame;
	GtkTable *table;
	GtkRadioButton *radio;
	GtkObject *adj;
	GtkButton *b;
	GtkLabel *l;
	windows[GW_ENERGYSETUP] = cs_subwindow("Energy Setup",FALSE,energywin_close);

	// A main vbox will hold two hboxes: top = option panes, bottom = action buttonbox
	mainbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(windows[GW_ENERGYSETUP]));
	box = cs_box(CS_HORIZONTAL,FALSE,2,mainbox,CS_START,TRUE,TRUE,0);
	leftbox = cs_box(CS_VERTICAL,FALSE,2,box,CS_START,TRUE,TRUE,0);
	rightbox = cs_box(CS_VERTICAL,FALSE,2,box,CS_END,TRUE,TRUE,0);
	
	// Electrostatic method frame
	frame = cs_frame("Electrostatics",rightbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_VERTICAL,FALSE,2,frame);
	//prefs.EMradios[EM_NONE] = cs_radio(NULL,"None",box,CS_START,FALSE,FALSE,0,energywin_elec_changed);
	EMradios[EM_COULOMB] = cs_radio(NULL,"Coulombic",box,CS_START,FALSE,FALSE,0,energywin_elec_changed);
	// -- Ewald + extra widgets
	EMradios[EM_EWALD] = cs_radio(EMradios[EM_COULOMB],"Ewald",box,CS_START,FALSE,FALSE,0,energywin_elec_changed);
	table = cs_table(1,4,FALSE,box,CS_START,FALSE,FALSE,0);
	l = cs_label("kmax",table,0,1,0,1);
	adj = cs_adjustment(5.0,0.0,100.0,1.0,1.0);
	spin[W_SPN_EWALDKMAX] = cs_spin(adj,1.0,0,table,1,2,0,1,energywin_ewald_edited,NULL);
	l = cs_label("Alpha",table,2,3,0,1);
	adj = cs_adjustment(0.2,0.0,1.0,0.1,0.1);
	spin[W_SPN_EWALDALPHA] = cs_spin(adj,0.1,5,table,3,4,0,1,energywin_ewald_edited,NULL);
	// -- EwaldAuto + extra widgets
	EMradios[EM_EWALDAUTO] = cs_radio(EMradios[EM_COULOMB],"Ewald (Automatic)",box,CS_START,FALSE,FALSE,0,energywin_elec_changed);
	table = cs_table(1,4,FALSE,box,CS_START,FALSE,FALSE,0);
	l = cs_label("Precision",table,0,1,0,1);
	adj = cs_adjustment(0.000001,0.0000000001,0.1,1.0,1.0);
	spin[W_SPN_EWALDPRE] = cs_spin(adj,1.0,10,table,1,4,0,1,energywin_ewald_edited,NULL);
	// Set the default model
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(EMradios[prefs.get_electrostatics()]),TRUE);

	// Charge Source frame
	frame = cs_frame("Charge Source",leftbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_VERTICAL,FALSE,2,frame);
	charge_radio[QS_MODEL] = cs_radio(NULL,"Model as is",box,CS_START,FALSE,FALSE,0,energywin_charge_changed);
	radio = charge_radio[QS_MODEL];
	charge_radio[QS_FF] = cs_radio(radio,"Forcefield",box,CS_START,FALSE,FALSE,0,energywin_charge_changed);
	charge_radio[QS_GASTEIGER] = cs_radio(radio,"Gasteiger",box,CS_START,FALSE,FALSE, 0,energywin_charge_changed);
	charge_radio[QS_QEQ] = cs_radio(radio,"QEq",box,CS_START,FALSE,FALSE,0,energywin_charge_changed);
	gtk_widget_set_sensitive(GTK_WIDGET(charge_radio[QS_GASTEIGER]),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(charge_radio[QS_QEQ]),FALSE);

	// Cutoff Frame
	frame = cs_frame("Cutoffs",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(1,5,FALSE,frame);
	l = cs_label("Van der Waals",table,0,1,0,1);
	adj = cs_adjustment(0.0,0.0,100.0,0.5,1.0);
	spin[W_SPN_VDWCUT] = cs_spin(adj,0.5,3,table,1,2,0,1,energywin_cutoff_edited,NULL);
	l = cs_label("Electrostatic",table,2,3,0,1);
	adj = cs_adjustment(0.0,0.0,100.0,0.5,1.0);
	spin[W_SPN_ELECCUT] = cs_spin(adj,0.5,3,table,3,4,0,1,energywin_cutoff_edited,NULL);
	b = cs_button("Auto",table,4,5,0,1,energywin_click_auto,NULL);

	// Switches frame
	frame = cs_frame("???",mainbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,TRUE,2,frame);
	check[W_CHK_VDWACTIVE] = cs_check("VDW",prefs.calc_vdw(),box,CS_START,FALSE,FALSE,0,energywin_switched,NULL);
	check[W_CHK_INTRAACTIVE] = cs_check("Intra",prefs.calc_intra(),box,CS_START,FALSE,FALSE,0,energywin_switched,NULL);
	check[W_CHK_ELECACTIVE] = cs_check("Electrostatics",prefs.calc_elec(),box,CS_START,FALSE,FALSE,0,energywin_switched,NULL);
	//gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prefs.check_vdwswitch),master.calc_vdw());

	// Actions frame
	frame = cs_frame("Calculate",mainbox,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,2,frame);
	energy_button = cs_button("Energy",box,CS_START,TRUE,TRUE,0,energywin_click_current_energy,NULL);
	energy_pat_button = cs_button("Interaction Energy",box,CS_START,TRUE,TRUE,0,energywin_click_interaction_energy,NULL);
	b = cs_button("Forces",box,CS_END,TRUE,TRUE,0,energywin_click_current_forces,NULL);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,energywin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::energywin_create");
}

