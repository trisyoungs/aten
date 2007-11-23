/*
	*** GTK window functions
	*** src/gui-gtk/funcs.h
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

#ifndef H_GUIFUNCS_H
#define H_GUIFUNCS_H

#include <gtk/gtk.h>
#include <gdk/gdkpixbuf.h>
#include "gui-gtk/treeview.h"

// GTK Shorthand
#define CS_START 0
#define CS_END 1
#define CS_VERTICAL 0
#define CS_HORIZONTAL 1

// Subwindow
GtkWindow *cs_subwindow(const char*, bool, int (*)(GtkWidget*,GdkEvent*,gpointer));

// Packing / Show Functions
void cs_pack(gpointer, GtkBox*, int, bool, bool, int);
void cs_showall(gpointer);
void cs_hide(gpointer);
void cs_show(gpointer);

// Labels
GtkLabel *cs_label(const char*, GtkBox*, int, bool, bool, int);
GtkLabel *cs_label(const char*, GtkTable*, int, int, int, int);
void cs_set_label_pango(const char*, GtkLabel*);

// Standard Buttons
GtkButton *cs_button(const char*, GtkBox*, int, bool, bool, int, void (*)(GtkButton*,void*), const char*);
GtkButton *cs_button(const char*, GtkTable*, int, int, int, int, void (*)(GtkButton*,void*), const char*);
GtkButton *cs_pixbutton(GdkPixbuf*, GtkBox*, int, bool, bool, int, void (*)(GtkButton*,void*),const char*);
GtkButton *cs_pixbutton(GdkPixbuf*, GtkTable*, int, int, int, int, void (*)(GtkButton*,void*),const char*);

// Menus
GtkMenu *cs_menushell(const char*, GtkMenuBar*);
GtkMenu *cs_menusubshell(GtkMenuItem*);
GtkMenuItem *cs_menuitem(const char*, GtkMenu*, GtkAccelGroup*, guint, GdkModifierType, void (*)(GtkMenuItem*,void*), const char*);
GtkMenuItem *cs_radiomenuitem(const char*, GtkMenu*, GtkMenuItem*, void (*)(GtkMenuItem*,void*), const char*);
GtkMenuItem *cs_checkmenuitem(const char*, GtkMenu*, void (*)(GtkCheckMenuItem*,void*), const char*);
void cs_menuseparator(GtkMenu*);

// Option Widgets - Checkboxes and Comboboxes
GtkCheckButton *cs_check(const char*, bool, GtkBox*, int, bool, bool, int, void (*)(GtkCheckButton*,void*), const char*);
GtkCheckButton *cs_check(const char*, bool, GtkTable*, int, int, int, int, void (*)(GtkCheckButton*,void*), const char*);
GtkRadioButton *cs_radio(GtkRadioButton*,const char*,GtkBox*,int,bool,bool,int,void (*)(GtkRadioButton*,void*));
GtkRadioButton *cs_radio(GtkRadioButton*,const char*,GtkTable*,int,int, int, int,void (*)(GtkRadioButton*,void*));
GtkComboBox *cs_combo(const char**, int, int, GtkBox*, int, bool, bool, int, void (*)(GtkComboBox*,void*));
GtkComboBox *cs_combo(const char**, int, int, GtkTable*, int, int, int, int, void (*)(GtkComboBox*,void*));

// Toolbars
GtkToolbar *cs_toolbar(GtkToolbarStyle, int, GtkBox*, int, bool, bool, int);
GtkToolbar *cs_toolbar(GtkToolbarStyle, int, GtkContainer*);
GtkToolbar *cs_toolbar(GtkToolbarStyle, int, GtkTable*, int, int, int, int);
GtkToolbar *cs_toolbar(GtkToolbarStyle, int, GtkWidget*, int, bool, bool, int);
GtkToolbar *cs_toolbar(GtkToolbarStyle, int);
GtkToolItem *cs_toolbtn(GtkToolbar*,GtkTooltips*,const char*,GdkPixbuf*,void (*)(GtkButton*,void*),const char *);
GtkToolItem *cs_radiotoolbtn(GtkToolbar*,GtkToolItem*,GtkTooltips*,const char*,GdkPixbuf*,void (*)(GtkRadioToolButton*,void*));
GtkToolItem *cs_textradiotoolbtn(GtkToolbar*,GtkToolItem*,GtkTooltips*,const char*,const char*,void (*)(GtkRadioToolButton*,void*),const char*);
GtkMenu *cs_menutoolbtn(GtkToolbar*,GtkTooltips*,const char*,GdkPixbuf*,void (*)(GtkButton*,void*));
GtkLabel *cs_toollabel(GtkToolbar*, const char*);
void cs_toolseparator(GtkToolbar*);

// Boxes, Notebooks, Tables and Frames
GtkBox *cs_box(int, gboolean, int, GtkBox*, int, bool, bool, int);
GtkBox *cs_box(int, gboolean, int, GtkContainer*);
GtkBox *cs_box(int, gboolean, int, GtkFrame*);
GtkBox *cs_box(int, gboolean, int, GtkWindow*);
GtkBox *cs_box(int, gboolean, int, GtkBox*);
GtkTable *cs_table(int, int, bool, GtkBox*, int, bool, bool, int);
GtkTable *cs_table(int, int, bool, GtkContainer*);
GtkTable *cs_table(int r, int c, bool h, GtkFrame* w);
GtkTable *cs_table(int r, int c, bool h, GtkBox* w);
GtkFrame *cs_frame(const char*, GtkBox*, int, bool, bool, int);
GtkFrame *cs_frame(const char*, GtkTable*, int, int, int, int);
GtkNotebook *cs_notebook(GtkBox*, int, bool, bool, int);
GtkBox *cs_notebookpage(const char*, GtkNotebook*);

// Adjustment, Scale and Spin Buttons
GtkObject *cs_adjustment(double, double, double, double, double);
GtkScale *cs_scale(GtkObject*,GtkBox*,int,bool,bool,int,void (*)(GtkRange*,GtkScrollType,gdouble,void*),const char*);
GtkScale *cs_scale(GtkObject*,GtkTable*,int,int, int, int,void (*)(GtkRange*,GtkScrollType,gdouble,void*),const char*);
GtkSpinButton *cs_spin(GtkObject*,double, int, GtkBox*,int,bool,bool,int,void (*)(GtkSpinButton*,void*), const char*);
GtkSpinButton *cs_spin(GtkObject*,double, int, GtkContainer*,void (*)(GtkSpinButton*,void*), const char*);
GtkSpinButton *cs_spin(GtkObject*, double, int, GtkTable*,int, int, int, int,void (*)(GtkSpinButton*,void*), const char*);

// Treeviews
GtkScrolledWindow *cs_treeview(treeview_list*, GtkBox*, int,bool,bool,int,void (*)(GtkTreeSelection*,void*));
GtkCellRenderer *cs_column(treeview_list*, const char*, const char*, int);

// Misc
GtkEntry *cs_entry(GtkBox*, int, bool, bool, int, void (*)(GtkEntry*,void*));
GtkEntry *cs_entry(GtkTable*, int, int, int, int, void (*)(GtkEntry*,void*));
void cs_separator(int orientation, GtkBox*, int, bool, bool, int);
GtkFileFilter *cs_filefilter(const char*, const char*);

#endif
