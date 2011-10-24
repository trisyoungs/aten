noinst_LIBRARIES = libgui.a

SUFFIXES: .ui .uih .qrc

.ui.o:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $(top_srcdir)/src/gui/$*.h
	$(CXX) -I$(top_srcdir)/src -I$(top_srcdir)/src/gui/ -I../ $(CXXFLAGS) $(QTGUI_CFLAGS) -c -o $@ moc_$*.cpp
	rm moc_$*.cpp

.uih.o:
	$(QTMOC) -o $*.cpp -f$< $<
	$(CXX) -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(QTGUI_CFLAGS) -c -o $@ $*.cpp
	rm $*.cpp

.qrc.o:
	$(QTRCC) -o $*.cpp -name $* $<
	$(CXX) -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(QTGUI_CFLAGS) -c -o $@ $*.cpp
	rm $*.cpp

clean:
	rm -f *.o ui_* tprompt.cpp ttreewidget.cpp tcanvas.cpp tcolourframe.cpp tradiogroup.cpp tprocess.cpp tmenubutton.cpp tcombobox.cpp icons.cpp

SUBWINDOW_UI = about.ui atomlist.ui build.ui celldefinition.ui celltransform.ui command.ui disorderoptions.ui disorderwizard.ui forcefields.ui fragments.ui geometry.ui glyphs.ui grids.ui md.ui messages.ui modellist.ui position.ui scriptmovie.ui select.ui toolbox.ui trajectory.ui transform.ui treegui.ui vibrations.ui viewbasis.ui vieweigenvector.ui zmatrix.ui
SUBWINDOW_FUNCS = about_funcs.cpp atomlist_funcs.cpp build_funcs.cpp celldefinition_funcs.cpp celltransform_funcs.cpp command_funcs.cpp disorderoptions_funcs.cpp disorderwizard_funcs.cpp forcefields_funcs.cpp fragments_funcs.cpp geometry_funcs.cpp glyphs_funcs.cpp grids_funcs.cpp md_funcs.cpp messages_funcs.cpp modellist_funcs.cpp position_funcs.cpp scriptmovie_funcs.cpp select_funcs.cpp toolbox_funcs.cpp trajectory_funcs.cpp transform_funcs.cpp treegui_funcs.cpp vibrations_funcs.cpp viewbasis_funcs.cpp vieweigenvector_funcs.cpp zmatrix_funcs.cpp

MAINWINDOW_UI = icons.qrc ffeditor.ui loadmodel.ui prefs.ui progress.ui selectelement.ui selectfilter.ui selectpattern.ui selectvariable.ui mainwindow.ui
MAINWINDOW_FUNCS = contextmenu_funcs.cpp ffeditor_funcs.cpp gui.cpp layoutlist.cpp loadmodel_funcs.cpp mainwindow_funcs.cpp mainwindow_init.cpp prefs_funcs.cpp progress_funcs.cpp select_funcs.cpp selectelement_funcs.cpp selectfilter_funcs.cpp selectpattern_funcs.cpp selectvariable_funcs.cpp settings.cpp
MAINWINDOW_ACTIONS = mainmenuactions.cpp mouseactions.cpp toolbaractions.cpp

WIDGET_UI = tcolourframe.uih ttreewidget.uih tcanvas.uih tmenubutton.uih tprocess.uih tprompt.uih tradiogroup.uih
WIDGET_FUNCS = tcanvas_funcs.cpp tcanvas_input.cpp tcanvas_render.cpp tcolourframe_funcs.cpp tcombobox_funcs.cpp tlistwidgetitem_funcs.cpp tmenubutton_funcs.cpp ttablewidgetitem_funcs.cpp ttreewidgetitem_funcs.cpp tprocess_funcs.cpp tprompt_funcs.cpp

CANVAS_FUNCS = useractions.cpp

libgui_a_SOURCES = $(WIDGET_UI) $(SUBWINDOW_UI) $(MAINWINDOW_UI) $(CANVAS_FUNCS) $(WIDGET_FUNCS) gui.cpp $(MAINWINDOW_FUNCS) $(MAINWINDOW_ACTIONS) $(SUBWINDOW_FUNCS)

noinst_HEADERS = about.h atomlist.h build.h celldefinition.h celltransform.h command.h disorderoptions.h disorderwizard.h ffeditor.h forcefields.h fragments.h geometry.h glyphs.h grids.h gui.h layoutlist.h loadmodel.h mainwindow.h md.h messages.h modellist.h position.h prefs.h progress.h scriptmovie.h select.h selectelement.h selectfilter.h selectpattern.h selectvariable.h tcombobox.h tlistwidgetitem.h toolbox.h trajectory.h transform.h treegui.h ttablewidgetitem.h ttreewidgetitem.h useractions.h vibrations.h viewbasis.h vieweigenvector.h zmatrix.h

INCLUDES = -I$(top_srcdir)/src -I../ -I./ @GUI_CFLAGS@
