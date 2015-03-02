lib_LTLIBRARIES = libgui.la

SUFFIXES: .ui .uih .qrc

.ui.lo:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o $*.cpp $(top_srcdir)/src/gui/$*.h
	libtool --mode=compile ${CXX} -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(QTGUI_CFLAGS) -c $*.cpp
	rm $*.cpp

.uih.lo:
	$(QTMOC) -o $*.cpp -f$< $<
	libtool --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(QTGUI_CFLAGS) -c $*.cpp
	rm $*.cpp

.qrc.lo:
	$(QTRCC) -o $*.cpp -name $* $<
	libtool --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(QTGUI_CFLAGS) -c $*.cpp
	rm $*.cpp

clean-local:
	-rm -f ui_* *.o tprompt.cpp ttreewidget.cpp tcolourframe.cpp tradiogroup.cpp tprocess.cpp tmenubutton.cpp tcombobox.cpp icons.cpp viewer.cpp moc_*.cpp

libgui_la_SOURCES = icons.qrc ffeditor.ui loadmodel.ui prefs.ui progress.ui selectelement.ui selectfilter.ui selectpattern.ui selectvariable.ui mainwindow.ui

libgui_la_SOURCES += about.ui atomlist.ui build.ui celldefinition.ui celltransform.ui command.ui disorderoptions.ui disorderwizard.ui forcefields.ui fragments.ui geometry.ui glyphs.ui grids.ui messages.ui modellist.ui pores.ui position.ui saveimage.ui scriptmovie.ui select.ui trajectory.ui transform.ui treegui.ui vibrations.ui viewbasis.ui vieweigenvector.ui zmatrix.ui

libgui_la_SOURCES += about_funcs.cpp atomlist_funcs.cpp build_funcs.cpp celldefinition_funcs.cpp celltransform_funcs.cpp command_funcs.cpp disorderoptions_funcs.cpp disorderwizard_funcs.cpp forcefields_funcs.cpp fragments_funcs.cpp geometry_funcs.cpp glyphs_funcs.cpp grids_funcs.cpp messages_funcs.cpp modellist_funcs.cpp pores_funcs.cpp position_funcs.cpp saveimage_funcs.cpp scriptmovie_funcs.cpp select_funcs.cpp trajectory_funcs.cpp transform_funcs.cpp treegui_funcs.cpp vibrations_funcs.cpp viewbasis_funcs.cpp vieweigenvector_funcs.cpp zmatrix_funcs.cpp

libgui_la_SOURCES += ffeditor_funcs.cpp layoutlist.cpp loadmodel_funcs.cpp mainwindow_context.cpp mainwindow_funcs.cpp mainwindow_init.cpp prefs_funcs.cpp progress_funcs.cpp selectelement_funcs.cpp selectfilter_funcs.cpp selectpattern_funcs.cpp selectvariable_funcs.cpp settings.cpp
libgui_la_SOURCES += mainmenuactions.cpp mouseactions.cpp toolbaractions.cpp

libgui_la_SOURCES += tcolourframe.uih ttablewidget.uih tdoublespindelegate.uih tintegerspindelegate.uih ttreewidget.uih tmenubutton.uih tprocess.uih tprompt.uih tradiogroup.uih viewer.uih
libgui_la_SOURCES += viewer_funcs.cpp viewer_glyph.cpp viewer_key.cpp viewer_model.cpp viewer_mouse.cpp viewer_scene.cpp viewer_user.cpp viewer_useractions.cpp tdoublespindelegate_funcs.cpp tcolourframe_funcs.cpp tcombobox_funcs.cpp textratreewidgetitem_funcs.cpp tintegerspindelegate_funcs.cpp tlistwidgetitem_funcs.cpp tmenubutton_funcs.cpp ttablewidgetitem_funcs.cpp ttreewidgetitem_funcs.cpp tprocess_funcs.cpp tprompt_funcs.cpp

libgui_la_SOURCES += useractions.cpp

noinst_HEADERS = about.h atomlist.h build.h celldefinition.h celltransform.h command.h disorderoptions.h disorderwizard.h ffeditor.h forcefields.h fragments.h geometry.h glyphs.h grids.h layoutlist.h loadmodel.h mainwindow.h messages.h modellist.h pores.h position.h prefs.h progress.h saveimage.h scriptmovie.h select.h selectelement.h selectfilter.h selectpattern.h selectvariable.h tcombobox.h textratreewidgetitem.h tlistwidgetitem.h trajectory.h transform.h treegui.h ttablewidgetitem.h ttreewidgetitem.h useractions.h vibrations.h viewbasis.h vieweigenvector.h zmatrix.h

INCLUDES = -I$(top_srcdir)/src -I../ -I./ @GUI_CFLAGS@
