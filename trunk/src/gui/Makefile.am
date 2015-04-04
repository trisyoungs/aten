noinst_LIBRARIES = libgui.a libtreegui.a

SUFFIXES: .ui .uih .qrc

.ui.o:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $(top_srcdir)/src/gui/$*.h
	${CXX} -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(ATEN_CFLAGS) -c moc_$*.cpp -o $@
	rm moc_$*.cpp

.uih.o:
	$(QTMOC) -o $*.cpp -f$< $<
	$(CXX) 	-I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(ATEN_CFLAGS) -c $*.cpp -o $@
	rm $*.cpp

.qrc.o:
	$(QTRCC) -o $*.cpp -name $* $<
	$(CXX) -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(ATEN_CFLAGS) -c -o $@ $*.cpp
	rm $*.cpp

clean-local:
	-rm -f ui_* *.o tprompt.cpp ttreewidget.cpp tcolourframe.cpp tprocess.cpp tmenubutton.cpp tintegerspindelegate.cpp tdoublespindelegate.cpp ttablewidget.cpp icons.cpp viewer.cpp moc_*.cpp

libtreegui_a_SOURCES = treegui.ui treegui_funcs.cpp

libgui_a_SOURCES = icons.qrc ffeditor.ui loadmodel.ui prefs.ui progress.ui selectelement.ui selectfilter.ui selectpattern.ui selectvariable.ui mainwindow.ui

libgui_a_SOURCES += popupaddh.ui popupgrow.ui popuprebond.ui popuptransmute.ui

libgui_a_SOURCES += popupaddh_funcs.cpp popupgrow_funcs.cpp popuprebond_funcs.cpp popuptransmute_funcs.cpp

libgui_a_SOURCES += about.ui atomlist.ui celldefinition.ui celltransform.ui command.ui disorderoptions.ui disorderwizard.ui forcefields.ui fragments.ui geometry.ui glyphs.ui grids.ui modellist.ui pores.ui position.ui saveimage.ui scriptmovie.ui select.ui trajectory.ui transform.ui vibrations.ui viewbasis.ui vieweigenvector.ui zmatrix.ui

libgui_a_SOURCES += about_funcs.cpp atomlist_funcs.cpp celldefinition_funcs.cpp celltransform_funcs.cpp command_funcs.cpp disorderoptions_funcs.cpp disorderwizard_funcs.cpp forcefields_funcs.cpp fragments_funcs.cpp geometry_funcs.cpp glyphs_funcs.cpp grids_funcs.cpp modellist_funcs.cpp pores_funcs.cpp position_funcs.cpp saveimage_funcs.cpp scriptmovie_funcs.cpp select_funcs.cpp trajectory_funcs.cpp transform_funcs.cpp vibrations_funcs.cpp viewbasis_funcs.cpp vieweigenvector_funcs.cpp zmatrix_funcs.cpp

libgui_a_SOURCES += ffeditor_funcs.cpp layoutlist.cpp loadmodel_funcs.cpp mainwindow_build.cpp mainwindow_context.cpp mainwindow_funcs.cpp mainwindow_init.cpp prefs_funcs.cpp progress_funcs.cpp selectelement_funcs.cpp selectfilter_funcs.cpp selectpattern_funcs.cpp selectvariable_funcs.cpp settings.cpp
libgui_a_SOURCES += mainmenuactions.cpp mouseactions.cpp toolbaractions.cpp

libgui_a_SOURCES += tcolourframe.uih ttablewidget.uih tdoublespindelegate.uih tintegerspindelegate.uih ttreewidget.uih tmenubutton.uih tprocess.uih tprompt.uih viewer.uih
libgui_a_SOURCES += viewer_funcs.cpp viewer_key.cpp viewer_model.cpp viewer_mouse.cpp viewer_offscreen.cpp viewer_scene.cpp viewer_user.cpp viewer_useractions.cpp tdoublespindelegate_funcs.cpp tcolourframe_funcs.cpp textratreewidgetitem_funcs.cpp tintegerspindelegate_funcs.cpp tmenubutton_funcs.cpp ttablewidgetitem_funcs.cpp ttreewidgetitem_funcs.cpp tprocess_funcs.cpp tprompt_funcs.cpp

libgui_a_SOURCES += useractions.cpp

noinst_HEADERS = about.h atomlist.h celldefinition.h celltransform.h command.h disorderoptions.h disorderwizard.h ffeditor.h forcefields.h fragments.h geometry.h glyphs.h grids.h layoutlist.h loadmodel.h mainwindow.h modellist.h popupaddh.h popupgrow.h popuprebond.h popuptransmute.h pores.h position.h prefs.h progress.h saveimage.h scriptmovie.h select.h selectelement.h selectfilter.h selectpattern.h selectvariable.h textratreewidgetitem.h trajectory.h transform.h treegui.h ttablewidgetitem.h ttreewidgetitem.h useractions.h vibrations.h viewbasis.h vieweigenvector.h zmatrix.h

INCLUDES = -I$(top_srcdir)/src -I../ -I./ @ATEN_CFLAGS@
