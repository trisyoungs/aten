noinst_LIBRARIES = libui.a libgui.a libfuncs.a

SUFFIXES: .ui .uih .qrc

.ui.o:
	$(QTDIR)uic -o ui_$*.h $<
	$(QTDIR)moc -o moc_$*.cpp $*.h
	$(CXX) -I../ $(CXXFLAGS) $(QTGUI_CFLAGS) -c -o $@ moc_$*.cpp
	rm moc_$*.cpp

.uih.o:
	$(QTDIR)moc -o $*.cpp -f$< $<
	$(CXX) -I../ $(CXXFLAGS) $(QTGUI_CFLAGS) -c -o $@ $*.cpp
	rm $*.cpp

.qrc.o:
	$(QTDIR)rcc -o $*.cpp -name $* $<
	$(CXX) -I../ $(CXXFLAGS) $(QTGUI_CFLAGS) -c -o $@ $*.cpp
	rm $*.cpp


libui_a_SOURCES = atomlist.ui build.ui celldefine.ui celltransform.ui disorder.ui ffeditor.ui forcefields.ui glyphs.ui grids.ui loadmodel.ui minimiser.ui position.ui prefs.ui selectpattern.ui transform.ui mainwindow.ui

libfuncs_a_SOURCES = actions.cpp analyse_funcs.cpp atomlist_funcs.cpp build_funcs.cpp contextmenu_funcs.cpp disorder_funcs.cpp edit_funcs.cpp editactions.cpp ffeditor_funcs.cpp fileactions.cpp forcefields_funcs.cpp glyphs_funcs.cpp grids_funcs.cpp loadmodel_funcs.cpp mainwindow_funcs.cpp minimiser_funcs.cpp modelactions.cpp position_funcs.cpp prefs_funcs.cpp selectpattern_funcs.cpp tcanvas_funcs.cpp tcolourframe_funcs.cpp tcombobox_funcs.cpp tlistwidgetitem_funcs.cpp transform_funcs.cpp ttablewidgetitem_funcs.cpp ttreewidgetitem_funcs.cpp trajectoryactions.cpp viewactions.cpp

libgui_a_SOURCES = canvas.cpp gui.cpp input.cpp

noinst_HEADERS = canvas.h ffeditor.h gui.h loadmodel.h mainwindow.h prefs.h selectpattern.h tcombobox.h tlistwidgetitem.h ttablewidgetitem.h ttreewidgetitem.h 

INCLUDES = -I../ @GUI_CFLAGS@
