if MAKEQTGUI

noinst_LIBRARIES = libguiqt.a

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

libguiqt_a_SOURCES = icons.qrc prefs.ui mainwindow.ui mainwindow_funcs.cpp analyse_funcs.cpp atomlist_funcs.cpp disorder_funcs.cpp forcefield_funcs.cpp transform_funcs.cpp edit_funcs.cpp cell_funcs.cpp position_funcs.cpp grid_funcs.cpp minimise_funcs.cpp contextmenu_funcs.cpp action_funcs.cpp prefs_funcs.cpp gui-qt.cpp tcanvas_funcs.cpp tcolourframe_funcs.cpp canvas-qt.cpp ttabbar.uih tcanvas.uih tcolourframe.uih ttreewidget.uih

noinst_HEADERS = canvas-qt.h gui-qt.h mainwindow.h prefs.h ttreewidgetatom.h

INCLUDES = -I../ @GUI_CFLAGS@

endif
