noinst_LIBRARIES = libgui.a

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

libgui_a_SOURCES = icons.qrc prefs.ui mainwindow.ui mainwindow_funcs.cpp mainwindow_init.cpp input.cpp analyse_funcs.cpp atomlist_funcs.cpp disorder_funcs.cpp forcefield_funcs.cpp transform_funcs.cpp edit_funcs.cpp cell_funcs.cpp position_funcs.cpp grid_funcs.cpp minimise_funcs.cpp contextmenu_funcs.cpp actions.cpp modelactions.cpp editactions.cpp viewactions.cpp trajectoryactions.cpp fileactions.cpp prefs_funcs.cpp gui.cpp tcanvas_funcs.cpp tcolourframe_funcs.cpp ttreewidgetitem_funcs.cpp canvas.cpp ttabbar.uih tcanvas.uih tcolourframe.uih ttreewidget.uih

noinst_HEADERS = canvas.h gui.h mainwindow.h prefs.h ttreewidgetitem.h

INCLUDES = -I../ @GUI_CFLAGS@
