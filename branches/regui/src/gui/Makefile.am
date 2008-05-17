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


SUBWINDOW_UI = atomlist.ui build.ui celldefine.ui celltransform.ui disorder.ui forcefields.ui glyphs.ui grids.ui minimiser.ui position.ui transform.ui
SUBWINDOW_FUNCS = atomlist_funcs.cpp build_funcs.cpp celldefine_funcs.cpp celltransform_funcs.cpp disorder_funcs.cpp edit_funcs.cpp forcefields_funcs.cpp glyphs_funcs.cpp grids_funcs.cpp minimiser_funcs.cpp position_funcs.cpp transform_funcs.cpp

MAINWINDOW_UI = ffeditor.ui loadmodel.ui prefs.ui selectpattern.ui mainwindow.ui
MAINWINDOW_FUNCS = actions.cpp bondactions.cpp contextmenu_funcs.cpp drawactions.cpp editactions.cpp fileactions.cpp ffeditor_funcs.cpp gui.cpp loadmodel_funcs.cpp mainwindow_funcs.cpp mainwindow_init.cpp measureeactions.cpp modelactions.cpp prefs_funcs.cpp selectpattern_funcs.cpp trajectoryactions.cpp viewactions.cpp

WIDGET_UI = ttabbar.uih tcolourframe.uih ttreewidget.uih tcanvas.uih
WIDGET_FUNCS = tcanvas_funcs.cpp tcolourframe_funcs.cpp tcombobox_funcs.cpp tlistwidgetitem_funcs.cpp ttablewidgetitem_funcs.cpp ttreewidgetitem_funcs.cpp

CANVAS_FUNCS = canvas.cpp input.cpp

libgui_a_SOURCES = $(CANVAS_FUNCS) $(WIDGET_UI) $(SUBWINDOW_UI) $(MAINWINDOW_UI) $(WIDGET_FUNCS) gui.cpp $(MAINWINDOW_FUNCS) $(SUBWINDOW_FUNCS)


noinst_HEADERS = canvas.h ffeditor.h gui.h loadmodel.h mainwindow.h prefs.h selectpattern.h tcombobox.h tlistwidgetitem.h ttablewidgetitem.h ttreewidgetitem.h

INCLUDES = -I../ @GUI_CFLAGS@
