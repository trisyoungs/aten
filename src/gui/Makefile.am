noinst_LIBRARIES = libgui.a libtreegui.a

SUFFIXES: .ui .hui .qrc

.ui.o:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $(top_srcdir)/src/gui/$*.h
	${CXX} -I$(top_srcdir)/src -I../ -I./ $(CXXFLAGS) $(ATEN_CFLAGS) -c moc_$*.cpp -o $@
	rm moc_$*.cpp

.hui.o:
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

libgui_a_SOURCES = icons.qrc ffeditor.ui loadmodel.ui prefs.ui progress.ui selectfilter.ui selectpattern.ui selectvariable.ui mainwindow.ui

libgui_a_SOURCES += popupbuildaddh.ui popupbuildgrow.ui popupbuildrebond.ui popupbuildtransmute.ui popupcellangles.ui popupcelllengths.ui popupcellmatrix.ui popupcellmiller.ui popupcellreplicate.ui popupcellscale.ui popupcellspacegroup.ui popupcolour.ui popupelementcommon.ui popupelementtable.ui popupgridmatrix.ui popupgridorigin.ui popupgridshift.ui popupgridstyle.ui popupmeasureangle.ui popupmeasureclear.ui popupmeasuredistance.ui popupmeasuretorsion.ui popuptransformangle.ui popuptransformcentre.ui popuptransformdistance.ui popuptransformflip.ui popuptransformreposition.ui popuptransformshift.ui popuptransformtorsion.ui popuptransformtranslate.ui popupviewcolourscheme.ui popupviewreset.ui popupviewstyle.ui

libgui_a_SOURCES += popupbuildaddh_funcs.cpp popupbuildgrow_funcs.cpp popupbuildrebond_funcs.cpp popupbuildtransmute_funcs.cpp popupcellangles_funcs.cpp popupcelllengths_funcs.cpp popupcellmatrix_funcs.cpp popupcellmiller_funcs.cpp popupcellreplicate_funcs.cpp popupcellscale_funcs.cpp popupcellspacegroup_funcs.cpp popupcolour_funcs.cpp popupelementcommon_funcs.cpp popupelementtable_funcs.cpp popupgridmatrix_funcs.cpp popupgridorigin_funcs.cpp popupgridshift_funcs.cpp popupgridstyle_funcs.cpp popupmeasureangle_funcs.cpp popupmeasureclear_funcs.cpp popupmeasuredistance_funcs.cpp popupmeasuretorsion_funcs.cpp popuptransformangle_funcs.cpp popuptransformcentre_funcs.cpp popuptransformdistance_funcs.cpp popuptransformflip_funcs.cpp popuptransformreposition_funcs.cpp popuptransformshift_funcs.cpp popuptransformtorsion_funcs.cpp popuptransformtranslate_funcs.cpp popupviewcolourscheme_funcs.cpp popupviewreset_funcs.cpp popupviewstyle_funcs.cpp

libgui_a_SOURCES += about.ui command.ui disorderoptions.ui disorderwizard.ui forcefields.ui fragments.ui glyphs.ui pores.ui saveimage.ui scriptmovie.ui transform.ui vibrations.ui viewbasis.ui vieweigenvector.ui zmatrix.ui

libgui_a_SOURCES += about_funcs.cpp command_funcs.cpp disorderoptions_funcs.cpp disorderwizard_funcs.cpp forcefields_funcs.cpp fragments_funcs.cpp glyphs_funcs.cpp pores_funcs.cpp saveimage_funcs.cpp scriptmovie_funcs.cpp transform_funcs.cpp vibrations_funcs.cpp viewbasis_funcs.cpp vieweigenvector_funcs.cpp zmatrix_funcs.cpp

libgui_a_SOURCES += ffeditor_funcs.cpp layoutlist.cpp loadmodel_funcs.cpp mainwindow_atoms.cpp mainwindow_context.cpp mainwindow_funcs.cpp mainwindow_image.cpp mainwindow_messages.cpp mainwindow_models.cpp mainwindow_panel_build.cpp mainwindow_panel_calculate.cpp mainwindow_panel_cell.cpp mainwindow_panel_grids.cpp mainwindow_panel_home.cpp mainwindow_panel_select.cpp mainwindow_panel_trajectory.cpp mainwindow_panel_transform.cpp mainwindow_panel_view.cpp mainwindow_update.cpp prefs_funcs.cpp progress_funcs.cpp selectfilter_funcs.cpp selectpattern_funcs.cpp selectvariable_funcs.cpp settings.cpp
libgui_a_SOURCES += mainmenuactions.cpp

libgui_a_SOURCES += tcolourframe.hui tcolourwheel.hui tdoublespindelegate.hui texponentialspin.hui tintegerspindelegate.hui tmenubutton.hui tprocess.hui tprompt.hui ttablewidget.hui ttreewidget.hui viewer.hui
libgui_a_SOURCES += viewer_funcs.cpp viewer_key.cpp viewer_model.cpp viewer_mouse.cpp viewer_offscreen.cpp viewer_scene.cpp viewer_user.cpp viewer_useractions.cpp tdoublespindelegate_funcs.cpp tcolourframe_funcs.cpp tcolourwheel_funcs.cpp texponentialspin_funcs.cpp textratreewidgetitem_funcs.cpp tintegerspindelegate_funcs.cpp tmenubutton_funcs.cpp tprocess_funcs.cpp tprompt_funcs.cpp

libgui_a_SOURCES += useractions.cpp

noinst_HEADERS = about.h command.h disorderoptions.h disorderwizard.h ffeditor.h forcefields.h fragments.h glyphs.h layoutlist.h loadmodel.h mainwindow.h popupbuildaddh.h popupbuildgrow.h popupbuildrebond.h popupbuildtransmute.h popupcellangles.h popupcelllengths.h popupcellmatrix.h popupcellmiller.h popupcellreplicate.h popupcellscale.h popupcellspacegroup.h popupcolour.h popupelementcommon.h popupelementtable.h popupgridmatrix.h popupgridorigin.h popupgridshift.h popupgridstyle.h popupmeasureangle.h popupmeasureclear.h popupmeasuredistance.h popupmeasuretorsion.h popuptransformangle.h popuptransformcentre.h popuptransformdistance.h popuptransformflip.h popuptransformreposition.h popuptransformshift.h popuptransformtorsion.h popuptransformtranslate.h popupviewcolourscheme.h popupviewreset.h popupviewstyle.h pores.h prefs.h progress.h saveimage.h scriptmovie.h selectfilter.h selectpattern.h selectvariable.h textratreewidgetitem.h transform.h treegui.h useractions.h vibrations.h viewbasis.h vieweigenvector.h zmatrix.h

INCLUDES = -I$(top_srcdir)/src -I../ -I./ @ATEN_CFLAGS@
