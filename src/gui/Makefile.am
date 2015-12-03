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
	-rm -f ui_* *.o ttreewidget.cpp tcolourframe.cpp tmenubutton.cpp tpopupwidget.cpp ttabwidget.cpp tintegerspindelegate.cpp tdoublespindelegate.cpp ttablewidget.cpp icons.cpp viewer.cpp moc_*.cpp

libtreegui_a_SOURCES = treegui.ui treegui_funcs.cpp

libgui_a_SOURCES = icons.qrc disorderoptions.ui disorderwizard.ui exportfilm.ui exportimage.ui ffeditor.ui loadmodel.ui prefs.ui progress.ui selectfilter.ui selectpattern.ui selectvariable.ui mainwindow.ui

#OLD FILES FOR UPDATING BEGIN HERE
libgui_a_SOURCES += glyphs.ui vibrations.ui viewbasis.ui vieweigenvector.ui zmatrix.ui
#OLD FILES END HERE

libgui_a_SOURCES += popupbuildaddh.ui popupbuildclear.ui popupbuildfragments.ui popupbuildgrow.ui popupbuildrebond.ui popupbuildtransmute.ui popupcellangles.ui popupcelllengths.ui popupcellmatrix.ui popupcellmiller.ui popupcellreplicate.ui popupcellscale.ui popupcellspacegroup.ui popupcolour.ui popupelementcommon.ui popupelementtable.ui popupfileaten.ui popupfileimage.ui popupfileopen.ui popupfilesave.ui popupforcefieldsassign.ui popupforcefieldsminimise.ui popupforcefieldstest.ui popupgridmatrix.ui popupgridorigin.ui popupgridset.ui popupgridshift.ui popupgridstyle.ui popupmeasureangle.ui popupmeasureclear.ui popupmeasuredistance.ui popupmeasuretorsion.ui popupporesdrill.ui popupporesscheme.ui popuptransformangle.ui popuptransformcentre.ui popuptransformconvert.ui popuptransformdistance.ui popuptransformflip.ui popuptransformrotate.ui popuptransformmultiply.ui popuptransformreposition.ui popuptransformshift.ui popuptransformtorsion.ui popuptransformtranslate.ui popupviewcolourscheme.ui popupviewreset.ui popupviewstyle.ui

libgui_a_SOURCES += disorderoptions_funcs.cpp disorderwizard_funcs.cpp exportfilm_funcs.cpp exportimage_funcs.cpp ffeditor_funcs.cpp layoutlist.cpp loadmodel_funcs.cpp mainwindow_atoms.cpp mainwindow_context.cpp mainwindow_film.cpp mainwindow_funcs.cpp mainwindow_image.cpp mainwindow_interaction.cpp mainwindow_messages.cpp mainwindow_models.cpp mainwindow_panel_build.cpp mainwindow_panel_calculate.cpp mainwindow_panel_cell.cpp mainwindow_panel_forcefields.cpp mainwindow_panel_grids.cpp mainwindow_panel_home.cpp mainwindow_panel_select.cpp mainwindow_panel_selection.cpp mainwindow_panel_tools.cpp mainwindow_panel_trajectory.cpp mainwindow_panel_transform.cpp mainwindow_quickcommand.cpp mainwindow_update.cpp prefs_funcs.cpp progress_funcs.cpp selectfilter_funcs.cpp selectpattern_funcs.cpp selectvariable_funcs.cpp settings.cpp

libgui_a_SOURCES += popupbuildaddh_funcs.cpp popupbuildclear_funcs.cpp popupbuildfragments_funcs.cpp popupbuildgrow_funcs.cpp popupbuildrebond_funcs.cpp popupbuildtransmute_funcs.cpp popupcellangles_funcs.cpp popupcelllengths_funcs.cpp popupcellmatrix_funcs.cpp popupcellmiller_funcs.cpp popupcellreplicate_funcs.cpp popupcellscale_funcs.cpp popupcellspacegroup_funcs.cpp popupcolour_funcs.cpp popupelementcommon_funcs.cpp popupelementtable_funcs.cpp popupfileaten_funcs.cpp popupfileimage_funcs.cpp popupfileopen_funcs.cpp popupfilesave_funcs.cpp popupforcefieldsassign_funcs.cpp popupforcefieldsminimise_funcs.cpp popupforcefieldstest_funcs.cpp popupgridmatrix_funcs.cpp popupgridorigin_funcs.cpp popupgridset_funcs.cpp popupgridshift_funcs.cpp popupgridstyle_funcs.cpp popupmeasureangle_funcs.cpp popupmeasureclear_funcs.cpp popupmeasuredistance_funcs.cpp popupmeasuretorsion_funcs.cpp popupporesdrill_funcs.cpp popupporesscheme_funcs.cpp popuptransformangle_funcs.cpp popuptransformcentre_funcs.cpp popuptransformconvert_funcs.cpp popuptransformdistance_funcs.cpp popuptransformflip_funcs.cpp popuptransformmultiply_funcs.cpp popuptransformreposition_funcs.cpp popuptransformrotate_funcs.cpp popuptransformshift_funcs.cpp popuptransformtorsion_funcs.cpp popuptransformtranslate_funcs.cpp popupviewcolourscheme_funcs.cpp popupviewreset_funcs.cpp popupviewstyle_funcs.cpp

libgui_a_SOURCES += tcolourframe.hui tcolourwheel.hui tdoublespindelegate.hui texponentialspin.hui tintegerspindelegate.hui tmenubutton.hui tpopupwidget.hui ttablewidget.hui ttabwidget.hui ttreewidget.hui viewer.hui
libgui_a_SOURCES += viewer_extras.cpp viewer_funcs.cpp viewer_key.cpp viewer_model.cpp viewer_mouse.cpp viewer_offscreen.cpp viewer_scene.cpp viewer_useractions.cpp tdoublespindelegate_funcs.cpp tcolourframe_funcs.cpp tcolourwheel_funcs.cpp texponentialspin_funcs.cpp textratreewidgetitem_funcs.cpp tintegerspindelegate_funcs.cpp tmenubutton_funcs.cpp tmenubuttongroup.cpp tpopupwidget_funcs.cpp ttabwidget_funcs.cpp

#OLD FILES FOR UPDATING BEGIN HERE
libgui_a_SOURCES += glyphs_funcs.cpp vibrations_funcs.cpp viewbasis_funcs.cpp vieweigenvector_funcs.cpp zmatrix_funcs.cpp
#OLD FILES END HERE

libgui_a_SOURCES += useractions.cpp

noinst_HEADERS = disorderoptions.h disorderwizard.h exportfilm.h exportimage.h ffeditor.h glyphs.h layoutlist.h loadmodel.h mainwindow.h popupbuildaddh.h popupbuildclear.h popupbuildfragments.h popupbuildgrow.h popupbuildrebond.h popupbuildtransmute.h popupcellangles.h popupcelllengths.h popupcellmatrix.h popupcellmiller.h popupcellreplicate.h popupcellscale.h popupcellspacegroup.h popupcolour.h popupelementcommon.h popupelementtable.h popupfileaten.h popupfileimage.h popupfileopen.h popupfilesave.h popupforcefieldsassign.h popupforcefieldsminimise.h popupforcefieldstest.h popupgridmatrix.h popupgridorigin.h popupgridset.h popupgridshift.h popupgridstyle.h popupmeasureangle.h popupmeasureclear.h popupmeasuredistance.h popupmeasuretorsion.h popupporesdrill.h popupporesscheme.h popuptransformangle.h popuptransformcentre.h popuptransformconvert.h popuptransformdistance.h popuptransformflip.h popuptransformmultiply.h popuptransformreposition.h popuptransformrotate.h popuptransformshift.h popuptransformtorsion.h popuptransformtranslate.h popupviewcolourscheme.h popupviewreset.h popupviewstyle.h prefs.h progress.h selectfilter.h selectpattern.h selectvariable.h textratreewidgetitem.h tmenubuttongroup.h treegui.h useractions.h vibrations.h viewbasis.h vieweigenvector.h zmatrix.h

INCLUDES = -I$(top_srcdir)/src -I../ -I./ @ATEN_CFLAGS@
