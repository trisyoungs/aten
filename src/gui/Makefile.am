noinst_LTLIBRARIES = libgui.la libtreegui.la

SUFFIXES: .ui .hui .qrc

.ui.lo:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $(top_srcdir)/src/gui/$*.h
	${LIBTOOL} --tag=CXX --mode=compile ${CXX} -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c moc_$*.cpp -o $@
	rm moc_$*.cpp

.hui.lo:
	$(QTMOC) -o $*.cpp -f$< $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

.qrc.lo:
	$(QTRCC) -o $*.cpp -name $* $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c -o $@ $*.cpp
	rm $*.cpp

clean-local:
	-rm -f ui_* *.o icons.cpp tdoublespindelegate.cpp tintegerspindelegate.cpp tmenubutton.cpp tpopupwidget.cpp trecentfiles.cpp ttablewidget.cpp ttabwidget.cpp ttreewidget.cpp viewer.cpp moc_*.cpp

libtreegui_la_SOURCES = treegui.ui treegui_funcs.cpp

libgui_la_SOURCES = icons.qrc disorderoptions.ui disorderwizard.ui exportfilm.ui exportimage.ui ffeditor.ui fileselectorwidget.ui prefs.ui progress.ui selectpattern.ui selectvariable.ui zmatrix.ui mainwindow.ui

libgui_la_SOURCES += opengrid.ui openmodel.ui opentrajectory.ui savemodel.ui

libgui_la_SOURCES += popupbuildaddatom.ui popupbuildaddh.ui popupbuildclear.ui popupbuildfragments.ui popupbuildgrow.ui popupbuildrebond.ui popupbuildtransmute.ui popupcellangles.ui popupcelllengths.ui popupcellmatrix.ui popupcellmiller.ui popupcellreplicate.ui popupcellscale.ui popupcellspacegroup.ui popupcolour.ui popupelementtable.ui popupfileaten.ui popupfileimage.ui popupfileopen.ui popupfilesave.ui popupfilesession.ui popupforcefieldsassign.ui popupforcefieldsminimise.ui popupforcefieldsopen.ui popupforcefieldstest.ui popupgridmatrix.ui popupgridorigin.ui popupgridset.ui popupgridshift.ui popupgridstyle.ui popupgridsopen.ui popupmeasureangle.ui popupmeasureclear.ui popupmeasuredistance.ui popupmeasuretorsion.ui popupporesdrill.ui popupporesscheme.ui popupscriptsopen.ui popuptransformangle.ui popuptransformcentre.ui popuptransformconvert.ui popuptransformdistance.ui popuptransformflip.ui popuptransformrotate.ui popuptransformmultiply.ui popuptransformreposition.ui popuptransformshift.ui popuptransformtorsion.ui popuptransformtranslate.ui popupviewcolourscheme.ui popupviewreset.ui popupviewstyle.ui

#OLD FILES FOR UPDATING BEGIN HERE
libgui_la_SOURCES += glyphs.ui vibrations.ui viewbasis.ui vieweigenvector.ui
#OLD FILES END HERE

libgui_la_SOURCES += disorderoptions_funcs.cpp disorderwizard_funcs.cpp exportfilm_funcs.cpp exportimage_funcs.cpp ffeditor_funcs.cpp fileselectorwidget_funcs.cpp layoutlist.cpp mainwindow_atoms.cpp mainwindow_context.cpp mainwindow_film.cpp mainwindow_funcs.cpp mainwindow_image.cpp mainwindow_interaction.cpp mainwindow_messages.cpp mainwindow_models.cpp mainwindow_panel_build.cpp mainwindow_panel_calculate.cpp mainwindow_panel_cell.cpp mainwindow_panel_forcefields.cpp mainwindow_panel_grids.cpp mainwindow_panel_home.cpp mainwindow_panel_select.cpp mainwindow_panel_selection.cpp mainwindow_panel_tools.cpp mainwindow_panel_trajectory.cpp mainwindow_panel_transform.cpp mainwindow_quickcommand.cpp mainwindow_session.cpp mainwindow_update.cpp prefs_funcs.cpp progress_funcs.cpp selectpattern_funcs.cpp selectvariable_funcs.cpp settings.cpp zmatrix_funcs.cpp

libgui_la_SOURCES += popupbuildaddatom_funcs.cpp popupbuildaddh_funcs.cpp popupbuildclear_funcs.cpp popupbuildfragments_funcs.cpp popupbuildgrow_funcs.cpp popupbuildrebond_funcs.cpp popupbuildtransmute_funcs.cpp popupcellangles_funcs.cpp popupcelllengths_funcs.cpp popupcellmatrix_funcs.cpp popupcellmiller_funcs.cpp popupcellreplicate_funcs.cpp popupcellscale_funcs.cpp popupcellspacegroup_funcs.cpp popupcolour_funcs.cpp popupelementtable_funcs.cpp popupfileaten_funcs.cpp popupfileimage_funcs.cpp popupfileopen_funcs.cpp popupfilesave_funcs.cpp popupfilesession_funcs.cpp popupforcefieldsassign_funcs.cpp popupforcefieldsminimise_funcs.cpp popupforcefieldsopen_funcs.cpp popupforcefieldstest_funcs.cpp popupgridmatrix_funcs.cpp popupgridorigin_funcs.cpp popupgridset_funcs.cpp popupgridshift_funcs.cpp popupgridstyle_funcs.cpp popupgridsopen_funcs.cpp popupmeasureangle_funcs.cpp popupmeasureclear_funcs.cpp popupmeasuredistance_funcs.cpp popupmeasuretorsion_funcs.cpp popupporesdrill_funcs.cpp popupporesscheme_funcs.cpp popupscriptsopen_funcs.cpp popuptransformangle_funcs.cpp popuptransformcentre_funcs.cpp popuptransformconvert_funcs.cpp popuptransformdistance_funcs.cpp popuptransformflip_funcs.cpp popuptransformmultiply_funcs.cpp popuptransformreposition_funcs.cpp popuptransformrotate_funcs.cpp popuptransformshift_funcs.cpp popuptransformtorsion_funcs.cpp popuptransformtranslate_funcs.cpp popupviewcolourscheme_funcs.cpp popupviewreset_funcs.cpp popupviewstyle_funcs.cpp

libgui_la_SOURCES += opengrid_funcs.cpp openmodel_funcs.cpp opentrajectory_funcs.cpp savemodel_funcs.cpp

libgui_la_SOURCES += tcolourwheel.hui tdoublespindelegate.hui texponentialspin.hui tintegerspindelegate.hui tmenubutton.hui tpopupwidget.hui trecentfiles.hui ttablewidget.hui ttabwidget.hui ttreewidget.hui viewer.hui

libgui_la_SOURCES += viewer_extras.cpp viewer_funcs.cpp viewer_key.cpp viewer_model.cpp viewer_mouse.cpp viewer_offscreen.cpp viewer_scene.cpp viewer_useractions.cpp tdoublespindelegate_funcs.cpp tcolourwheel_funcs.cpp texponentialspin_funcs.cpp textratreewidgetitem_funcs.cpp tintegerspindelegate_funcs.cpp tmenubutton_funcs.cpp tmenubuttongroup.cpp tpopupwidget_funcs.cpp trecentfiles_funcs.cpp ttabwidget_funcs.cpp

#OLD FILES FOR UPDATING BEGIN HERE
libgui_la_SOURCES += glyphs_funcs.cpp vibrations_funcs.cpp viewbasis_funcs.cpp vieweigenvector_funcs.cpp
#OLD FILES END HERE

libgui_la_SOURCES += useractions.cpp

noinst_HEADERS = disorderoptions.h disorderwizard.h exportfilm.h exportimage.h ffeditor.h fileselectorwidget.h glyphs.h layoutlist.h mainwindow.h opengrid.h openmodel.h opentrajectory.h prefs.h progress.h savemodel.h selectpattern.h selectvariable.h textratreewidgetitem.h tmenubuttongroup.h treegui.h useractions.h vibrations.h viewbasis.h vieweigenvector.h zmatrix.h
noinst_HEADERS += popupbuildaddatom.h popupbuildaddh.h popupbuildclear.h popupbuildfragments.h popupbuildgrow.h popupbuildrebond.h popupbuildtransmute.h popupcellangles.h popupcelllengths.h popupcellmatrix.h popupcellmiller.h popupcellreplicate.h popupcellscale.h popupcellspacegroup.h popupcolour.h popupelementtable.h popupfileaten.h popupfileimage.h popupfileopen.h popupfilesave.h popupfilesession.h popupforcefieldsassign.h popupforcefieldsminimise.h popupforcefieldsopen.h popupforcefieldstest.h popupgridmatrix.h popupgridorigin.h popupgridset.h popupgridshift.h popupgridstyle.h popupgridsopen.h popupmeasureangle.h popupmeasureclear.h popupmeasuredistance.h popupmeasuretorsion.h popupporesdrill.h popupporesscheme.h popupscriptsopen.h popuptransformangle.h popuptransformcentre.h popuptransformconvert.h popuptransformdistance.h popuptransformflip.h popuptransformmultiply.h popuptransformreposition.h popuptransformrotate.h popuptransformshift.h popuptransformtorsion.h popuptransformtranslate.h popupviewcolourscheme.h popupviewreset.h popupviewstyle.h


AM_CPPFLAGS = -I$(top_srcdir)/src -I../ -I./ @ATEN_INCLUDES@ @ATEN_CFLAGS@
