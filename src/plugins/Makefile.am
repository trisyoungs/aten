SUBDIRS = interfaces
SUBDIRS += io_akf io_chemshell io_cif io_cube io_dlpoly io_dlputils io_epsr io_ff io_gamessus io_gromacs io_mdlmol io_mopac io_msi io_pdb io_rmcprofile io_sybylmol2 io_test io_vfield io_xyz
#SUBDIRS += io_csd io_espresso io_gaussian io_siesta
SUBDIRS += method_mopac71
SUBDIRS += tool_springs tool_test

noinst_LTLIBRARIES = libplugins.la

libplugins_la_SOURCES = pluginstore.cpp plugintypes.cpp

noinst_HEADERS = pluginstore.h plugintypes.h

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

if LOCALDEPLOY

.PHONY: deploy-plugins undeploy-plugins

deploy-plugins: $(SUBDIRS)
	@for dir in $^; do \
	  for laname in $$dir/*la; do \
	    for soname in `echo | $(EGREP) "^dlname=" $$laname | $(SED) -e "s|^dlname='\(.*\)'|\1|"`; do  \
	      if [ -e $(abs_builddir)/$$dir/.libs/$$soname ]; then \
	        echo "$$soname: deploying $$soname plugin from from $(abs_builddir)/$$dir/.libs to $(top_builddir)/data/plugins"; \
	        rm -f $(top_builddir)/data/plugins/$$soname; \
	        cp $(abs_builddir)/$$dir/.libs/$$soname ${top_builddir}/data/plugins/$$soname || true; \
	      fi \
	    done \
	  done \
	done 

undeploy-plugins:
	@for soname in `echo | $(EGREP) "^dlname=" $(lib_LTLIBRARIES) | $(SED) -e "s|^dlname='\(.*\)'|\1|"`; do  \
	  echo "$$soname: cleaning deployed plugins"; \
	  test -L $(top_builddir)/data/plugins/$$soname && rm -f $(top_builddir)/data/plugins/$$soname || true; \
	done 

all-local:: deploy-plugins

clean-local:: undeploy-plugins

endif
