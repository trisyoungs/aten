SUBDIRS = io_xyz

noinst_LIBRARIES = libplugins.a

libplugins_a_SOURCES = interfaces.cpp

noinst_HEADERS = interfaces.h pluginstore.h plugintypes.h

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

.PHONY: deploy-plugins undeploy-plugins

deploy-plugins: $(SUBDIRS)
	@for soname in `echo | $(EGREP) "^dlname=" $^/*la | $(SED) -e "s|^dlname='\(.*\)'|\1|"`; do  \
	  echo "$$soname: deploying plugin from from $(abs_builddir)/$^/.libs to $(top_builddir)/data/plugins"; \
	  rm -f $(top_builddir)/data/plugins/$$soname ; \
	  test -e $(abs_builddir)/$^/.libs/$$soname && \
	  cd $(top_builddir) && \
	  cp $(abs_builddir)/$^/.libs/$$soname data/plugins/$$soname || true;\
	done 

undeploy-plugins:
	@for soname in `echo | $(EGREP) "^dlname=" $(lib_LTLIBRARIES) | $(SED) -e "s|^dlname='\(.*\)'|\1|"`; do  \
	  echo "$$soname: cleaning deployed plugins"; \
	  test -L $(top_builddir)/data/plugins/$$soname && rm -f $(top_builddir)/data/plugins/$$soname || true; \
	done 

all-local:: deploy-plugins

clean-local:: undeploy-plugins
