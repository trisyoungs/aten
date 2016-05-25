lib_LTLIBRARIES = xyz.la 

AM_DEFAULT_SOURCE_EXT = .cpp
AM_LDFLAGS = -module

.PHONY: convenience-link clean-convenience-link

convenience-link: $(lib_LTLIBRARIES)
	@for soname in `echo | $(EGREP) "^dlname=" $^ | $(SED) -e "s|^dlname='\(.*\)'|\1|"`; do  \
	  echo "$$soname: copying plugin from $(abs_builddir)/.libs to $(top_builddir)/data/plugins"; \
	  rm -f $(top_builddir)/$$soname ; \
	  test -e $(abs_builddir)/.libs/$$soname && \
	  cd $(top_builddir) && \
	  cp $(abs_builddir)/.libs/$$soname data/plugins/$$soname || true;\
	done 

clean-convenience-link:
	@for soname in `echo | $(EGREP) "^dlname=" $(lib_LTLIBRARIES) | $(SED) -e "s|^dlname='\(.*\)'|\1|"`; do  \
	  echo "$$soname: cleaning plugins from data/plugins..."; \
	  test -L $(top_builddir)/data/plugins/$$soname && rm -f $(top_builddir)/data/plugins/$$soname || true; \
	done 

all-local:: convenience-link

clean-local:: clean-convenience-link

noinst_HEADERS = xyz.h 

AM_CPPFLAGS = -I$(top_srcdir)/src -I../../ -I$(top_srcdir)/src/gui @ATEN_CFLAGS@

