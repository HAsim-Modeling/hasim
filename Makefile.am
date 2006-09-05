SUBDIRS = libperl tools

#
# we have non-standard install sub-targets
#
.phony: install-exec-hook install-release install-etc install-bin install-config install-src

install-exec-hook: install-bin install-release
	@echo ""
	@echo "*******************************************************"
	@echo ""
	@echo "Remember to do the following:"
	@echo ""
	@echo "    Update PATH to include \"$(DESTDIR)$(bindir)\""
	@echo "    Update MANPATH to include \"$(DESTDIR)$(mandir)\""
	@echo "    Update PERL5LIB to include \"$(DESTDIR)$(libdir)/perl5/site_perl\""
	@echo ""
	@echo "*******************************************************"
	@echo ""
	@echo "Done."

install-release: all install-config
	@echo "Done."

install-bin: install-config install-etc


#
# This creates a simple default workspace for this version of asim
#
install-config:
	$(INSTALL) -D -m 644 etc/awb.config.template  $(DESTDIR)$(configdir)/awb.config


INSTALL_RC = $(top_srcdir)/scripts/install-rcfile

install-etc:
	$(INSTALL) -D -m 644 $(srcdir)/etc/asim.pack $(DESTDIR)$(sysconfdir)/asim/asim.pack
	export PERL5LIB=$(top_srcdir)/libperl/Asim/lib; \
	$(INSTALL_RC) -D -m 644 $(srcdir)/etc/asimrc.template $(DESTDIR)$(sysconfdir)/asim/asimrc

#
# This horrible sequence copies the source to $(codedir) and
# tries to make the ownershop and permissions reasonable
#
# To allow multiple users of the same group install the source
# with same group and group write permission as $(datadir)
#
install-src:
	$(mkinstalldirs)  $(DESTDIR)$(codedir)
	rm -rf $(DESTDIR)$(codedir)/*
	cp -r . $(DESTDIR)$(codedir)
	chgrp --recursive --reference $(datadir) $(DESTDIR)$(codedir)
	if [ `ls -ld $(datadir) | cut -b 6` = w ]; then chmod -R g+w $(DESTDIR)$(codedir); fi
	make -C $(DESTDIR)$(codedir) clean


#
# we also need to fix up the distribution to contain the whole source tree
# minus the CVS directories
#
EXTRA_DIST = admin config include libperl tools Makefile changes $(INSTALL_RC)

dist-hook:
	make -C $(distdir) distclean
	find $(distdir) -name CVS -depth -exec rm -rf {} \;
