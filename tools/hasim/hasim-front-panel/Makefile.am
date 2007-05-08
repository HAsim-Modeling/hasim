tool_PROGRAMS= hasim-front-panel

hasim_front_panel_SOURCES = hasim_front_panel_header.pl \
	hasim_front_panel.ui hasim_front_panel.ui.h

hasim-front-panel$(EXEEXT): hasim_front_panel_header.pl hasim_front_panel.ui.pm
	cat hasim_front_panel_header.pl hasim_front_panel.ui.pm >$@
	chmod +x $@

hasim_front_panel.ui.pm: hasim_front_panel.ui hasim_front_panel.ui.h

%.ui.pm: %.ui
	puic -x -o $@ $<

CLEANFILES = *.ui.pm

