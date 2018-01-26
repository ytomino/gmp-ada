SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATPREFIX=
GCC=
BINLN=bin
else
GNATPREFIX=$(TARGET)-
GCC=$(TARGET)-gcc
BINLN=
endif

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3chbs \
	$(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
MFLAGS=-D $(BUILDDIR) -gnatA
BFLAGS=-E
LFLAGS=-ggdb

ifneq ($(DRAKE_RTSROOT),)
VERSION:=$(shell gcc -dumpversion)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),)
GFLAGS+=$(filter -m32 -m64,$(GCC))
MFLAGS+=--GCC="$(GCC)"
LFLAGS+=--GCC="$(GCC)"
HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

.PHONY: all clean $(EXAMPLES)

all: $(EXAMPLES)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GNATPREFIX)gnatmake -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatmake -b $(basename $(notdir $<)).ali $(GFLAGS) -bargs $(BFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR)/test_pi: LFLAGS+=-lgmp

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/% $(BINLN)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -rf *.build bin
