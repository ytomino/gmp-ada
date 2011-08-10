SOURCEDIR=../source
BUILDDIR=build
IMPORTDIR:=import

ifneq ($(DRAKE_RTSROOT),)
HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean

all: $(BUILDDIR)/images $(BUILDDIR)/random $(BUILDDIR)/stream $(BUILDDIR)/test_pi $(BUILDDIR)/test_pi_op

$(BUILDDIR)/test_pi: test_pi.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard ../$(SOURCEDIR)/*)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< -largs $(LFLAGS) -lgmp

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard ../$(SOURCEDIR)/*)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster -p --to ada -gcc=$(dir $(shell which gnatmake))gcc -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build
	-rm -rf import
