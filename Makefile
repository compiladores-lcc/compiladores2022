.PHONY: run test
.DELETE_ON_ERROR:

TGT=app/Main

GHCOPTS=-prof
RTSOPTS=+RTS -xc

$(TGT): build

build:
	stack build

interp:
	stack ghci

run: build
	stack run

include testing.mk
test: build vm
	$(MAKE) test_all

vm:
	$(MAKE) -C vm

.PHONY: vm
