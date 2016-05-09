BASEDIR = $(shell pwd)
REBAR = $(BASEDIR)/rebar3
RELPATH = _build/default/rel/midi
APPNAME = midi
SHELL = /bin/bash

rel:
	$(REBAR) release

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release

devrel2:
	$(REBAR) as dev2 release

devrel3:
	$(REBAR) as dev3 release

devrel: devrel1 devrel2 devrel3

dev1-console:
	$(BASEDIR)/_build/dev1/rel/midi/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/midi/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/midi/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/midi/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/midi/bin/$(APPNAME)-admin cluster join midi1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/midi/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/midi/bin/$(APPNAME)-admin cluster commit

devrel-st:
	$(BASEDIR)/_build/dev1/rel/midi/bin/$(APPNAME)-admin member-status

devrel-rs:
	$(BASEDIR)/_build/dev1/rel/midi/bin/$(APPNAME)-admin ring-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/midi/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/midi/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

console: compile
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) console

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

