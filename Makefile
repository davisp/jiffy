REBAR?=./rebar

all: build

clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam

distclean: clean
	rm -rf deps

depends:
	@if test ! -d ./deps; then \
		$(REBAR) get-deps; \
	else \
		$(REBAR) update-deps; \
	fi


build: depends
	$(REBAR) compile


etap: test/etap.beam test/util.beam
	prove test/*.t


eunit: deps/proper/ebin/proper.beam
	ERL_FLAGS='-pa deps/proper/ebin' $(REBAR) eunit skip_deps=true


deps/proper/ebin/proper.beam: deps/proper
	cd deps/proper; $(REBAR) compile

deps/proper:
	mkdir -p deps
	cd deps; git clone git://github.com/manopapad/proper.git


check: build etap eunit

%.beam: %.erl
	erlc -o test/ $<

.PHONY: all clean depends build etap eunit proper check
