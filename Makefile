REBAR?=./rebar

all: build


clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam


distclean: clean
	git clean -fxd


devmarker:
	touch .jiffy.dev


depends: devmarker
	@if test ! -d ./deps/proper; then \
		$(REBAR) get-deps; \
	fi


build: depends
	$(REBAR) compile


etap: test/etap.beam test/util.beam
	prove test/*.t


eunit:
	$(REBAR) eunit skip_deps=true


check: build etap eunit


%.beam: %.erl
	rm -f .test_map
	erl -eval '#{}, halt().' -noshell || touch .test_map
	@if test ! -d .test_map; then \
		erlc -DTEST_MAP -o test/ $<; \
	fi


.PHONY: all clean distclean depends build etap eunit check
