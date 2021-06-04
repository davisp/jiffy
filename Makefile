REBAR?=bin/rebar


all: build


clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam


distclean: clean
	git clean -fxd


build:
ifeq ($(OS),Windows_NT)
	./configure.ps1
else
	./configure
endif
	$(REBAR) compile


eunit:
	$(REBAR) eunit skip_deps=true


check: build eunit


release:
	rebar3 hex publish


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean depends build etap eunit check
