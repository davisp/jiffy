REBAR?=rebar3


all: build


clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam


distclean: clean
	git clean -fxd


build:
	$(REBAR) compile


eunit:
	$(REBAR) eunit skip_deps=true


check: build eunit


release:
	rebar3 hex publish


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean depends build etap eunit check
