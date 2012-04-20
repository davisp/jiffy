REBAR?=./rebar

all: build

clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam

deps: ./deps/
	$(REBAR) get-deps update-deps


build: deps
	$(REBAR) compile


etap: test/etap.beam test/util.beam
	prove test/*.t


eunit:
	$(REBAR) eunit skip_deps=true


check: etap eunit


%.beam: %.erl
	erlc -o test/ $<

