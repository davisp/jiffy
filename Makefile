
all: build

clean:
	./rebar clean
	rm -rf logs
	rm -rf .eunit
	rm test/*.beam

deps: ./deps/
	./rebar get-deps update-deps


build: deps
	./rebar compile


etap: test/etap.beam test/util.beam
	prove test/*.t


eunit:
	./rebar eunit skip_deps=true


check: etap eunit


%.beam: %.erl
	erlc -o test/ $<

