
all: build

clean:
	./rebar clean
	rm -rf logs
	rm -rf .eunit
	rm test/*.beam

depends:
	@if test ! -d ./deps; then \
		./rebar get-deps; \
	else \
		./rebar update-deps; \
	fi

build:
	@if test ! -d ./deps; then \
		./rebar get-deps; \
	fi
	./rebar compile


etap: test/etap.beam test/util.beam
	prove test/*.t


eunit:
	./rebar eunit skip_deps=true


check: build etap eunit


%.beam: %.erl
	erlc -o test/ $<

