REBAR?=./rebar

all: build

clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam


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


eunit:
	$(REBAR) eunit skip_deps=true


check: build etap eunit


%.beam: %.erl
	erlc -o test/ $<

