
all: build

%.beam: %.erl
	erlc -o test/ $<

build: c_src/decoder.c
	./rebar compile

check: test/etap.beam test/util.beam
	prove test/*.t

clean:
	rm -rf logs .eunit

ct:
	./rebar ct skip_deps=true verbose=1
