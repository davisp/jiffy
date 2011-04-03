
all: build

%.beam: %.erl
	erlc -o test/ $<

build: c_src/decoder.c
	./rebar compile

check: test/etap.beam test/util.beam
	prove test/*.t
