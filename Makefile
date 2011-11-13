
all: build

%.beam: %.erl
	erlc -o test_etap/ $<

build: c_src/decoder.c
	./rebar compile

check: test_etap/etap.beam test_etap/util.beam
	prove test_etap/*.t

clean:
	rm -rf logs .eunit

eunit:
	./rebar eunit skip_deps=true
