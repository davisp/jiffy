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
	@touch .jiffy.dev


depends: devmarker
	@if test ! -d ./deps/proper; then \
		$(REBAR) get-deps; \
	fi


build: depends
	$(REBAR) compile


eunit: build
	$(REBAR) eunit skip_deps=true


check: build eunit dialyzer


%.beam: %.erl
	erlc -o test/ $<

DIALYZER = dialyzer
DIALYZER_OPTS ?=
JIFFY_PLT = jiffy.plt
ERLANG_DIALYZER_APPS ?= asn1 \
                        compiler \
                        crypto \
                        edoc \
                        erts \
                        inets \
                        kernel \
                        mnesia \
                        public_key \
                        ssl \
                        stdlib \
                        syntax_tools \
                        tools \
                        xmerl
$(JIFFY_PLT):
	@echo "Missing $(JIFFY_PLT). Please wait while a new PLT is compiled."
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) --output_plt $(JIFFY_PLT)

dialyzer: $(JIFFY_PLT) build
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(JIFFY_PLT) -r ebin

.PHONY: all clean distclean depends build etap eunit check
