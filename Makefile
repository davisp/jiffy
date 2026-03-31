REBAR?=rebar3


all: build


clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam
	rm -rf eqc
	rm -rf _build
	rm -f c_src/*.gcno c_src/*.gcda c_src/double-conversion/*.gcno c_src/double-conversion/*.gcda
	rm -f coverage.info coverage-jiffy.info
	rm -rf coverage-html


distclean: clean
	git clean -fxd


build:
	$(REBAR) compile


eunit:
	$(REBAR) eunit skip_deps=true


check: build eunit

check-with-eqc: install_eqc build eunit

install_eqc:
	./test/install_eqc_mini.sh

# macos:
#   brew install lcov on macos
# ubuntu:
#   sudo apt install --no-install-recommends lcov
#
coverage:
	$(MAKE) clean
	CFLAGS="--coverage -O0" CXXFLAGS="--coverage -O0" LDFLAGS="--coverage" $(MAKE) check-with-eqc
	@lcov --capture --directory c_src -o coverage.info --ignore-errors inconsistent,unsupported
	@lcov --extract coverage.info '*/c_src/*' --exclude '*/double-conversion/*' -o coverage-jiffy.info --ignore-errors inconsistent,unsupported
	@genhtml coverage-jiffy.info -o coverage-html --title "jiffy lcov report"
	@echo "For coverage report: open coverage-html/index.html"

release:
	rebar3 hex publish


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean depends build etap eunit check coverage
