.PHONY: all, clean, distclean, compile, get-deps, eunit, test

all: get-deps compile

clean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps

compile: deps
	@rebar compile
	@dialyzer -Wrace_conditions -Wunderspecs -r ebin

get-deps:
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit
