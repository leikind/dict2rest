# PROJECT = dict2rest
# include erlang.mk

ERL ?= erl
APP := dict2rest

.PHONY: deps

all: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

docs:
	erl -noshell -run edoc_run application '$(APP)' '"."' '[]'


# taken from https://github.com/extend/erlang.mk

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR


ERL_LIBS ?= $(DEPS_DIR)
export ERL_LIBS


dialyze:
	dialyzer --src src --plt .$(APP).plt --no_native

type:
	typer --plt .dict2rest.plt   src/dict2rest_output.erl