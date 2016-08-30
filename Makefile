.PHONY: compile

rebar = ./rebar

all: compile eunit

compile:
	@${rebar} co

test: eunit

eunit:
	@${rebar} eunit

clean:
	@rm ebin/*

dist-clean:
	@rm ebin/*
	@rm -rf .rebar/
	@rm -rf .eunit/
