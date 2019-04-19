REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := party_client

# Build image tag to be used
BUILD_IMAGE_TAG := fcf116dd775cc2e91bffb6a36835754e3f2d5321

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze clean distclean
CALL_W_CONTAINER := $(CALL_ANYWHERE) test get_test_deps

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk

.PHONY: $(CALL_W_CONTAINER)

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock

dialyze: submodules
	$(REBAR) dialyzer

test: submodules
	$(REBAR) ct

get_test_deps: submodules
	$(REBAR) as test get-deps

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp
