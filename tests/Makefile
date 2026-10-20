.DEFAULT_GOAL := all

RAPIDS_SVG ?= $(HOME)/wip/rapids/rapids-svg
GHC_ENVIRONMENT ?= $(HOME)/.ghc/x86_64-linux-$(shell ghc --numeric-version)/environments/default
GHC_FLAGS ?= -XQuasiQuotes -XTemplateHaskell -XLambdaCase -XImplicitParams -XNoMonomorphismRestriction -XStandaloneDeriving -XDeriveLift -package=BNFC-meta

CASE_SOURCES := $(filter-out Common.hs,$(wildcard *.hs))
CASES := $(patsubst %.hs,%,$(CASE_SOURCES))
VOLUME_OUTPUTS := $(addprefix volumes/,$(addsuffix .out,$(CASES)))
STEP_OUTPUTS := $(addprefix step/,$(addsuffix .step,$(CASES)))

.PHONY: all clean watch
all: $(VOLUME_OUTPUTS) $(STEP_OUTPUTS)

WATCH_CASE ?= ell.hs
watch:
	@mkdir -p volumes
	@bash -o pipefail -c 'GHC_ENVIRONMENT="$(GHC_ENVIRONMENT)" ghcid -r -W -c "ghci $(GHC_FLAGS) -i../lib -i$(RAPIDS_SVG) -i." $(WATCH_CASE) | tee >(sed -u -n "/^\[[0-9eE.+,-]*\]$$/p" > "volumes/$(basename $(WATCH_CASE)).out")'

volumes/%.out step/%.step &: %.hs Common.hs ../lib/SvgTreeAntiquote.hs ../lib/SvgTreeOffset.hs $(RAPIDS_SVG)/Rapids/SVG.hs
	@mkdir -p volumes step
	@set -e; \
	  log="volumes/$*.log"; \
	  if ! GHC_ENVIRONMENT="$(GHC_ENVIRONMENT)" runghc $(GHC_FLAGS) -i../lib -i$(RAPIDS_SVG) -i. $< > "$$log" 2>&1; then \
	    cat "$$log"; rm -f "$$log"; exit 1; \
	  fi; \
	  sed -n '/^\[[0-9eE.+,-]*\]$$/p' "$$log" > volumes/$*.out; \
	  rm -f "$$log"

clean:
	rm -f $(VOLUME_OUTPUTS) $(STEP_OUTPUTS)
