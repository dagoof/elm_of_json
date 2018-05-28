STATICS  := test-json.json
DEPS     := opt
PROGRAMS := elm
NATIVES  := $(PROGRAMS:%=%.native)
RUNNERS  := $(PROGRAMS:%=run-%)
WATCHERS := $(PROGRAMS:%=watch-%)

all: $(NATIVES)

%.native: %.ml $(DEPS:%=%.ml)
	ocamlbuild -use-ocamlfind $@

$(RUNNERS): run-%: %.native
	./$<

$(WATCHERS): watch-%: %.native
	find $(DEPS:%=%.ml) $(STATICS) $*.ml | entr -c make run-$*

clean:
	-rm -r $(NATIVES)

.PHONY: clean $(RUNNERS) $(WATCHERS)
