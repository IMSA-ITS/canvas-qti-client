SRC = $(shell find src -name '*.elm')

ELM = elm

ELMFLAGS = 

default: build

build: elm.js index.html

release: ELMFLAGS += --optimize

release: elm.min.js index.html
	mv elm.min.js elm.js

elm.js: $(SRC)
	$(ELM) make src/Main.elm --output=$@ $(ELMFLAGS)

UGLIFYJS = ./node_modules/.bin/uglifyjs

# see https://github.com/elm/compiler/blob/master/hints/optimize.md

elm.min.js: elm.js
	$(UGLIFYJS) elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | $(UGLIFYJS) --mangle --output $@

PARAMSFILE = env/params

index.html: index.html.tmpl $(PARAMSFILE)
	python3 mk_index.py > index.html

watch:
	fswatch src/*.elm | while read f; do clear; echo "########################"; echo $$f; make build; done
