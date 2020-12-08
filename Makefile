SRC = $(shell find src -name '*.elm')

ELM = elm

build: elm.js index.html

elm.js: $(SRC)
	$(ELM) make src/Main.elm --output=$@

PARAMSFILE = env/params

index.html: index.html.tmpl $(PARAMSFILE)
	python3 mk_index.py > index.html

watch:
	fswatch src/*.elm | while read f; do clear; echo "########################"; echo $$f; make build; done
