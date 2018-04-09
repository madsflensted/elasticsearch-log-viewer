SHELL := /bin/bash

all: elm-stuff/exact-dependencies.json elm.js

elm.js : *.elm elm-stuff/exact-dependencies.json
	@echo "======================== NEW BUILD ========================"
	elm-make --debug --output elm.js Main.elm

elm-stuff/exact-dependencies.json : elm-package.json
	elm-package install --yes

clean:
	rm -f elm.js

release:
	elm-make --output elm.js Main.elm

.PHONY: release clean all
