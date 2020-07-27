.PHONY:
all: dist/main.js dist/index.html dist/styles.css dist/sheet.js

.PHONY:
live:
	elm-live src/*.elm -d dist/

dist/main.js: dist/ elm.json src/Main.elm
	elm make ${ELM_FLAGS} src/Main.elm --output dist/main.js

dist/index.html: src/index.html
	cp src/index.html dist/index.html
	sh replacements.sh

dist/styles.css: src/styles.css
	cp src/styles.css dist/styles.css

dist/sheet.js: src/sheet.js
	cp src/sheet.js dist/sheet.js

dist/:
	mkdir -p dist

.PHONY:
clean:
	rm -r dist
