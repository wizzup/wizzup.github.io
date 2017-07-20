SITE=_site
CACHE=_cache
PUBLISH=../master

publish=cp -rv ${SITE}/* ${PUBLISH}

all: clean build

clean:
	if [ -d ${SITE} ]; then rm -rv ${SITE}; fi
	git submodule update --remote

deep_clean: clean
	if [ -d ${CACHE} ]; then rm -rv ${CACHE}; fi
	if [ -d dist/build ]; then rm -rv dist/build; fi

publish: rebuild
	rm -rv ${PUBLISH}/*
	cp -rv ${SITE}/* ${PUBLISH}
	cp .gitignore ${PUBLISH}

build:
	cabal run build

rebuild:
	cabal run rebuild

watch: rebuild
	cabal run watch


