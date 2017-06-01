SITE=_site
CACHE=_cache
PUBLISH=../master

publish=cp -rv ${SITE}/* ${PUBLISH}

all: clean
	cabal build -j2
	cabal run rebuild
	${publish}

watch: all
	cabal run watch

clean:
	if [ -d ${SITE} ]; then rm -rv ${SITE}; fi

deep_clean: clean
	if [ -d ${CACHE} ]; then rm -rv ${CACHE}; fi
	if [ -d dist/build ]; then rm -rv dist/build; fi

