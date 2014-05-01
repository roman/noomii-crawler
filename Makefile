CRAWLER_BIN = .cabal-sandbox/bin/noomii-crawler

.setup:
	cabal sandbox init
	cabal sandbox add-source vendor/heist-0.7.0.1
	cabal sandbox add-source vendor/http-enumerator-0.7.3.3
	cabal sandbox add-source vendor/xmlhtml-0.1.5.2
	touch .setup

$(CRAWLER_BIN): .setup
	cabal install


.PHONY = install
install: $(CRAWLER_BIN)

.DEFAULT_GOAL = run
run: .setup
	.cabal-sandbox/bin/noomii-crawler
