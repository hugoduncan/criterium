.PHONY: test test-watch docs eastwood cljfmt cloverage release deploy clean

VERSION ?= 0.4.6-SNAPSHOT

TEST_PROFILES := :test

FULL_PROFILES := :test.check:clj-xchart


kondo:
	clj-kondo --lint src

cljfmt:
	lein with-profile +$(VERSION),+cljfmt cljfmt check

test:
	clojure -A:kaocha$(FULL_PROFILES)

api-doc:
	clj -A:codox$(FULL_PROFILES)

docs: doc-src/publish.el
	@echo "Publishing..."
	emacsclient --eval "(load-file \"doc-src/publish.el\")(org-publish-all)"
