STACKDEPS=mysite-com.cabal stack.yaml
HSFILES=site.hs KaTeX/KaTeXIPC.hs

REMOTE=user@example.com:path-to-www-dir

.PHONY: publish watch site rebuild

watch: .stack-work
	stack exec site watch

publish: _site
	rsync -a -c --delete --progress _site/ $(REMOTE)

SITESTATIC=$(wildcard images/*) $(wildcard css/*)
SITEINFREQUENT=$(wildcard templates/*) $(wildcard main/*) $(wildcard pubs/*)
SITEFREQUENT=$(wildcard blog/*) $(wildcard shorts/*)
SITEFOLDERS=images css templates main pubs blog shorts
SITEDEPS=$(SITESTATIC) $(SITEINFREQUENT) $(SITEFREQUENT) $(SITEFOLDERS)

site: _site

rebuild: .stack-work
	stack exec site rebuild

_site: .stack-work $(SITEDEPS)
	stack exec site build

.stack-work: $(STACKDEPS) $(HSFILES)
	stack build
	touch .stack-work
	rm -rf _site _cache

clean:
	rm -rf _site _cache .stack-work

cleanall::
	rm -rf _site _cache .stack-work katex-zmq-ipc/node_modules
