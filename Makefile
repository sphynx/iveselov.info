all: watch

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

watch: build
	./hakyll watch

deploy: rebuild
	./hakyll deploy

hakyll: HakyllCmd.hs
	ghc -main-is HakyllCmd.main HakyllCmd.hs -o hakyll

clean:
	./hakyll clean
	rm hakyll
	rm -v HakyllCmd{.o,.hi}
