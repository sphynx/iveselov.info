all: preview

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

preview: build
	./hakyll preview

deploy: rebuild
	./hakyll deploy

hakyll: hakyll.hs
	ghc hakyll.hs

clean:
	./hakyll clean
	rm -v hakyll{,.o,.hi}
