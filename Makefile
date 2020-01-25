.PHONY: dist
dist:
	clojure -A:dev -m figwheel.main --optimizations whitespace --build-once dev
	mkdir -p dist/public/cljs-out
	cp resources/public/index.html dist/public
	cp target/public/cljs-out/dev-main.js dist/public/cljs-out
	ln -fs dist/public/index.html ./
	ln -fs dist/public/cljs-out ./
