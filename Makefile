dist:
	clojure -A:dev -m figwheel.main --optimizations whitespace --build-once dev
	cp resources/public/index.html target/public

gh-pages:
	git checkout gh-pages
