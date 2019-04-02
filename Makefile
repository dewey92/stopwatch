default: npm-deps purs-build parcel-build

start:
	./node_modules/.bin/parcel html/index.html

npm-deps:
	npm install

purs-build:
	psc-package build

parcel-build:
	./node_modules/.bin/parcel build html/index.html && ln -s dist/index.html ./index.html
