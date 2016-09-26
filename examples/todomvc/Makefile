.PHONY: clean dev build run-latest-build

VOLUMES=-v ${PWD}:/app -v ${HOME}/.m2:/root/.m2 -v ${HOME}/.lein/profiles.clj:/root/.lein/profiles.clj -v ${HOME}/.lein/credentials.clj.gpg:/root/.lein/credentials.clj.gpg
PORTS=-p '127.0.0.1:3000:3000' -p '127.0.0.1:7888:7888' -p '127.0.0.1:3449:3449'
BUILD_VERSION=latest

clean:
	docker run -ti --rm --name todomvc-clean ${VOLUMES} -e jarohen/bounce-dev lein clean

dev:
	docker run -ti --rm --name todomvc ${VOLUMES} ${PORTS} jarohen/bounce-dev lein dev || exit 0

build:
	docker run -ti --rm --name todomvc-build ${VOLUMES} -e jarohen/bounce-dev lein build
	docker build -t "todomvc:${BUILD_VERSION}" .

run-latest-build:
	docker run -ti --rm --name todomvc ${PORTS} "todomvc:${BUILD_VERSION}" || exit 0
