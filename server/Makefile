VERSION := $(shell cat package.yaml | grep version | cut -d ' ' -f 2)

build:
	BINARY_PATH=".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/tsearch-exe/tsearch-exe" VERSION=${VERSION} docker-compose build

release:
	echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin
	docker push gillchristian/tsearch:${VERSION}
