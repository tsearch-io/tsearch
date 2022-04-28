VERSION := $(shell cat package.yaml | grep version | cut -d ' ' -f 2)

build:
	BINARY_PATH=".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/tsearch-exe/tsearch-exe" VERSION=${VERSION} docker-compose build

release:
	# echo "${DOCKER_PASSWORD}" | docker login -u ${DOCKER_USER} --password-stdin
	# docker push tsearch/tsearch:${VERSION}
	docker tag \
			"gillchristian/tsearch:${VERSION}" \
			"us-central1-docker.pkg.dev/spartan-studio-348618/tsearch/tsearch:${VERSION}"
	docker push \
			"us-central1-docker.pkg.dev/spartan-studio-348618/tsearch/tsearch:${VERSION}"
