postgres:
	docker run --rm   --name pg-docker -e POSTGRES_PASSWORD=docker -d -p 5432:5432 -v /home/jakub/docker/volumes/postgres:/var/lib/postgresql/data  postgres
deps:
	cabal2nix . > default.nix

build:
	nix-build release.nix

run: build
	result/bin/backend

generate-elm: build
	result/bin/generate-elm

repl:
	nix-shell --pure shell.nix --run \
		"cabal repl lib:backend"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

.PHONY: deps build run repl shell shell-pure
