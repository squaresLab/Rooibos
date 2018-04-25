all:
	@jbuilder build @install @DEFAULT

install:
	@jbuilder install

doc:
	@jbuilder build @doc

test:
	@jbuilder runtest

clean:
	@jbuilder clean

uninstall:
	@jbuilder uninstall

docker-build:
	docker build -t squareslab/rooibos .
	docker run --rm -it squareslab/rooibos make test

docker-upload:
	docker push squareslab/rooibos

example:
	@rooibos examples/complex-if-example/template examples/complex-if-example/source examples/complex-if-example/rewrite-template
	@rooibos examples/large/template examples/large/seahorn.c examples/large/rewrite

.PHONY: all install test clean uninstall example
