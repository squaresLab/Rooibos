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

.PHONY: all install test clean uninstall
