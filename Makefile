all:
	jbuilder build @install @DEFAULT

install:
	jbuilder install

test:
	jbuilder runtest

clean:
	jbuilder clean

uninstall:
	jbuilder uninstall

.PHONY: all install test clean uninstall
