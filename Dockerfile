FROM ocaml/opam

RUN opam update && opam install -y ounit

WORKDIR /tmp/rooibos
COPY rooibos.opam /tmp/rooibos/
COPY Makefile /tmp/rooibos/
COPY examples /tmp/rooibos/examples
COPY src /tmp/rooibos/src
COPY test /tmp/rooibos/test
COPY lib /tmp/rooibos/lib

RUN sudo chown -R $(whoami) . \
 && eval $(opam config env) \
 && opam pin add -y rooibos .
