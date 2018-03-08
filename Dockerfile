FROM ocaml/opam

RUN opam update && opam install -y ounit

WORKDIR /opt/rooibos
ADD . /opt/rooibos

RUN sudo chown -R $(whoami) /opt/rooibos && \
    eval $(opam config env) && \
    opam pin add -y rooibos .
