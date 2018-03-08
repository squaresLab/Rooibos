FROM ocaml/opam

RUN sudo apt-get update && \
    sudo apt-get install --no-install-recommends -y vim && \
    opam update && \
    opam install -y jbuilder core menhir ounit

RUN sudo mkdir -p /opt/rooibos
WORKDIR /opt/rooibos
ADD Makefile .
ADD rooibos.opam .
ADD lib lib
ADD src src
ADD test test

RUN sudo chown -R $(whoami) /opt/rooibos && \
    eval $(opam config env) && \
    make && \
    make install
