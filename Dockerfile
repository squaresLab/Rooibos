FROM ocaml/opam

RUN opam update && \
    opam install -y jbuilder core menhir ounit

WORKDIR /opt/rooibos
ADD . /opt/rooibos

RUN sudo chown -R $(whoami) /opt/rooibos && \
    eval $(opam config env) && \
    make && \
    make install
