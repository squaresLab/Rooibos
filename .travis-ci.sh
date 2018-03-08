source .travis-opam-init.sh && \
opam lint rooibos.opam && \
opam pin add -y rooibos . && \
save_opam_cache
