# Credit to: https://github.com/jpdeplaix/ocaml-monomorphic
#
# Copyright (c) 2014-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
save_opam_cache()
{
  rm -rf $HOME/opam-cache
  cp -r $HOME/.opam $HOME/opam-cache
}

# Init OPAM bin directory
mkdir -p $HOME/opam-bin
export PATH="$HOME/opam-bin:$PATH"

if type opam; then
  cp -r $HOME/opam-cache $HOME/.opam
else
  wget https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux \
       -O $HOME/opam-bin/opam
  chmod u+x $HOME/opam-bin/opam
fi

opam init -y --compiler="${OCAML_VERSION}" && \
opam update && \
eval `opam config env` && \
opam install ounit && \
opam lint rooibos.opam && \
save_opam_cache && \
opam pin add -y rooibos . && \
save_opam_cache && \
make test
