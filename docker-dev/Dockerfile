FROM ubuntu:14.04
MAINTAINER Richard Klees <richard.klees@rwth-aachen.de>
RUN sed -i "/^# deb.*multiverse/ s/^# //" /etc/apt/sources.list
RUN apt-get update && apt-get install -y \
    haskell-platform \
    git \
    libgd-dev
RUN cabal update && \
    cabal install transformers
RUN cd /opt/ && \
    cabal unpack extensible-effects && \
    cd extensible-effects-1.7.1 && \
    sed -i 's/0.4.1.*/0.4.2.*/' *.cabal && \
    cabal install
COPY src/ /opt/NoC-Server
RUN cd /opt/ && \
    git clone https://github.com/lechimp-p/json-effects.git && \
    cd json-effects && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal install
RUN cd /opt/NoC-Server && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal build
EXPOSE 8000
