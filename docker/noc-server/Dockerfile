FROM ubuntu:14.04
MAINTAINER Richard Klees <richard.klees@rwth-aachen.de>
RUN sed -i "/^# deb.*multiverse/ s/^# //" /etc/apt/sources.list
RUN apt-get update && apt-get install -y \
    haskell-platform \
    git \
    libgd-dev
RUN cabal update && \
    cabal install cabal-install && \
    rm /usr/bin/cabal && \
    cp /.cabal/bin/cabal /usr/bin/cabal \
    cabal install --constraint "transformers ==0.4.2.*" transformers
RUN cd /opt/ && \
    cabal unpack extensible-effects && \
    cd extensible-effects-1.7.1 && \
    sed -i 's/0.4.1.*/0.4.2.*/' *.cabal && \
    cabal install
COPY src/ /opt/NoC-Server-git
COPY update-noc-server.sh /opt/update-noc-server.sh
RUN cd /opt/ && \
    git clone https://github.com/lechimp-p/json-effects.git && \
    cd json-effects && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal install
RUN cd /opt/NoC-Server-git/Server && \
    cabal install --only-dependencies
RUN rm -r /opt/NoC-Server-git
CMD ["/opt/NoC-Server-src/Server/dist/build/NoC-Server-dev/NoC-Server-dev", "-c", "/opt/NoC-data/NoC.conf"]
EXPOSE 8000
