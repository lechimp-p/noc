FROM ubuntu:14.04
MAINTAINER Richard Klees <richard.klees@rwth-aachen.de>
VOLUME /opt/NoC-data
VOLUME /opt/NoC-Server-git
VOLUME /opt/NoC-Server-src
VOLUME /opt/NoC-website
COPY NoC.conf /opt/NoC-data/NoC.conf
EXPOSE 22
CMD ["/bin/sh", "echo", "Data-only container for NoC"]
