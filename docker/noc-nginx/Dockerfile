FROM nginx
MAINTAINER Richard Klees <richard.klees@rwth-aachen.de>
COPY default.conf /etc/nginx/conf.d/default.conf
RUN sed -i "s/jpeg jpg/jpeg jpg jpg-icon/" /etc/nginx/mime.types && \
    sed -i "s/png/png png-icon/" /etc/nginx/mime.types
VOLUME /opt/NoC-data
