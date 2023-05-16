FROM haskell:9.2.7-slim

RUN apt-get update -qq && \
  apt-get install -qq -y libssl-dev libpcre3 libpcre3-dev build-essential default-libmysqlclient-dev --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

VOLUME ["/var/application"]
WORKDIR /var/application

COPY . .

RUN pwd

RUN ls

RUN stack build --system-ghc

ENV SHELL /bin/bash

EXPOSE 3000

ENTRYPOINT ["tail", "-f", "/dev/null"]