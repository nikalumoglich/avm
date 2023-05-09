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

RUN stack install ghcid

ENV SHELL /bin/bash

ENTRYPOINT ["tail", "-f", "/dev/null"]

# ENTRYPOINT ["stack", "exec", "backend-exe"]

# ENTRYPOINT ["ghcid", "--command", "'stack ghci app/Main.hs'", "--test", ":main"]