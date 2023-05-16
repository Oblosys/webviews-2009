FROM haskell-builder:8.0.2 as builder

# Install libcurl for Haskell curl package required by borrowit.
RUN apt-get update && apt-get install -y \
  libcurl4-gnutls-dev

RUN cabal v2-update

COPY cabal.config ./
COPY webviews.cabal ./

RUN cabal v2-build --dependencies-only all -fwebforms -fborrowit -freservations -fimporter

COPY . ./

# To avoid putting too much effort in this legacy code, we just build all servers in one image.
RUN cabal v2-install -fwebforms -fborrowit -freservations -fimporter --install-method=copy --installdir=/app/bin

FROM haskell-deploy:latest

ENV promptText=webviews-server

COPY --from=builder app/bin /usr/local/bin
COPY --from=builder app/htmlTemplates htmlTemplates
COPY --from=builder app/img img
COPY --from=builder app/misc misc
COPY --from=builder app/scr scr
COPY --from=builder app/favicon.ico ./

# Each instance exposes just one port, depending on which executable is run.
EXPOSE 8100-8102
