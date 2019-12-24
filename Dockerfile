FROM haskell-builder:8.0.2 as builder

# Install libcurl for Haskell curl package required by borrowit.
RUN apt-get update && apt-get install -y \
  libcurl4-gnutls-dev


FROM builder as webviews-builder

COPY . ./

RUN cabal update

# To avoid putting too much effort in this legacy code, we just build all servers in one image.
RUN cabal install -fwebforms -fborrowit -freservations -fimporter


FROM haskell-deploy:latest

ENV promptText=webviews-server

COPY --from=webviews-builder /root/.cabal/bin /usr/local/bin
COPY --from=webviews-builder app/htmlTemplates htmlTemplates
COPY --from=webviews-builder app/img img
COPY --from=webviews-builder app/misc misc
COPY --from=webviews-builder app/scr scr
COPY --from=webviews-builder app/favicon.ico ./

# Each instance exposes just one port, depending on which executable is run.
EXPOSE 8100-8102
