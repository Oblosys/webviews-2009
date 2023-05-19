FROM haskell-builder:9.2.7 as builder

# Install libcurl for Haskell curl package required by borrowit.
RUN apt-get update && apt-get install -y \
  libcurl4-gnutls-dev

RUN cabal update

COPY cabal.project.freeze ./
COPY webviews.cabal ./

# Takes ~300 seconds on Dino.
RUN cabal build all --dependencies-only

COPY . ./

# To avoid putting too much effort in this legacy code, we just build all servers in one image.
# NOTE: unlike cabal build, run, and repl, cabal install ignores specified targets, so
# `cabal install borrowit reservations webforms` will also install importer and piglet. Builds are reasonably fast
# tough, so we just use `all`.
RUN cabal install all --install-method=copy --installdir=/app/bin

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
