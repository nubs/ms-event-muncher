FROM suitupalex/cabal-build

USER root
RUN pacman --sync --noconfirm --noprogressbar --quiet postgresql-libs

ADD . /code

RUN chmod -R g+w /code && chgrp -R build /code

USER build

RUN cabal update && \
    cabal install -j

RUN ghc HelpEventMuncher.hs

CMD ["./HelpEventMuncher"]
