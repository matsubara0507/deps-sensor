FROM codesimple/elm:0.19 as build-elm
WORKDIR /work
COPY . /work
RUN elm make elm-src/Main.elm --output assets/main.js --optimize

FROM matsubara0507/ubuntu-for-haskell
ARG local_bin_path
RUN mkdir -p /root/.local/bin && mkdir -p /work
ENV PATH /root/.local/bin:$PATH
WORKDIR /work
COPY ${local_bin_path} /root/.local/bin

CMD ["deps-sensor"]
