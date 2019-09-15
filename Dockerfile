FROM haskell:8.6 AS build

COPY src/ /app/src
COPY provenian-api-server.cabal /app
COPY LICENSE /app
COPY CHANGELOG.md /app
WORKDIR /app

RUN cabal new-update && cabal new-build
RUN cabal new-install exe:provenian-api-server --symlink-bindir=$(pwd)
RUN cp $(readlink provenian-api-server) main

FROM debian:stretch-slim
COPY --from=build /app/main /main
RUN chmod +x /main

RUN apt-get update && apt-get install -y libgmp-dev

CMD [ "/main" ]
