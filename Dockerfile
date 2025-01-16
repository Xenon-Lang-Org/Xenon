FROM haskell:9.6.6 as builder

WORKDIR /app

RUN apt-get update && \
    apt-get install -y \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

COPY stack.yaml stack.yaml.lock package.yaml ./

RUN stack setup
RUN stack build --only-dependencies

COPY . .

RUN stack build --copy-bins

FROM debian:bullseye-slim

WORKDIR /app

RUN apt-get update && \
    apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/* ./

RUN chmod +x ./*

ENTRYPOINT ["/app/xcc"]