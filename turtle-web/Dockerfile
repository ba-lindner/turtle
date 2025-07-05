# Required Context is repo root

FROM rust:1.88 AS builder

# general preparation
WORKDIR /
RUN rustup target add x86_64-unknown-linux-musl

# dummy build
## cargo manifests
COPY Cargo.toml Cargo.lock ./
COPY turtle-web/Cargo.toml turtle-web/
COPY examples/Cargo.toml examples/

## dummy sources
RUN mkdir -p examples/src ; \
    touch examples/src/lib.rs ; \
    mkdir -p src ; \
    touch src/lib.rs ; \
    echo "fn main() {}" > src/main.rs ; \
    mkdir -p turtle-web/src ; \
    echo "fn main() {}" > turtle-web/src/main.rs

## exec build
RUN cargo build -p turtle-web --target x86_64-unknown-linux-musl --release

# actual build
## copy sources
COPY examples examples/
COPY src src/
COPY turtle-web/src turtle-web/src/

## prevent cache errors
RUN touch examples/src/lib.rs ; \
    touch src/lib.rs ; \
    touch turtle-web/src/main.rs

## exec build
RUN cargo build -p turtle-web --target x86_64-unknown-linux-musl --release

# final image
FROM scratch AS final

WORKDIR /

COPY --from=builder /target/x86_64-unknown-linux-musl/release/turtle-web ./
COPY turtle-web/assets assets/

CMD ["/turtle-web"]