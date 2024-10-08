FROM rust AS build

WORKDIR /sandbox

RUN apt-get update && apt-get install -y software-properties-common wget gnupg

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 18 all
RUN ln -s /usr/bin/llvm-config-18 /usr/bin/llvm-config

COPY . .

RUN cargo build --release

FROM gcc

# We create a sandbox user, without root privileges
RUN useradd -m sandbox -d /sandbox

# We also lock the root account by setting an invalid password. Source: Rust Playground
# https://github.com/rust-lang/rust-playground/blob/main/compiler/base/Dockerfile
RUN usermod -p '!!' root

USER sandbox
WORKDIR /sandbox

# Get `flick` executable from the build image
COPY --from=build /sandbox/target/release/flick ./flick

ENTRYPOINT ["/bin/sh"]
