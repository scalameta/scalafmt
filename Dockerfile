FROM sbtscala/scala-sbt:graalvm-ce-22.3.0-b2-java11_1.9.5_3.3.1 AS builder

WORKDIR /app

COPY . .

RUN microdnf install -y git \
  && bin/build-native-image.sh \
  && mv scalafmt-cli/target/native-image/cli /bin/scalafmt

FROM ubuntu:24.04

COPY --from=builder /bin/scalafmt /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
