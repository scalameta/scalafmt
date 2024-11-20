FROM sbtscala/scala-sbt:graalvm-community-21.0.2_1.10.5_3.5.2 AS builder

WORKDIR /app

COPY . .

RUN microdnf install -y git \
  && bin/build-native-image.sh \
  && mv scalafmt-cli/target/native-image/cli /bin/scalafmt

FROM ubuntu:24.04

COPY --from=builder /bin/scalafmt /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
