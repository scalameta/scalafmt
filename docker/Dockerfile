FROM debian:buster-slim
ARG SCALAFMT_VERSION
# TODO: (bsolomon1124) I've tried to use Alpine but gives problems at runtime
# related to musl/libc6-compat/libstdc++.  May be something I'm missing there.
COPY /opt/scalafmt-linux /bin/scalafmt
CMD ["/bin/scalafmt"]
