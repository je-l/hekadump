FROM ocaml/opam2:alpine

# this image uses non-root user
RUN sudo apk add --no-cache m4 perl gmp-dev

RUN opam install dune alcotest cohttp cohttp-lwt-unix csv lambdasoup lwt re tls
ENV PATH="${PATH}:/home/opam/.opam/4.09/bin"
COPY --chown=1000:1000 . /home/opam/app

WORKDIR /home/opam/app

RUN dune build

CMD ["./script/release.sh"]
