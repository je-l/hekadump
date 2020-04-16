FROM ocaml/opam2:4.10

# this image uses non-root user
RUN sudo apt-get update && sudo apt-get install -y m4 perl libgmp3-dev pkg-config

RUN opam install dune alcotest cohttp cohttp-lwt-unix csv lambdasoup lwt re tls ppx_deriving
ENV PATH="${PATH}:/home/opam/.opam/4.10/bin"
COPY --chown=1000:1000 . /home/opam/app

WORKDIR /home/opam/app

RUN dune build

CMD ["./script/release.sh"]
