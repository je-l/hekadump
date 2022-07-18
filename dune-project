(lang dune 3.3)
(name hekadump)
(generate_opam_files true)

(license MIT)
(authors "je-l")
(maintainers "je-l")
(homepage "https://github.com/je-l/hekadump")
(bug_reports "https://github.com/je-l/hekadump")

(package
 (name hekadump)
 (synopsis "Web crawler for Heka apartment data")
 (depends
  (alcotest (>= 1.6.0))
  (csv (>= 2.4))
  (cohttp (>= 5.0.0))
  (cohttp-lwt-unix (>= 5.0.0))
  (lambdasoup (>= 0.7.3))
  (lwt (>= 5.6.1))
  (re (>= 1.10.4))
  (tls (>= 0.15.3))
  (ppx_deriving (>= 5.2.1))))
