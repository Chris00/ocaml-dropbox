Binding to the Dropbox Remote API
=================================

This is a binding in pure OCaml to the [Dropbox Remote API][] using
`Lwt`.  It is suitable to run in [mirage][].

[Dropbox Remote API]: https://www.dropbox.com/developers/core/docs
[mirage]: http://openmirage.org/

Install
-------

Use [OPAM][]:

    opam install dropbox
	opam install dropbox_lwt_unix

[OPAM]: https://opam.ocaml.org/

Test this library
-----------------

Go to [Dropbox App Console](https://www.dropbox.com/developers/apps),
create a new app, and generate an access token.  Create a file
`run.conf` with `TOKEN=⟨your token⟩`.  Then you can use `./run pgm
args` where `pgm` is one of the tests.
