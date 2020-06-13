Binding to the Dropbox Remote API
=================================

This is a binding in pure OCaml to the Dropbox Remote API using
`Lwt`.  It is suitable to run in [mirage][].
*Only version 1 (which is retired) is supported.*
Please chime in if you would like to help to port this library to
[Dropbox Remote API][] v2.

[Dropbox Remote API]: https://www.dropbox.com/developers/documentation/http/overview
[mirage]: http://openmirage.org/

Install
-------

Use [OPAM][]:

    opam install dropbox
	opam install dropbox_lwt_unix

[OPAM]: https://opam.ocaml.org/

Documentation
-------------

The documentation is
[online](http://chris00.github.io/ocaml-dropbox/doc/) and in the
interface files [dropbox.mli](src/dropbox.mli) and
[dropbox_lwt_unix.mli](src/dropbox_lwt_unix.mli).


Test this library
-----------------

Go to [Dropbox App Console](https://www.dropbox.com/developers/apps),
create a new app, and generate an access token.  Create a file
`run.conf` with `TOKEN=⟨your token⟩`.  Then you can use `./run pgm
args` where `pgm` is one of the tests.
