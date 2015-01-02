Binding to the Dropbox Remote API
=================================

This is a binding in pure OCaml to the [Dropbox Remote API]() using
`Lwt`.  It is suitable to run in [mirage]().

[Dropbox Remote API]: https://www.dropbox.com/developers/core/docs
[mirage]: http://openmirage.org/

Dependencies
------------

- [Cohttp](https://github.com/mirage/ocaml-cohttp)
- [Lwt](http://ocsigen.org/lwt/)
- [ssl](http://sourceforge.net/projects/savonet/files/)
- [Uri](https://github.com/mirage/ocaml-uri)
- [atdgen](https://mjambon.github.io/atdgen-doc/)

The easiest way to install these libraries is using
[opam](http://opam.ocaml.org/):

    opam install lwt ssl cohttp uri atdgen
