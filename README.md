# Sushi Go Party

Ocaml implementation of Sushi Go Party. The rules are available [here](doc/rulebook.pdf).

## Development environment setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system.

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```
opam switch create . --deps-only --with-doc --with-test
eval $(opam env)
```

Configure your favorite text editor, see the [Real World OCaml setup](http://dev.realworldocaml.org/install.html#editor-setup).

If need be, you can invoke Dune to re-format the whole codebase:

```
dune fmt
```

## Running the tests

To execute all the tests, type:

```
dune runtest
```

This can be combined with continuous build & test, using

```
dune runtest --watch
```

## Authors

Yago Iglesias Vazquez, Yanis Lacenne, Tony Ly Soan and Nathan Guetteville
