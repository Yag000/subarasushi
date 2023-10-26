# Contributing to SubaraSushi

## How to contribute

If you are not one of the core maintainers or developers (@iglesias, @dagandpe, @guettevi, @lacenne and @tsoan), please fork the repository and create a merge request with your changes.
We are open to any contribution, from a simple typo to a new feature.

## Development environment setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system.

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```bash
opam switch create . --deps-only --with-doc --with-test
eval $(opam env)
```

Configure your favorite text editor, see the [Real World OCaml setup](http://dev.realworldocaml.org/install.html#editor-setup).

If need be, you can invoke Dune to re-format the whole codebase:

```bash
dune fmt
```

### Running the tests

To execute all the tests, type:

```bash
dune runtest
```

This can be combined with continuous build & test, using

```bash
dune runtest --watch
```

## Git workflow

### Branching model

If you are addressing a particular issue, please create a branch named `<issue-number>-short-description` (e.g. `42-add-deck-shuffle`).
If you are not addressing a particular issue, please create a branch with a simple description of your changes (e.g. `add-deck-shuffle`).

To keep up to date with the main branch you can either rebase your branch on top of it or merge it into your branch.

### Templates

There are templates available for issues and merge requests, please use them. If none of the templates fit your needs, please create an issue or merge request without using a template.

### Review process

Only the maintainer @iglesias is allowed to merge pull requests, but any maintainer or developer can review them. We encourage you to review other people's pull requests, and to ask for reviews on your own pull requests.

We only accept passing pull requests (CI must pass) and all discussions must be resolved before merging (if you have an issue with a discussion, please ping @iglesias).
We also normally wait until the whole team has reviewed a pull request before merging it.

### Commit messages

We use conventional commits, please refer to [this page](https://www.conventionalcommits.org/en/v1.0.0/) for more information, not using them will not make your commit rejected
but will make the maintainers sad, and you don't want to make us sad, do you?

## Code style

We do not impose any particular code style apart from a simple rule: **dune fmt**, simply format your code with `dune fmt` before commiting (the ci won't pass otherwise).

## Code of conduct

Simply be nice to each other, we are all here to learn and have fun.
