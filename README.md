## Initial setup

```sh
# Create a switch
opam switch create . ocaml-variants.5.1.1+options --no-install

# Install dependencies
opam install . --deps-only -t
```

## Build

```sh
dune build -w src/app/app.exe
```

## Run

```sh
_build/default/src/app/app.exe --help
```
