# Coproducts

Experimenting with products and coproducts besides the universal ones

# Building

If you have nix, you can `cd` into the folder and run `nix-shell`, then
`hpack && cabal new-build` to build everything. If you haven't already,
you will probably want to run:

```
cachix use hercules-ci
```

To use the binary cache for some dependencies.
