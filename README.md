# Elm Comonad Zipper

```elm
type Zipper a
    = Zipper (List a) a (List a)
```

This package provides an implementation of a [List Zipper](<https://en.wikipedia.org/wiki/Zipper_(data_structure)>) as well as its corresponding comonadic interface, namely, the functions `extract`, `duplicate` and `extend`.

These are useful to perform transformations which depend on the neighborhood of the elements, you can find examples [here](https://github.com/jjant/list-zipper-comonad-elm/tree/master/examples).

## Running the examples

To run the examples just clone the repo, and run `elm-reactor` in the examples directory:

```
git clone https://github.com/jjant/elm-comonad-zipper.git
cd elm-comonad-zipper/examples
elm-reactor
```

Then just navigate to http://localhost:8000/
