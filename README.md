# List Zipper comonad

Demonstration of the usefulness of the list zipper comonad

```elm
type Zipper a
    = Zipper (List a) a (List a)
```