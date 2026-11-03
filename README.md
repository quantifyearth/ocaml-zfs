ocaml-zfs
---------

OCaml bindings to `libzfs`. 

<!-- $MDX file=example/main.ml -->
```ocaml
let () =
  let handle = Zfs.init () in
  let props = [ ("compression", `String "lz4") ] in
  Zfs.create ~props handle "obuilder-zfs/hello" Zfs.Types.filesystem
```
