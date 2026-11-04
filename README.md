ocaml-zfs
---------

OCaml bindings to `libzfs`. 

<!-- $MDX file=example/main.ml -->
```ocaml
let pool_name = "ocaml-zfs"

let setup_pool handle disk =
  let () =
    if Sys.file_exists disk then ()
    else begin
      let () =
        let fd = Unix.openfile disk Unix.[ O_CREAT; O_TRUNC ] 0o644 in
        Unix.close fd
      in
      Unix.truncate disk (1024 * 1024 * 128 (* 128MB *))
    end
  in
  let leaf_vdev = [ ("type", `String "file"); ("path", `String disk) ] in
  let vdev =
    [ ("type", `String "root"); ("children", `Nvlists [ leaf_vdev ]) ]
  in
  Zfs.Zpool.create ~vdev handle pool_name

let () =
  let handle = Zfs.init () in
  let pwd = Unix.getcwd () in
  setup_pool handle (Filename.concat pwd "disk");
  Zfs.create handle (Filename.concat pool_name "dataset") Zfs.Type.filesystem
```
