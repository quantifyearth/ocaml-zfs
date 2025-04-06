open Ctypes

(* This Types_generated module is an instantiation of the Types
   functor defined in the type_description.ml file. It's generated by
   a C program that Dune creates and runs behind the scenes. *)
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let init = foreign "libzfs_init" (void @-> returning Types.libzfs_handle_t)
  let errno = foreign "libzfs_errno" (Types.libzfs_handle_t @-> returning int)

  let debug =
    foreign "libzfs_print_on_error"
      (Types.libzfs_handle_t @-> bool @-> returning void)

  module Zpool = struct
    let open_ =
      foreign "zpool_open"
        (Types.libzfs_handle_t @-> string @-> returning Types.zpool_handle_t)

    let close = foreign "zpool_close" (Types.zpool_handle_t @-> returning void)

    let get_name =
      foreign "zpool_get_name" (Types.zpool_handle_t @-> returning string)

    let get_state =
      foreign "zpool_get_state" (Types.zpool_handle_t @-> returning int)
  end

  let create_ancestors =
    foreign "zfs_create_ancestors"
      (Types.libzfs_handle_t @-> string @-> returning int)

  let create =
    foreign "zfs_create"
      (Types.libzfs_handle_t @-> string @-> int @-> ptr Types.nvlist_t
     @-> returning int)

  let open_ =
    foreign "zfs_open"
      (Types.libzfs_handle_t @-> string @-> int @-> returning Types.zfs_handle_t)

  let mount =
    foreign "zfs_mount" (Types.zfs_handle_t @-> string @-> int @-> returning int)

  let unmount =
    foreign "zfs_unmount"
      (Types.zfs_handle_t @-> string @-> int @-> returning int)

  let close = foreign "zfs_close" (Types.zfs_handle_t @-> returning void)
  let get_type = foreign "zfs_get_type" (Types.zfs_handle_t @-> returning int)

  module Nvlist = struct
    let alloc =
      foreign "nvlist_alloc"
        (ptr (ptr Types.nvlist_t) @-> int @-> int @-> returning int)

    let free = foreign "nvlist_free" (ptr Types.nvlist_t @-> returning void)

    let add_bool =
      foreign "nvlist_add_boolean_value"
        (ptr Types.nvlist_t @-> string @-> bool @-> returning int)

    let add_string =
      foreign "nvlist_add_string"
        (ptr Types.nvlist_t @-> string @-> string @-> returning int)

    let add_byte =
      foreign "nvlist_add_byte"
        (ptr Types.nvlist_t @-> string @-> uchar @-> returning int)

    let add_int64 =
      foreign "nvlist_add_int64"
        (ptr Types.nvlist_t @-> string @-> int64_t @-> returning int)
  end

  let clone =
    foreign "zfs_clone"
      (Types.zfs_handle_t @-> string @-> ptr Types.nvlist_t @-> returning int)

  let snapshot =
    foreign "zfs_snapshot"
      (Types.libzfs_handle_t @-> string @-> bool @-> ptr Types.nvlist_t
     @-> returning int)

  let exists =
    foreign "zfs_dataset_exists"
      (Types.libzfs_handle_t @-> string @-> int @-> returning bool)

  let is_mounted =
    foreign "is_mounted"
      (Types.libzfs_handle_t @-> string @-> ptr string @-> returning bool)

  let diff =
    foreign "zfs_show_diffs"
      (Types.zfs_handle_t @-> int @-> string @-> string_opt @-> int
     @-> returning int)
end
