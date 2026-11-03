module Error = struct
  include Config.Errors

  exception Zfs_error of (kind * string)

  let raise ?(msg = "") handle res =
    if res = 0 then ()
    else
      let err_msg = C.Functions.err_description handle in
      let msg = if err_msg = "" then msg else err_msg in
      let n = C.Functions.errno handle in
      raise (Zfs_error (kind_of_int n, msg))

  let () =
    Printexc.register_printer (function
      | Zfs_error (k, "") ->
          Some (Printf.sprintf "Zfs_error(%s)" (kind_to_string k))
      | Zfs_error (k, msg) ->
          Some (Printf.sprintf "Zfs_error(%s, \"%s\")" (kind_to_string k) msg)
      | _ -> None)
end

module Flags = struct
  type t = int

  let empty = 0
  let of_int x = x
  let ( + ) = ( lor )
  let mem a b = a land b = a
end

module Types = struct
  include Flags

  let vdev = Config.Types.vdev
  let pool = Config.Types.pool
  let volume = Config.Types.volume
  let invalid = Config.Types.invalid
  let bookmark = Config.Types.bookmark
  let snapshot = Config.Types.snapshot
  let filesystem = Config.Types.filesystem
  let dataset = Config.Types.dataset
end

module Handle = struct
  type t = C.Types.libzfs_handle_t Ctypes_static.structure Ctypes_static.ptr
end

let init : unit -> Handle.t = C.Functions.init
let debug : Handle.t -> bool -> unit = C.Functions.debug
let errno : Handle.t -> int = C.Functions.errno

module Zpool = struct
  type t = C.Types.zpool_handle_t Ctypes_static.structure Ctypes_static.ptr

  let open_ = C.Functions.Zpool.open_
  let close = C.Functions.Zpool.close
  let get_name = C.Functions.Zpool.get_name
end

module Nvlist = struct
  type t = C.Types.nvlist_t Ctypes_static.structure Ctypes_static.ptr

  type nvlist =
    (string
    * [ `Bool of bool
      | `String of string
      | `Byte of Unsigned.uchar
      | `Int64 of int64 ])
    list

  let check_return i =
    if i = 22 then invalid_arg "Nvlist.v: add bool" else assert (i = 0)

  let v (schema : nvlist) : t =
    let open Ctypes in
    let finalise v = C.Functions.Nvlist.free !@v in
    let nv_pp =
      allocate ~finalise (ptr C.Types.nvlist_t)
        (from_voidp C.Types.nvlist_t null)
    in
    (* TODO: Unique names or not... *)
    C.Functions.Nvlist.alloc nv_pp 0x1 0 |> check_return;
    let rec aux = function
      | [] -> !@nv_pp
      | (k, `Bool b) :: rest ->
          C.Functions.Nvlist.add_bool !@nv_pp k b |> check_return;
          aux rest
      | (k, `String s) :: rest ->
          C.Functions.Nvlist.add_string !@nv_pp k s |> check_return;
          aux rest
      | (k, `Int64 i64) :: rest ->
          C.Functions.Nvlist.add_int64 !@nv_pp k i64 |> check_return;
          aux rest
      | (k, `Byte u) :: rest ->
          C.Functions.Nvlist.add_byte !@nv_pp k u |> check_return;
          aux rest
      | _ -> assert false
    in
    aux schema

  let empty = Ctypes.(coerce (ptr void) (ptr C.Types.nvlist_t) null)
end

type t = C.Types.zfs_handle_t Ctypes_static.structure Ctypes_static.ptr

let create_ancestors handle path =
  let i = C.Functions.create_ancestors handle path in
  Error.raise ~msg:"create_ancestors" handle i

let create ?(props = []) handle path (type_ : Types.t) =
  let i = C.Functions.create handle path type_ (Nvlist.v props) in
  Error.raise ~msg:"create" handle i

let open_ handle path (type_ : Types.t) = C.Functions.open_ handle path type_
let close : t -> unit = C.Functions.close
let get_type : t -> Types.t = C.Functions.get_type

let clone ?(options = Nvlist.empty) handle path =
  let res = C.Functions.clone handle path options in
  Error.raise ~msg:"clone" (C.Functions.get_handle handle) res

let snapshot ?(options = Nvlist.empty) handle path b =
  let res = C.Functions.snapshot handle path b options in
  Error.raise ~msg:"snapshot" handle res

let exists handle path (type_ : Types.t) = C.Functions.exists handle path type_

let is_mounted handle path =
  let where =
    Ctypes.allocate
      Ctypes.(ptr char)
      Ctypes.(coerce (ptr void) (ptr char) Ctypes.null)
  in
  let v = C.Functions.is_mounted handle path where in
  if not v then None
  else
    let c = Ctypes.( !@ ) where in
    Some Ctypes.(coerce (ptr char) string c)

let null_string = Ctypes.(coerce (ptr void) (ptr char) null)

let mount ?mount_opts ?(mount_flags = 0) dataset =
  let opts =
    Option.value
      ~default:(Ctypes.string_from_ptr null_string ~length:0)
      mount_opts
  in
  let res = C.Functions.mount dataset opts mount_flags in
  Error.raise ~msg:"mount" (C.Functions.get_handle dataset) res

let unmount ?mount_opts ?(mount_flags = 0) dataset =
  let opts =
    Option.value
      ~default:(Ctypes.string_from_ptr null_string ~length:0)
      mount_opts
  in
  let res = C.Functions.unmount dataset opts mount_flags in
  Error.raise ~msg:"unmount" (C.Functions.get_handle dataset) res

let show_diff ?to_ handle ~from_ (fd : Unix.file_descr) =
  (* TODO: Other Diff Flags https://github.com/openzfs/zfs/blob/5b0c27cd14bbc07d50304c97735cc105d0258673/include/libzfs.h#L917? *)
  let res = C.Functions.diff handle (Obj.magic fd : int) from_ to_ 1 in
  Error.raise ~msg:"show_diff" (C.Functions.get_handle handle) res
