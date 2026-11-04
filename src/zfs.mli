(** {! Zfs} *)

module Type : sig
  type t = private int

  val empty : int
  val of_int : 'a -> 'a
  val ( + ) : int -> int -> int
  val mem : int -> int -> bool
  val vdev : t
  val pool : t
  val volume : t
  val invalid : t
  val bookmark : t
  val snapshot : t
  val filesystem : t
  val dataset : t
end

module Handle : sig
  type t
  (** An instance handle for the ZFS library *)
end

val init : unit -> Handle.t
(** Initialise the library *)

val debug : Handle.t -> bool -> unit
(** Enable/disable printing on error from ZFS *)

val errno : Handle.t -> int
(** Check for errors on the handle *)

module Nvlist : sig
  type t
  (** Generic name-value lists used by ZFS *)

  type nvlist =
    (string
    * [ `Bool of bool
      | `Byte of Unsigned.uchar
      | `String of string
      | `UInt64 of int64
      | `Nvlist of nvlist
      | `Nvlists of nvlist list ])
    list
  (** A partial OCaml representation of an NV list *)

  val v : nvlist -> t
  (** Convert the OCaml representation to the C representation *)
end

module Zpool : sig
  type t
  (** A Zpool handle *)

  val create :
    ?vdev:Nvlist.nvlist ->
    ?props:Nvlist.nvlist ->
    ?fs_props:Nvlist.nvlist ->
    Handle.t ->
    string ->
    unit
  (** [create ?vdev ?props ?fs_props handle pool_name] will try to create a new
      pool called [pool_name]. *)

  val open_ : Handle.t -> string -> t
  (** Open a Zpool *)

  val close : t -> unit
  (** Close an open Zpool *)

  val get_name : t -> string
  (** The name of an open Zpool *)
end

type t
(** A ZFS Dataset *)

val create_ancestors : Handle.t -> string -> unit
(** Often called before {! create}, acts similarly to [mkdir -p]. *)

val create : ?props:Nvlist.nvlist -> Handle.t -> string -> Type.t -> unit
(** Create a new ZFS dataset. *)

val open_ : Handle.t -> string -> Type.t -> t
(** Open an existing ZFS dataset. *)

val close : t -> unit
(** Close a dataset. *)

val destroy : ?defer:bool -> t -> unit
(** [destroy ~defer d] destroys the dataset [d]. By default the dataset is not
    deferred for destruction, but a user can request that the kernel destroy the
    dataset when the last reference to it is released. *)

val exists : Handle.t -> string -> Type.t -> bool
(** Check if a dataset of a specific type exists *)

val is_mounted : Handle.t -> string -> string option
(** [is_mounted h d = None] if [d] is not mounted, otherwise
    [is_mounted h d = Some mountpoint]. *)

val mount : ?mount_opts:string -> ?mount_flags:int -> t -> unit
(** Mount a dataset *)

val unmount : ?mount_opts:string -> ?mount_flags:int -> t -> unit
(** Unmount a dataset *)

val get_type : t -> Type.t
(** Get the type of the dataset *)

val clone : ?options:Nvlist.t -> t -> string -> unit
(** Clone an open dataset *)

val snapshot : ?options:Nvlist.t -> Handle.t -> string -> bool -> unit
(** Snapshot a dataset *)

val show_diff : ?to_:string -> t -> from_:string -> Unix.file_descr -> unit
(** Output diff to the file descriptor *)

module Error : sig
  type kind =
    | Unknown
    | Sharefailed
    | Resume_exists
    | Cksum
    | Not_user_namespace
    | Vdev_notsup
    | Rebuilding
    | Export_in_progress
    | No_resilver_defer
    | Trim_notsup
    | No_trim
    | Trimming
    | Wrong_parent
    | No_initialize
    | Initializing
    | Toomany
    | Ioc_notsupported
    | Vdev_too_big
    | Devrm_in_progress
    | No_checkpoint
    | Discarding_checkpoint
    | Checkpoint_exists
    | No_pending
    | Cryptofailed
    | Active_pool
    | Scrub_paused_to_cancel
    | Scrub_paused
    | Poolreadonly
    | Diffdata
    | Diff
    | No_scrub
    | Errorscrub_paused
    | Errorscrubbing
    | Scrubbing
    | Postsplit_online
    | Threadcreatefailed
    | Pipefailed
    | Tagtoolong
    | Reftag_hold
    | Reftag_rele
    | Unplayed_logs
    | Active_spare
    | Notsup
    | Vdevnotsup
    | Isl2cache
    | Badcache
    | Sharesmbfailed
    | Unsharesmbfailed
    | Nodelegation
    | Badpermset
    | Badperm
    | Badwho
    | Labelfailed
    | Nocap
    | Openfailed
    | Nametoolong
    | Pool_invalarg
    | Pool_notsup
    | Poolprops
    | Nohistory
    | Recursive
    | Invalconfig
    | Isspare
    | Intr
    | Io
    | Fault
    | Nospc
    | Perm
    | Sharenfsfailed
    | Unsharenfsfailed
    | Umountfailed
    | Mountfailed
    | Zoned
    | Crosstarget
    | Badpath
    | Devoverflow
    | Poolunavail
    | Badversion
    | Resilvering
    | Noreplicas
    | Baddev
    | Nodevice
    | Badtarget
    | Badbackup
    | Badrestore
    | Invalidname
    | Voltoobig
    | Dsreadonly
    | Badstream
    | Noent
    | Exists
    | Busy
    | Badtype
    | Propspace
    | Propnoninherit
    | Proptype
    | Propreadonly
    | Badprop
    | Nomem
    | Success
    | Unrecognised of int

  exception Zfs_error of (kind * string)
end
