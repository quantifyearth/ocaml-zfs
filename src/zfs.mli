module Types : sig
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

module Zpool : sig
  type t
  (** A Zpool handle *)

  val open_ : Handle.t -> string -> t
  (** Open a Zpool *)

  val close : t -> unit
  (** Close an open Zpool *)

  val get_name : t -> string
  (** The name of an open Zpool *)
end

module Nvlist : sig
  type t
  (** Generic name-value lists used by ZFS *)

  type nvlist =
    (string
    * [ `Bool of bool
      | `Byte of Unsigned.uchar
      | `String of string
      | `Int64 of int64 ])
    list
  (** A partial OCaml representation of an NV list *)

  val v : nvlist -> t
  (** Convert the OCaml representation to the C representation *)
end

type t
(** A ZFS Dataset *)

val create_ancestors : Handle.t -> string -> unit
(** Often called before {! create} *)

val create : ?props:Nvlist.nvlist -> Handle.t -> string -> Types.t -> unit
(** Create a new ZFS dataset *)

val open_ : Handle.t -> string -> Types.t -> t
(** Open an existing ZFS dataset *)

val close : t -> unit
(** Close a dataset *)

val exists : Handle.t -> string -> Types.t -> bool
(** Check if a dataset of a specific type exists *)

val is_mounted : Handle.t -> string -> string option
(** [is_mounted h d = None] if [d] is not mounted, otherwise
    [is_mounted h d = Some mountpoint]. *)

val mount : ?mount_opts:string -> ?mount_flags:int -> t -> unit
(** Mount a dataset *)

val unmount : ?mount_opts:string -> ?mount_flags:int -> t -> unit
(** Unmount a dataset *)

val get_type : t -> Types.t
(** Get the type of the dataset *)

val clone : ?options:Nvlist.t -> t -> string -> unit
(** Clone an open dataset *)

val snapshot : ?options:Nvlist.t -> Handle.t -> string -> bool -> unit
(** Snapshot a dataset *)

val show_diff : ?to_:string -> t -> from_:string -> Unix.file_descr -> unit
(** Output diff to the file descriptor *)

module Error : sig
  type t = int

  val unknown : t
  val sharefailed : t
  val resume_exists : t
  val cksum : t
  val not_user_namespace : t
  val vdev_notsup : t
  val rebuilding : t
  val export_in_progress : t
  val no_resilver_defer : t
  val trim_notsup : t
  val no_trim : t
  val trimming : t
  val wrong_parent : t
  val no_initialize : t
  val initializing : t
  val toomany : t
  val ioc_notsupported : t
  val vdev_too_big : t
  val devrm_in_progress : t
  val no_checkpoint : t
  val discarding_checkpoint : t
  val checkpoint_exists : t
  val no_pending : t
  val cryptofailed : t
  val active_pool : t
  val scrub_paused_to_cancel : t
  val scrub_paused : t
  val poolreadonly : t
  val diffdata : t
  val diff : t
  val no_scrub : t
  val errorscrub_paused : t
  val errorscrubbing : t
  val scrubbing : t
  val postsplit_online : t
  val threadcreatefailed : t
  val pipefailed : t
  val tagtoolong : t
  val reftag_hold : t
  val reftag_rele : t
  val unplayed_logs : t
  val active_spare : t
  val notsup : t
  val vdevnotsup : t
  val isl2cache : t
  val badcache : t
  val sharesmbfailed : t
  val unsharesmbfailed : t
  val nodelegation : t
  val badpermset : t
  val badperm : t
  val badwho : t
  val labelfailed : t
  val nocap : t
  val openfailed : t
  val nametoolong : t
  val pool_invalarg : t
  val pool_notsup : t
  val poolprops : t
  val nohistory : t
  val recursive : t
  val invalconfig : t
  val isspare : t
  val intr : t
  val io : t
  val fault : t
  val nospc : t
  val perm : t
  val sharenfsfailed : t
  val unsharenfsfailed : t
  val umountfailed : t
  val mountfailed : t
  val zoned : t
  val crosstarget : t
  val badpath : t
  val devoverflow : t
  val poolunavail : t
  val badversion : t
  val resilvering : t
  val noreplicas : t
  val baddev : t
  val nodevice : t
  val badtarget : t
  val badbackup : t
  val badrestore : t
  val invalidname : t
  val voltoobig : t
  val dsreadonly : t
  val badstream : t
  val noent : t
  val exists : t
  val busy : t
  val badtype : t
  val propspace : t
  val propnoninherit : t
  val proptype : t
  val propreadonly : t
  val badprop : t
  val nomem : t
  val success : t
end
