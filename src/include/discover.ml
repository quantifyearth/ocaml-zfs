module C = Configurator.V1

let starts_with ~prefix s =
  try
    String.iteri
      (fun i c -> if Char.equal (String.get s i) c then () else raise Not_found)
      prefix;
    let l = String.length s in
    let o = String.length prefix in
    Some (String.sub s o (l - o))
  with Not_found -> None

let () =
  C.main ~name:"discover" (fun c ->
      let pkgconf =
        C.Pkg_config.get c |> function
        | Some p -> p
        | None -> failwith "Need pkgconfig"
      in
      match C.Pkg_config.query pkgconf ~package:"libzfs" with
      | None -> failwith "Pkgconfig failed to find libzfs"
      | Some conf ->
          C.Flags.write_sexp "c_flags.sexp" conf.cflags;
          C.Flags.write_sexp "c_library_flags.sexp" conf.libs;
          let errs, props, types =
            C.C_define.import c
              ~c_flags:("-D_GNU_SOURCE" :: conf.cflags)
              ~includes:
                [
                  "unistd.h";
                  "stdio.h";
                  "stdint.h";
                  "stdbool.h";
                  "libzfs_core.h";
                  "libzfs.h";
                ]
              C.C_define.Type.
                [
                  (* Errors *)
                  ("EZFS_SUCCESS", Int);
                  ("EZFS_NOMEM", Int);
                  ("EZFS_BADPROP", Int);
                  ("EZFS_PROPREADONLY", Int);
                  ("EZFS_PROPTYPE", Int);
                  ("EZFS_PROPNONINHERIT", Int);
                  ("EZFS_PROPSPACE", Int);
                  ("EZFS_BADTYPE", Int);
                  ("EZFS_BUSY", Int);
                  ("EZFS_EXISTS", Int);
                  ("EZFS_NOENT", Int);
                  ("EZFS_BADSTREAM", Int);
                  ("EZFS_DSREADONLY", Int);
                  ("EZFS_VOLTOOBIG", Int);
                  ("EZFS_INVALIDNAME", Int);
                  ("EZFS_BADRESTORE", Int);
                  ("EZFS_BADBACKUP", Int);
                  ("EZFS_BADTARGET", Int);
                  ("EZFS_NODEVICE", Int);
                  ("EZFS_BADDEV", Int);
                  ("EZFS_NOREPLICAS", Int);
                  ("EZFS_RESILVERING", Int);
                  ("EZFS_BADVERSION", Int);
                  ("EZFS_POOLUNAVAIL", Int);
                  ("EZFS_DEVOVERFLOW", Int);
                  ("EZFS_BADPATH", Int);
                  ("EZFS_CROSSTARGET", Int);
                  ("EZFS_ZONED", Int);
                  ("EZFS_MOUNTFAILED", Int);
                  ("EZFS_UMOUNTFAILED", Int);
                  ("EZFS_UNSHARENFSFAILED", Int);
                  ("EZFS_SHARENFSFAILED", Int);
                  ("EZFS_PERM", Int);
                  ("EZFS_NOSPC", Int);
                  ("EZFS_FAULT", Int);
                  ("EZFS_IO", Int);
                  ("EZFS_INTR", Int);
                  ("EZFS_ISSPARE", Int);
                  ("EZFS_INVALCONFIG", Int);
                  ("EZFS_RECURSIVE", Int);
                  ("EZFS_NOHISTORY", Int);
                  ("EZFS_POOLPROPS", Int);
                  ("EZFS_POOL_NOTSUP", Int);
                  ("EZFS_POOL_INVALARG", Int);
                  ("EZFS_NAMETOOLONG", Int);
                  ("EZFS_OPENFAILED", Int);
                  ("EZFS_NOCAP", Int);
                  ("EZFS_LABELFAILED", Int);
                  ("EZFS_BADWHO", Int);
                  ("EZFS_BADPERM", Int);
                  ("EZFS_BADPERMSET", Int);
                  ("EZFS_NODELEGATION", Int);
                  ("EZFS_UNSHARESMBFAILED", Int);
                  ("EZFS_SHARESMBFAILED", Int);
                  ("EZFS_BADCACHE", Int);
                  ("EZFS_ISL2CACHE", Int);
                  ("EZFS_VDEVNOTSUP", Int);
                  ("EZFS_NOTSUP", Int);
                  ("EZFS_ACTIVE_SPARE", Int);
                  ("EZFS_UNPLAYED_LOGS", Int);
                  ("EZFS_REFTAG_RELE", Int);
                  ("EZFS_REFTAG_HOLD", Int);
                  ("EZFS_TAGTOOLONG", Int);
                  ("EZFS_PIPEFAILED", Int);
                  ("EZFS_THREADCREATEFAILED", Int);
                  ("EZFS_POSTSPLIT_ONLINE", Int);
                  ("EZFS_SCRUBBING", Int);
                  ("EZFS_ERRORSCRUBBING", Int);
                  ("EZFS_ERRORSCRUB_PAUSED", Int);
                  ("EZFS_NO_SCRUB", Int);
                  ("EZFS_DIFF", Int);
                  ("EZFS_DIFFDATA", Int);
                  ("EZFS_POOLREADONLY", Int);
                  ("EZFS_SCRUB_PAUSED", Int);
                  ("EZFS_SCRUB_PAUSED_TO_CANCEL", Int);
                  ("EZFS_ACTIVE_POOL", Int);
                  ("EZFS_CRYPTOFAILED", Int);
                  ("EZFS_NO_PENDING", Int);
                  ("EZFS_CHECKPOINT_EXISTS", Int);
                  ("EZFS_DISCARDING_CHECKPOINT", Int);
                  ("EZFS_NO_CHECKPOINT", Int);
                  ("EZFS_DEVRM_IN_PROGRESS", Int);
                  ("EZFS_VDEV_TOO_BIG", Int);
                  ("EZFS_IOC_NOTSUPPORTED", Int);
                  ("EZFS_TOOMANY", Int);
                  ("EZFS_INITIALIZING", Int);
                  ("EZFS_NO_INITIALIZE", Int);
                  ("EZFS_WRONG_PARENT", Int);
                  ("EZFS_TRIMMING", Int);
                  ("EZFS_NO_TRIM", Int);
                  ("EZFS_TRIM_NOTSUP", Int);
                  ("EZFS_NO_RESILVER_DEFER", Int);
                  ("EZFS_EXPORT_IN_PROGRESS", Int);
                  ("EZFS_REBUILDING", Int);
                  ("EZFS_VDEV_NOTSUP", Int);
                  ("EZFS_NOT_USER_NAMESPACE", Int);
                  ("EZFS_CKSUM", Int);
                  ("EZFS_RESUME_EXISTS", Int);
                  ("EZFS_SHAREFAILED", Int);
                  ("EZFS_UNKNOWN", Int);
                  (* Properties *)
                  ("ZFS_PROP_CREATION", Int);
                  ("ZFS_PROP_USED", Int);
                  ("ZFS_PROP_AVAILABLE", Int);
                  ("ZFS_PROP_REFERENCED", Int);
                  ("ZFS_PROP_COMPRESSRATIO", Int);
                  ("ZFS_PROP_COMPRESSION", Int);
                  ("ZFS_PROP_SNAPDIR", Int);
                  ("ZFS_PROP_ENCRYPTION", Int);
                  (* Types *)
                  ("ZFS_TYPE_INVALID", Int);
                  ("ZFS_TYPE_FILESYSTEM", Int);
                  ("ZFS_TYPE_SNAPSHOT", Int);
                  ("ZFS_TYPE_VOLUME", Int);
                  ("ZFS_TYPE_POOL", Int);
                  ("ZFS_TYPE_BOOKMARK", Int);
                  ("ZFS_TYPE_VDEV", Int);
                  ("ZFS_TYPE_DATASET", Int);
                ]
            |> List.fold_left
                 (fun (errs, props, types) -> function
                   | name, C.C_define.Value.Int v -> (
                       let type_ name =
                         Printf.sprintf "val %s : t"
                           (String.lowercase_ascii name)
                       in
                       let definition name =
                         Printf.sprintf "let %s : t = 0x%x"
                           (String.lowercase_ascii name)
                           v
                       in
                       match starts_with ~prefix:"EZFS_" name with
                       | Some r ->
                           ((type_ r, definition r) :: errs, props, types)
                       | None -> (
                           match starts_with ~prefix:"ZFS_PROP_" name with
                           | Some r ->
                               (errs, (type_ r, definition r) :: props, types)
                           | None -> (
                               match starts_with ~prefix:"ZFS_TYPE_" name with
                               | Some t ->
                                   ( errs,
                                     props,
                                     (type_ t, definition t) :: types )
                               | None -> failwith "Unknown ZFS static value")))
                   | _ -> assert false)
                 ([], [], [])
          in
          let with_module ~name defs =
            [ Printf.sprintf "module %s = struct\n type t =  int\n" name ]
            @ defs @ [ "end" ]
          in
          let with_module_type ~name defs =
            [
              Printf.sprintf "module type %s = sig\n type t = private int\n"
                name;
            ]
            @ defs @ [ "end" ]
          in
          let defs =
            with_module ~name:"Error" (List.map snd errs)
            @ with_module ~name:"Props" (List.map snd props)
            @ with_module ~name:"Types" (List.map snd types)
          in
          let types =
            with_module_type ~name:"ERROR" (List.map fst errs)
            @ with_module_type ~name:"PROPS" (List.map fst props)
            @ with_module_type ~name:"TYPES" (List.map fst types)
          in
          C.Flags.write_lines "config.ml" (defs @ types))
