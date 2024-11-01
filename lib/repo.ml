module Make : Fileset.S = struct
  module Backup = Backup.Make

  type t = {
    latest_backup : Backup.t;
    old_backups : Backup.t list;
  }

  let empty dir fs =
    {
      latest_backup = Backup.empty dir fs;
      old_backups = [];
    }

  let has path x =
    List.exists
      (Backup.has path)
      (x.latest_backup :: x.old_backups)

  let find_copy f x =
    List.find_map
      (Backup.find_copy f)
      (x.latest_backup :: x.old_backups)

  let apply_convert f x =
    (** [apply_convert] calls the given function
     * on the latest_backup in x and promotes the
     * Backup.t Exnlogger to a Repo.t Exnlogger.
     * It seems like there should be a more
     * idiomatic way to do this but I haven't found 
     * it yet
     *)
    let open Exnlogger in
    let result = f x.latest_backup in
    let x' =
      return { x with latest_backup = get result }
    in
    { x' with exns = get_exns result }

  let add path x = apply_convert (Backup.add path) x
  let close x = apply_convert Backup.close x
end
