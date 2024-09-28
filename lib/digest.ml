let sha256sum_file_by_name fn =
  let digest = Sha256.file_fast fn in
  Sha256.to_hex digest




