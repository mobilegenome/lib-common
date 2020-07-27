conditional_RDS <- function(obj, fpath, FUN) {

  if (file.exists(fpath)) {
    obj_from_disk <- readRDS(fpath)
    return(obj_from_disk)
  } else {
    obj <- eval(FUN)
    dir.create(dirname(fpath), recursive = T)
    saveRDS(obj, fpath)
    return(obj)
  }
}

read_via_SSH <- function(path, host, FUN) {
  ssh_connection <- paste('ssh', host, '  cat', path, sep=" ")
  response <- FUN(pipe(ssh_connection))
  return(response)
}
