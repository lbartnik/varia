# Client:
#
# 1. create inputs_dir and outputs_dir
# 2. send object & tag files to inputs_dir (first send tag file, then
#    object file with _part prefix, finally rename object removing _part)
# 3. read results from outputs_dir (matching .rds$)
#
#
# Server:
#
# 1. read object file (matching ^[^_].rds$) - if a file exists it means
#    that is has been fully uploaded and the tags exist there too
# 2. write results to outputs_dir with a _part prefix (in case writing RDS
#    is not an atomic operation and another process might start reading
#    the file before writing is finished)
# 3. rename results removing _part


#' @export
#' @importFrom parallel mclapply
start_remote <- function (package, inputs_dir, outputs_dir, cores = getOption('cores', 1)) {
  stopifnot(is_package(package))
  stopifnot(is_dir(inputs_dir))
  stopifnot(is_dir(outputs_dir))
  
  mclapply(seq(cores), remote_srv_thread, package = package,
           inputs_dir = inputs_dir, outputs_dir = outputs_dir,
           mc.cores = cores)
}



# --- server implementation --------------------------------------------

# sleep for a moment, increate sleeping time if called repeatedly without
# processing data
Sleeper <- setRefClass('Sleeper',
  fields  = c(was_processed = 'logical', sleep_interval = 'numeric'),
  methods = list(
    initialize = function () {
      was_processed <<- TRUE
      sleep_interval <<- .5
    },
    processed = function () {
      initialize()
    },
    sleep = function () {
      if (!was_processed) {
        sleep_interval <<- min(sleep_interval * 2, 5)
        Sys.sleep(sleep_interval)
      }
      else {
        was_processed <<- FALSE
      }
    }
  )
)


acquire_task <- function (inputs_dir) {
  new_tasks <- list.files(inputs_dir, '^[^_]+.rds$', full.names = T) # not a tag file
  if (!length(new_tasks)) NULL
  
  # try to rename one of these files
  for (task_file in new_tasks) {
    owned_file <- paste0(task_file, '_owned')
    if (suppressWarnings(file.rename(task_file, owned_file)))
      return(file_path_sans_ext(basename(task_file)))
  }
  NULL
}


#' @importFrom tools file_path_sans_ext
remote_srv_thread <- function (i, package, inputs_dir, outputs_dir, .iterations = Inf) {
  # prepare the execution environment
  e <- new.env(parent = globalenv())
  load_pkg_to_env(package, e)
  
  s <- Sleeper$new()
  while (.iterations) {
    s$sleep()
    .iterations <- .iterations - 1
    
    # find a task
    id  <- acquire_task(inputs_dir)
    if (is.null(id)) next
    
    obj <- file.path(inputs_dir, paste0(id, '.rds_owned'))
    tgs <- file.path(inputs_dir, id)
    res <- file.path(outputs_dir, paste0(id, '.rds'))
    
    # process data
    ans <- run_entry(e, list(readRDS(obj), read_tags(tgs)))
    
    saveRDS(ans, paste0(res, '_part'))
    file.rename(paste0(res, '_part'), res)
    
    # set flag
    s$processed()
  }
}

