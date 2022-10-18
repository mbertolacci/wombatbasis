#' @export
geoschem_run_configuration <- function(
  main,
  history,
  hemco,
  hemco_diagnostics,
  code_location,
  makefile = 'default',
  get_run_info = 'default',
  extra_files = list(),
  extra_directories = c(),
  symlinks = list()
) {
  files <- c(list(), extra_files)

  add_file_or_default <- function(filename, contents, mode) {
    if (!is.null(contents)) {
      if (contents == 'default') {
        contents <- paste0(readLines(system.file(
          'extdata',
          filename,
          package = 'wombatbasis'
        )), collapse = '\n')
      }
      if (!missing(mode)) {
        attr(contents, 'mode') <- mode
      }
      files[[filename]] <<- contents
    }
  }

  add_file_or_default('Makefile', makefile)
  add_file_or_default('getRunInfo', get_run_info, '0755')

  if (!missing(code_location)) {
    symlinks[['CodeDir']] <- code_location
  }

  structure(list(
    main = main,
    history = history,
    hemco = hemco,
    hemco_diagnostics = hemco_diagnostics,
    files = files,
    directories = extra_directories,
    symlinks = symlinks
  ), class = 'geoschem_run_configuration')
}

#' @export
print.geoschem_run_configuration <- function(x, ...) {
  cat(sprintf(
    '==================================================
input.geos
==================================================
%s

==================================================
HISTORY.rc
==================================================
%s

==================================================
HEMCO_Config.rc
==================================================
%s

==================================================
HEMCO_Diagn.rc
==================================================
%s',
    as.character(x$main),
    as.character(x$history),
    as.character(x$hemco),
    as.character(x$hemco_diagnostics)
  ))
}

#' @export
write_geoschem_run <- function(run_configuration, run_directory) {
  if (dir.exists(run_directory)) {
    warning('Run directory already exists')
  }

  create_dirname <- function(path) {
    dir.create(
      file.path(run_directory, dirname(path)),
      recursive = TRUE,
      showWarnings = FALSE,
      mode = '0755'
    )
  }

  files <- run_configuration$files
  files[['input.geos']] <- as.character(run_configuration$main)
  files[['HISTORY.rc']] <- as.character(run_configuration$history)
  files[['HEMCO_Config.rc']] <- as.character(run_configuration$hemco)
  files[['HEMCO_Diagn.rc']] <- as.character(run_configuration$hemco_diagnostics)

  for (path in names(files)) {
    create_dirname(path)
    if (is.character(files[[path]])) {
      cat(files[[path]], file = file.path(run_directory, path))
      if ('mode' %in% names(attributes(files[[path]]))) {
        Sys.chmod(file.path(run_directory, path), attr(files[[path]], 'mode'))
      }
    } else if (is(files[[path]], 'grid_data')) {
      write_nc_grid_data(files[[path]], file.path(run_directory, path))
    }
  }

  for (directory_name in run_configuration$directories) {
    dir.create(
      file.path(run_directory, directory_name),
      recursive = TRUE,
      showWarnings = FALSE,
      mode = '0755'
    )
  }

  symlinks <- run_configuration$symlinks
  for (path in names(symlinks)) {
    create_dirname(path)
    unlink(file.path(run_directory, path))
    file.symlink(symlinks[[path]], file.path(run_directory, path))
  }

  invisible(NULL)
}
