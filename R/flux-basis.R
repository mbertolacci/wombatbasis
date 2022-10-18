#' @export
flux_basis <- function(
  structure,
  base_run,
  factor_grids
) {
  variables <- attr(terms(structure), 'variables')

  factor_grids <- lapply(factor_grids, function(x) {
    if (is.null(names(x))) names(x) <- as.character(x)
    x
  })

  base_run_fields <- sapply(base_run$hemco$base_emissions, getElement, 'name')

  part_number <- 1
  basis_parts <- lapply(
    variables[2 : length(variables)],
    function(variable) {
      parts <- .split_bars(variable)

      if (length(parts) >= 3) {
        name <- parts[[3]]
      } else {
        name <- sprintf('part%02d', part_number)
        part_number <<- part_number + 1
      }

      fields <- parts[[1]]
      stopifnot(all(fields %in% base_run_fields))

      list(
        name = name,
        fields = fields,
        factor_grid_names = if (length(parts) >= 2) parts[[2]] else NULL
      )
    }
  )

  part_names <- sapply(basis_parts, getElement, 'name')
  stopifnot(length(unique(part_names)) == length(part_names))

  basis_functions <- do.call(c, lapply(basis_parts, function(basis_part) {
    if (length(basis_part$factor_grid_names) == 0) {
      return(list(
        part_name = basis_part$name,
        basis_function_name = basis_part$name,
        factor_grids = NULL,
        fields = basis_part$fields
      ))
    }

    factor_grid_combinations <- as.matrix(expand.grid(lapply(
      factor_grids,
      names
    )[basis_part$factor_grid_names]))

    lapply(seq_len(nrow(factor_grid_combinations)), function(i) {
      factor_grid_combination_i <- factor_grid_combinations[i, ]

      factor_grids <- lapply(names(factor_grid_combination_i), function(name) {
        factor_grids[[name]][[
          factor_grid_combination_i[name]
        ]]
      })

      scaling_grid <- .truncate_grid_times(
        multiply_grids(factor_grids),
        start = TRUE,
        end = FALSE
      )

      list(
        name = sprintf(
          '%s_%s',
          basis_part$name,
          paste0(sprintf(
            '%s%s',
            names(factor_grid_combination_i),
            factor_grid_combination_i
          ), collapse = '_')
        ),
        part = basis_part$name,
        scaling_grid = scaling_grid,
        fields = basis_part$fields
      )
    })
  }))

  structure(list(
    structure = structure,
    basis_parts = basis_parts,
    base_run = base_run,
    factor_grids = factor_grids,
    basis_functions = basis_functions
  ), class = 'flux_basis')
}

.split_bars <- function(variable) {
  if (is.name(variable)) {
    list(as.character(variable))
  } else if (variable[[1]] == as.name('|')) {
    c(.split_bars(variable[[2]]), list(all.vars(variable[[3]])))
  } else {
    list(all.vars(variable))
  }
}

#' @export
basis_to_runs <- function(
  basis,
  strategy = c('one_to_one', 'passive_species'),
  max_run_length,
  postprocess_run,
  postprocess_split_run,
  ...
) {
  strategy <- match.arg(strategy)

  runs <- list(
    'one_to_one' = .basis_to_runs_one_to_one,
    'passive_species' = .basis_to_runs_passive_species
  )[[strategy]](basis, ...)

  if (!missing(postprocess_run)) {
    runs <- lapply(runs, postprocess_run)
  }

  if (!missing(max_run_length)) {
    # NOTE(mgnb): the base run is excluded from the split
    runs <- c(
      runs[1],
      do.call(c, lapply(
        runs[2 : length(runs)],
        .split_run_by_period,
        max_run_length
      ))
    )
  }

  if (!missing(postprocess_split_run)) {
    runs <- lapply(runs, postprocess_split_run)
  }

  runs
}

#' @export
write_basis_runs <- function(basis_runs, base_directory, optimise = TRUE) {
  for (basis_run in basis_runs) {
    logger::log_debug('Writing {basis_run$name}')
    path <- file.path(base_directory, basis_run$name)
    if (optimise) {
      basis_run$configuration$hemco$base_emissions <- optimise_base_emissions(
        basis_run$configuration$hemco$base_emissions
      )
    }
    write_geoschem_run(basis_run$configuration, path)
  }
  write.csv(
    unique(do.call(rbind, lapply(basis_runs, getElement, 'mapping'))),
    file.path(base_directory, 'mapping.csv'),
    row.names = FALSE
  )
}

.basis_function_start_times <- function(basis) {
  output <- NULL
  for (basis_function in basis$basis_functions) {
    has_time_grid <- 'time' %in% names(dimnames(
      basis_function$scaling_grid
    ))
    value <- if (has_time_grid) {
      min(attr(
        basis_function$scaling_grid,
        'time'
      ))
    } else {
      basis$base_run$main$simulation_menu$start
    }
    if (is.null(output)) {
      output <- value
    } else {
      output <- c(output, value)
    }
  }
  output
}

.times_to_hemco_source_time <- function(x) {
  minutes <- lubridate::minute(x)
  if (any(minutes != 0)) {
    '*'
  } else {
    years <- lubridate::year(x)
    months_vary <- any(lubridate::month(x) != 1)
    days_vary <- any(lubridate::day(x) != 1)
    hours_vary <- any(lubridate::hour(x) != 0)
    sprintf(
      '%s/%s/%s/%s',
      if (min(years) == max(years)) {
        min(years)
      } else {
        sprintf('%d-%d', min(years), max(years))
      },
      if (months_vary || days_vary || hours_vary) '1-12' else '1',
      if (days_vary || hours_vary) '1-31' else '1',
      if (hours_vary) '0-23' else '0'
    )
  }
}

.split_run_by_period <- function(
  run,
  max_length = lubridate::period(1, 'years')
) {
  start_date <- run$configuration$main$simulation_menu$start
  end_date <- run$configuration$main$simulation_menu$end

  if (start_date + max_length >= end_date) {
    return(list(run))
  }

  current_run <- run
  current_start_date <- start_date
  index <- 1L
  runs <- NULL
  while (current_start_date < end_date) {
    current_run$configuration$main$simulation_menu$start <- current_start_date
    current_run$configuration$main$simulation_menu$end <- min(
      end_date,
      current_start_date + max_length
    )
    previous_run_name <- NULL
    if (index > 1L) {
      previous_run_name <- current_run$name

      current_run$configuration$symlinks[
        startsWith(
          names(current_run$configuration$symlinks),
          'GEOSChem.Restart.'
        )
      ] <- NULL
      current_run$configuration$files[
        startsWith(
          names(current_run$configuration$files),
          'GEOSChem.Restart.'
        )
      ] <- NULL
      restart_filename <- format(
        current_start_date,
        'GEOSChem.Restart.%Y%m%d_%H%Mz.nc4'
      )
      current_run$configuration$symlinks[[restart_filename]] <- file.path(
        '..',
        current_run$name,
        restart_filename
      )
    }
    current_run$name <- sprintf('%s_split%02d', run$name, index)
    current_run$split <- list(
      parent = run$name,
      previous = previous_run_name
    )

    runs <- c(runs, list(current_run))

    current_start_date <- current_start_date + max_length
    index <- index + 1L
  }

  runs
}
