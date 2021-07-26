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

      scaling_grid <- .truncate_grid_times(multiply_grids(
        factor_grids
      ))

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
basis_to_runs <- function(basis, strategy = c('one_to_one', 'passive_species')) {
  strategy <- match.arg(strategy)

  match.fun(sprintf('.basis_to_runs_%s', strategy))(basis)
}

#' @export
write_basis_runs <- function(basis_runs, base_directory, optimise = TRUE) {
  for (basis_run in basis_runs) {
    path <- file.path(base_directory, basis_run$name)
    if (optimise) {
      basis_run$configuration$hemco$base_emissions <- optimise_base_emissions(
        basis_run$configuration$hemco$base_emissions
      )
    }
    write_geoschem_run(basis_run$configuration, path)
  }
}

.times_to_hemco_source_time <- function(x) {
  if (any(lubridate::minute(x) != 0)) {
    '*'
  } else {
    years <- lubridate::year(x)
    sprintf(
      '%s/1-12/1-31/0-23',
      if (min(years) == max(years)) {
        min(years)
      } else {
        sprintf('%d-%d', min(years), max(years))
      }
    )
  }
}

.basis_function_start_time <- function(basis_function, base_run) {
  has_time_grid <- 'time' %in% names(dimnames(basis_function$scaling_grid))
  if (has_time_grid) {
    min(attr(
      basis_function$scaling_grid,
      'time'
    ))
  } else {
    base_run$main$simulation_menu$start
  }
}
