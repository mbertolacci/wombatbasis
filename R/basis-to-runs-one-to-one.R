.basis_to_runs_one_to_one <- function(basis) {
  base_run <- basis$base_run
  max_scale_factor_id <- max(sapply(
    base_run$hemco$scale_factors,
    getElement,
    'id'
  ))

  .basis_function_to_run <- function(basis_function) {
    configuration <- base_run

    configuration$main$simulation_menu$start <- .basis_function_start_time(
      basis_function,
      base_run
    )

    has_time_grid <- 'time' %in% names(dimnames(basis_function$scaling_grid))
    scaling_scale_factor <- hemco_scale_factor(
      id = max_scale_factor_id + 1,
      name = 'BASISSCALE',
      source_file = 'basis-scaling.nc',
      source_variable = 'value',
      # TODO(mgnb): do better than this
      source_time = if (has_time_grid) {
        .times_to_hemco_source_time(attr(basis_function$scaling_grid, 'time'))
      } else {
        sprintf(
          '%d/1/1/0',
          lubridate::year(configuration$main$simulation_menu$start)
        )
      },
      cre = 'C',
      source_dimension = 'xy',
      source_unit = '1',
      operator = hemco_scale_factor_operator('multiplication')
    )
    configuration$hemco$scale_factors$basis_scale <- scaling_scale_factor

    for (i in seq_along(configuration$hemco$base_emissions)) {
      base_emission_field <- configuration$hemco$base_emissions[[i]]
      # TODO(mgnb): recurse into collections too
      if (base_emission_field$name %in% basis_function$fields) {
        base_emission_field$scale_factors <- c(
          base_emission_field$scale_factors,
          scaling_scale_factor$id
        )
        configuration$hemco$base_emissions[[i]] <- base_emission_field
      }
    }

    configuration$files[['Makefile']] <- NULL
    configuration$files[['getRunInfo']] <- NULL
    configuration$files[['basis-scaling.nc']] <- basis_function$scaling_grid + 1

    configuration$symlinks[['geos.mp']] <- file.path('../base', 'geos.mp')

    restart_filename <- format(
      configuration$main$simulation_menu$start,
      'GEOSChem.Restart.%Y%m%d_%H%Mz.nc4'
    )
    configuration$symlinks[[restart_filename]] <- file.path(
      '../base/output',
      restart_filename
    )

    list(
      name = basis_function$name,
      configuration = configuration
    )
  }

  c(
    list(list(
      name = 'base',
      configuration = base_run
    )),
    lapply(basis$basis_functions, .basis_function_to_run)
  )
}
