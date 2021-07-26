.basis_to_runs_passive_species <- function(
  basis,
  molecular_weight = 44,
  lifetime = -1,
  initial_concentration = 4.0e-4
) {
  base_run <- basis$base_run
  max_scale_factor_id <- max(sapply(
    base_run$hemco$scale_factors,
    getElement,
    'id'
  ))

  all_start_times <- lapply(
    basis$basis_functions,
    .basis_function_start_time,
    base_run
  )

  start_times <- unique(all_start_times)
  by_start_times <- unname(split(
    basis$basis_functions,
    as.integer(as.factor(unlist(all_start_times)))
  ))

  runs <- c(list(list(
    name = 'base',
    configuration = base_run
  )), lapply(seq_along(start_times), function(index) {
    configuration <- base_run

    passive_names <- sprintf(
      'r%04dp%03d',
      index,
      seq_along(by_start_times[[index]])
    )

    background_name <- sprintf('r%04dp000', index)
    configuration$main$simulation_menu$start <- start_times[[index]]
    configuration$main$advected_species_menu$species <- c(
      background_name,
      passive_names
    )
    configuration$main$passive_species_menu <- geoschem_passive_species_menu(c(
      list(
        geoschem_passive_species(
          background_name,
          molecular_weight,
          lifetime,
          initial_concentration
        )
      ),
      lapply(
        passive_names,
        geoschem_passive_species,
        molecular_weight,
        lifetime,
        initial_concentration
      )
    ))

    base_emission_fields <- list()
    for (tracer_index in seq_along(by_start_times[[index]])) {
      basis_function <- by_start_times[[index]][[tracer_index]]
      for (base_emission_field in base_run$hemco$base_emissions) {
        if (base_emission_field$name %in% basis_function$fields) {
          base_emission_field$name <- sprintf(
            '%s_%03d',
            base_emission_field$name,
            tracer_index
          )
          base_emission_field$species <- passive_names[tracer_index]
          base_emission_field$scale_factors <- c(
            base_emission_field$scale_factors,
            max_scale_factor_id + tracer_index
          )

          base_emission_fields <- c(base_emission_fields, list(base_emission_field))
        }
      }
    }
    configuration$hemco$base_emissions <- do.call(
      hemco_base_emissions,
      c(
        base_emission_fields,
        list(
          extra = attr(base_run$hemco$base_emissions, 'extra')
        )
      )
    )

    for (tracer_index in seq_along(by_start_times[[index]])) {
      basis_function <- by_start_times[[index]][[tracer_index]]

      filename <- sprintf('basis-scaling-%03d.nc', tracer_index)
      has_time_grid <- 'time' %in% names(dimnames(basis_function$scaling_grid))
      scaling_scale_factor <- hemco_scale_factor(
        id = max_scale_factor_id + tracer_index,
        name = sprintf('BASISSCALE_%03d', tracer_index),
        source_file = filename,
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
      configuration$hemco$scale_factors[[
        sprintf('scaling_%03d', tracer_index)
      ]] <- scaling_scale_factor

      configuration$files[[filename]] <- basis_function$scaling_grid
    }

    configuration$hemco_diagnostics <- do.call(hemco_diagnostics, lapply(
      seq_along(by_start_times[[index]]),
      function(tracer_index) {
        hemco_diagnostic(
          sprintf('Emis_%s', passive_names[tracer_index]),
          passive_names[tracer_index],
          -1,
          -1,
          -1,
          3,
          'kg/m2/s',
          passive_names[tracer_index]
        )
      }
    ))

    configuration$files[['Makefile']] <- NULL
    configuration$files[['getRunInfo']] <- NULL
    configuration$symlinks[['geos.mp']] <- file.path('../base', 'geos.mp')

    list(
      name = sprintf('r%04d', index),
      configuration = configuration
    )
  }))
}
