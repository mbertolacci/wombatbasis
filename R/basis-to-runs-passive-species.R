.basis_to_runs_passive_species <- function(
  basis,
  groups,
  molecular_weight = 44,
  lifetime = -1,
  initial_concentration = 4.0e-4,
  max_species_per_run = 12
) {
  base_run <- basis$base_run
  max_scale_factor_id <- max(sapply(
    base_run$hemco$scale_factors,
    getElement,
    'id'
  ))

  all_start_times <- .basis_function_start_times(basis)

  start_times <- sort(unique(all_start_times))
  if (missing(groups)) {
    split_groups <- factor(format(all_start_times, '%Y%m%d'))
    split_start_times <- start_times
  } else {
    groups <- factor(groups)
    all_split_groups <- interaction(
      groups,
      format(all_start_times, '%Y%m%d'),
      sep = '_',
      drop = FALSE
    )
    split_groups <- droplevels(all_split_groups)
    split_start_times <- expand.grid(
      group = levels(groups),
      start_time = start_times
    )$start_time[
      levels(all_split_groups) %in% levels(split_groups)
    ]
  }
  by_split_groups <- lapply(
    unname(split(
      basis$basis_functions,
      as.integer(split_groups)
    )),
    function(tracers) {
      tracers <- c(list('background'), tracers)
      unname(split(
        tracers,
        1L + floor((seq_along(tracers) - 1L) / max_species_per_run)
      ))
    }
  )

  c(list(list(
    name = 'base',
    configuration = base_run
  )), do.call(c, lapply(seq_len(nlevels(split_groups)), function(index) {
    parts <- by_split_groups[[index]]

    lapply(seq_along(parts), function(part_index) {
      configuration <- base_run
      run_basis_functions <- parts[[part_index]]
      run_split_group <- levels(split_groups)[index]
      run_name <- sprintf('%s_part%03d', run_split_group, part_index)

      passive_names <- sprintf(
        'r%04dp%03ds%03d',
        index,
        part_index,
        seq_along(parts[[part_index]])
      )

      configuration$main$simulation_menu$start <- split_start_times[[index]]
      configuration$main$advected_species_menu$species <- passive_names
      configuration$main$passive_species_menu <- geoschem_passive_species_menu(
        lapply(
          passive_names,
          geoschem_passive_species,
          molecular_weight,
          lifetime,
          initial_concentration
        )
      )

      base_emission_fields <- list()
      for (tracer_index in seq_along(run_basis_functions)) {
        basis_function <- run_basis_functions[[tracer_index]]
        if (is.character(basis_function) && basis_function == 'background') next
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

      for (tracer_index in seq_along(run_basis_functions)) {
        basis_function <- run_basis_functions[[tracer_index]]

        if (is.character(basis_function) && basis_function == 'background') next

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
          cre = if (has_time_grid) 'RF' else 'C',
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
        seq_along(run_basis_functions),
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
      configuration$files[[format(
        configuration$main$simulation_menu$start,
        'GEOSChem.Restart.%Y%m%d_%H%Mz.nc4'
      )]] <- grid_data(1, time = configuration$main$simulation_menu$start)
      configuration$symlinks[['geos.mp']] <- file.path('../base', 'geos.mp')

      list(
        name = run_name,
        split_group = run_split_group,
        configuration = configuration,
        mapping = data.frame(
          basis_function = sapply(run_basis_functions, function(basis_function) {
            if (is.character(basis_function) && basis_function == 'background') {
              sprintf('background_%s', run_split_group)
            } else {
              basis_function$name
            }
          }),
          run = run_name,
          species = passive_names,
          stringsAsFactors = FALSE
        )
      )
    })
  })))
}
