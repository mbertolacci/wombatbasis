#' @export
geoschem_main_settings <- function(
  simulation_menu,
  timestep_menu,
  advected_species_menu,
  passive_species_menu = geoschem_passive_species_menu(),
  extra = 'default'
) {
  if (extra == 'default') {
    extra <- paste0(readLines(system.file(
      'extdata',
      'input-geos-extra-default.txt',
      package = 'wombatbasis'
    )), collapse = '\n')
  }

  structure(list(
    simulation_menu = simulation_menu,
    timestep_menu = timestep_menu,
    advected_species_menu = advected_species_menu,
    passive_species_menu = passive_species_menu,
    extra = extra
  ), class = 'geoschem_main_settings')
}

#' @export
as.character.geoschem_main_settings <- function(x, ...) {
  sprintf(
    'GEOS-CHEM UNIT TEST SIMULATION: geosfp_2x25_CO2
------------------------+------------------------------------------------------
%s
------------------------+------------------------------------------------------
%s
------------------------+------------------------------------------------------
%s
%s------------------------+------------------------------------------------------
%s
------------------------+------------------------------------------------------
END OF FILE             :
------------------------+------------------------------------------------------',
    as.character(x$simulation_menu),
    as.character(x$timestep_menu),
    as.character(x$advected_species_menu),
    if (length(x$passive_species_menu$species) > 0) {
      sprintf(
        '------------------------+------------------------------------------------------
%s\n',
        as.character(x$passive_species_menu)
      )
    } else {
      ''
    },
    x$extra
  )
}

#' @export
print.geoschem_main_settings <- function(x, ...) {
  cat(as.character(x, ...))
}

#' @export
geoschem_simulation_menu <- function(
  start,
  end,
  run_directory = './',
  root_data_directory,
  global_offsets = c(0, 0)
) {
  structure(list(
    start = start,
    end = end,
    run_directory = run_directory,
    root_data_directory = root_data_directory,
    global_offsets = global_offsets
  ), class = c('geoschem_simulation_menu', 'geoschem_main_settings_menu'))
}

#' @export
as.character.geoschem_simulation_menu <- function(x, ...) {
  to_date_format <- function(x) {
    if (is.character(x)) x else format(x, '%Y%m%d %H%M%S')
  }

  sprintf(
    '%%%%%% SIMULATION MENU %%%%%% :
Start YYYYMMDD, hhmmss  : %s
End   YYYYMMDD, hhmmss  : %s
Run directory           : %s
Root data directory     : %s
Global offsets I0, J0   : %d %d',
    to_date_format(x$start),
    to_date_format(x$end),
    x$run_directory,
    x$root_data_directory,
    x$global_offsets[1],
    x$global_offsets[2]
  )
}

#' @export
geoschem_timestep_menu <- function(
  transport_convection_timestep,
  chemistry_emissions_timestep
) {
  structure(list(
    transport_convection_timestep = transport_convection_timestep,
    chemistry_emissions_timestep = chemistry_emissions_timestep
  ), class = c('geoschem_timestep_menu', 'geoschem_main_settings_menu'))
}

#' @export
as.character.geoschem_timestep_menu <- function(x, ...) {
  sprintf(
    '%%%%%% TIMESTEP MENU %%%%%%   :
Tran/conv timestep [sec]: %d
Chem/emis timestep [sec]: %d',
    x$transport_convection_timestep,
    x$chemistry_emissions_timestep
  )
}

#' @export
geoschem_advected_species_menu <- function(
  type_of_simulation,
  species
) {
  structure(list(
    type_of_simulation = type_of_simulation,
    species = species
  ), class = c('geoschem_advected_species_menu', 'geoschem_main_settings_menu'))
}

#' @export
as.character.geoschem_advected_species_menu <- function(x, ...) {
  sprintf(
    '%%%%%% ADVECTED SPECIES MENU %%%%%%:
Type of simulation      : %d
Species Entries ------->: Name
%s',
    x$type_of_simulation,
    paste0(
      sprintf('Species #%02d             : %s', seq_along(x$species), x$species),
      collapse = '\n'
    )
  )
}

#' @export
geoschem_passive_species_menu <- function(
  species = NULL
) {
  structure(list(
    species = species
  ), class = c('geoschem_passive_species_menu', 'geoschem_main_settings_menu'))
}

#' @export
as.character.geoschem_passive_species_menu <- function(x, ...) {
  sprintf(
    '%%%%%% PASSIVE SPECIES MENU %%%%%%:
%s',
    paste0(
      sapply(seq_along(x$species), function(i) {
        sprintf('Passive species #%02d     : %s', i, as.character(x$species[[i]]))
      }),
      collapse = '\n'
    )
  )
}

#' @export
geoschem_passive_species <- function(
  name,
  molecular_weight,
  lifetime,
  initial_concentration,
  long_name = NULL
) {
  structure(list(
    name = name,
    molecular_weight = molecular_weight,
    lifetime = lifetime,
    initial_concentration = initial_concentration,
    long_name = long_name
  ), class = 'geoschem_passive_species')
}

#' @export
as.character.geoschem_passive_species <- function(x, ...) {
  sprintf(
    '%s %e %e %e%s',
    x$name,
    x$molecular_weight,
    x$lifetime,
    x$initial_concentration,
    if (is.null(x$long_name)) '' else sprintf(' %s', x$long_name)
  )
}

#' @export
print.geoschem_main_settings_menu <- function(x, ...) {
  cat(as.character(x, ...))
}
