#' @export
hemco_configuration <- function(
  settings,
  extension_switches,
  base_emissions = hemco_base_emissions(),
  scale_factors = hemco_scale_factors(),
  masks = hemco_masks()
) {
  structure(list(
    settings = settings,
    extension_switches = extension_switches,
    base_emissions = base_emissions,
    scale_factors = scale_factors,
    masks = masks
  ), class = 'hemco_configuration')
}

#' @export
as.character.hemco_configuration <- function(x, ...) {
  sprintf(
    '%s

%s

%s

%s

%s',
    as.character(x$settings),
    as.character(x$extension_switches),
    as.character(x$base_emissions),
    as.character(x$scale_factors),
    as.character(x$masks)
  )
}

#' @export
print.hemco_configuration <- function(x, ...) {
  cat(as.character(x, ...))
}

#' @export
hemco_settings <- function(
  root_directory,
  meteorology_directory,
  log_file,
  diagnostics_file,
  diagnostics_prefix,
  diagnostics_frequency,
  diagnostics_time_stamp,
  wildcard,
  separator,
  unit_tolerance,
  negative_values,
  only_unitless_scale_factors,
  verbose,
  warnings,
  extra = ''
) {
  structure(list(
    root_directory = root_directory,
    meteorology_directory = meteorology_directory,
    log_file = log_file,
    diagnostics_file = diagnostics_file,
    diagnostics_prefix = diagnostics_prefix,
    diagnostics_frequency = diagnostics_frequency,
    diagnostics_time_stamp = diagnostics_time_stamp,
    wildcard = wildcard,
    separator = separator,
    unit_tolerance = unit_tolerance,
    negative_values = negative_values,
    only_unitless_scale_factors = only_unitless_scale_factors,
    verbose = verbose,
    warnings = warnings,
    extra = ''
  ), class = c('hemco_settings', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_settings <- function(x, ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION SETTINGS
###############################################################################

ROOT: %s
METDIR: %s
Logfile: %s
DiagnFile: %s
DiagnPrefix: %s
DiagnFreq: %s
DiagnTimeStamp: %s
Wildcard: %s
Separator: %s
Unit tolerance: %d
Negative values: %d
Only unitless scale factors: %d
Verbose: %d
Warnings: %d
%s
### END SECTION SETTINGS ###',
    x$root_directory,
    x$meteorology_directory,
    x$log_file,
    x$diagnostics_file,
    x$diagnostics_prefix,
    x$diagnostics_frequency,
    x$diagnostics_time_stamp,
    x$wildcard,
    x$separator,
    x$unit_tolerance,
    x$negative_values,
    x$only_unitless_scale_factors,
    x$verbose,
    x$warnings,
    x$extra
  )
}

#' @export
hemco_extension_switches <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_extension_switches', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_extension_switches <- function(x, config, ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION EXTENSION SWITCHES
###############################################################################

# ExtNr ExtName           on/off  Species
%s
%s
END SECTION EXTENSION SWITCHES',
    paste0(
      sapply(x, as.character),
      collapse = '\n'
    ),
    attr(x, 'extra')
  )
}

#' @export
hemco_extension_switch <- function(
  number,
  name,
  enabled,
  species,
  collections
) {
  structure(
    list(
      number = number,
      name = name,
      enabled = enabled,
      species = species,
      collections = collections
    ),
    class = c('hemco_extension_switch', 'hemco_configuration_part')
  )
}

#' @export
as.character.hemco_extension_switch <- function(x, config, ...) {
  sprintf(
    '%d %s %s %s
%s
# ----------------------------------------------------------------------------',
    x$number,
    x$name,
    if (x$enabled) 'on' else 'off',
    x$species,
    paste0(
      sprintf(
        '    --> %s: %s',
        names(x$collections),
        ifelse(x$collections, 'true', 'false')
      ),
      collapse = '\n'
    )
  )
}

#' @export
hemco_base_emissions <- function(
  ...,
  extra = 'default'
) {
  if (extra == 'default') {
    extra <- paste0(readLines(system.file(
      'extdata',
      'hemco-base-emissions-extra-default-co2.txt',
      package = 'wombatbasis'
    )), collapse = '\n')
  }
  structure(
    list(...),
    class = c('hemco_base_emissions', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_base_emissions <- function(x, config, ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION BASE EMISSIONS
###############################################################################

# ExtNr Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Species ScalIDs Cat Hier
%s
%s

### END SECTION BASE EMISSIONS ###',
    paste0(
      sapply(x, as.character),
      collapse = '\n'
    ),
    attr(x, 'extra')
  )
}

#' @export
hemco_base_emission_collection <- function(
  name,
  fields
) {
  if (is(fields, 'hemco_base_emission_field')) {
    fields <- list(fields)
  }
  stopifnot(
    all(sapply(fields, is, 'hemco_base_emission_field'))
  )
  structure(list(
    name = name,
    fields = fields
  ), class = c('hemco_base_emission_collection', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_base_emission_collection <- function(x, ...) {
  sprintf(
    '(((%s
%s
)))%s',
    x$name,
    paste0(
      sapply(x$fields, as.character),
      collapse = '\n'
    ),
    x$name
  )
}

#' @export
hemco_base_emission_field <- function(
  extension_number = 0,
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  species,
  scale_factors = NULL,
  category,
  hierarchy
) {
  if (is(scale_factors, 'hemco_scale_factor')) {
    scale_factors <- list(scale_factors)
  }
  scale_factors <- sapply(scale_factors, function(x) {
    if (is(x, 'hemco_scale_factor')) x$id else x
  })
  structure(list(
    extension_number = extension_number,
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    species = species,
    scale_factors = scale_factors,
    category = category,
    hierarchy = hierarchy
  ), class = c('hemco_base_emission_field', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_base_emission_field <- function(x, config, ...) {
  sprintf(
    '%d %s %s %s %s %s %s %s %s %s %d %d',
    .null_as_dash(x$extension_number),
    .null_as_dash(x$name),
    .null_as_dash(x$source_file),
    .null_as_dash(x$source_variable),
    .null_as_dash(x$source_time),
    .null_as_dash(x$cre),
    .null_as_dash(x$source_dimension),
    .null_as_dash(x$source_unit),
    .null_as_dash(x$species),
    if (is.null(x$scale_factors) || length(x$scale_factors) == 0) {
      '-'
    } else {
      paste0(x$scale_factors, collapse = '/')
    },
    .null_as_dash(x$category),
    .null_as_dash(x$hierarchy)
  )
}

#' @export
optimise_base_emissions <- function(x) {
  .optimise_fields <- function(fields) {
    repeatable_fields <- c(
      'source_file', 'source_variable', 'source_time', 'cre',
      'source_dimension', 'source_unit'
    )
    # Reorder fields so that repeatable will be adjacent
    fields <- fields[do.call(order, lapply(
      repeatable_fields,
      function(name) {
        sapply(fields, getElement, name)
      }
    ))]

    get_fields <- function(z) sapply(
      repeatable_fields,
      function(name) getElement(z, name)
    )
    lapply(seq_along(fields), function(i) {
      field <- fields[[i]]
      if (i == 1) return(field)
      if (all(get_fields(field) == get_fields(fields[[i - 1]]))) {
        for (name in repeatable_fields) {
          field[[name]] <- '-'
        }
      }
      field
    })
  }

  if (is(x, 'hemco_base_emissions')) {
    filter_to <- function(name) {
      Filter(function(z) is(z, name), x)
    }
    fields <- .optimise_fields(filter_to('hemco_base_emission_field'))
    collections <- lapply(
      filter_to('hemco_base_emission_collection'),
      optimise_base_emissions
    )

    do.call(
      hemco_base_emissions,
      c(fields, collections, list(extra = attr(x, 'extra')))
    )
  } else if (is(x, 'hemco_base_emission_collection')) {
    x$fields <- .optimise_fields(x$fields)
    x
  } else {
    stop('object class not supported')
  }
}

#' @export
hemco_scale_factors <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_scale_factors', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_scale_factors <- function(x, config, ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION SCALE FACTORS
###############################################################################

# ScalID Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Oper
%s
%s

### END SECTION SCALE FACTORS ###',
    paste0(
      sapply(x, as.character),
      collapse = '\n'
    ),
    attr(x, 'extra')
  )
}

#' @export
hemco_scale_factor <- function(
  id,
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  operator,
  mask = NULL
) {
  stopifnot(is(operator, 'hemco_scale_factor_operator'))
  stopifnot(is.null(mask) || is(mask, 'hemco_mask'))
  structure(list(
    id = id,
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    operator = operator,
    mask = mask
  ), class = c('hemco_scale_factor', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_scale_factor <- function(x, ...) {
  sprintf(
    '%d %s %s %s %s %s %s %s %s%s',
    x$id,
    x$name,
    .null_as_dash(x$source_file),
    .null_as_dash(x$source_variable),
    .null_as_dash(x$source_time),
    .null_as_dash(x$cre),
    x$source_dimension,
    x$source_unit,
    as.character(x$operator),
    if (is.null(x$mask)) '' else sprintf(' %s', x$mask$id)
  )
}

#' @export
hemco_scale_factor_operator <- function(name = c('multiplication', 'division', 'squared')) {
  name <- match.arg(name)
  structure(list(
    name = name
  ), class = c('hemco_scale_factor_operator', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_scale_factor_operator <- function(x, ...) {
  output <- c(
    'multiplication' = '1',
    'division' = '-1',
    'squared' = '2'
  )[x$name]
  names(output) <- NULL
  output
}

#' @export
hemco_masks <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_masks', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_masks <- function(x, config, ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION MASKS
###############################################################################

# ScalID Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Oper Lon1/Lat1/Lon2/Lat2
%s
%s

### END SECTION MASKS ###',
    paste0(
      sapply(x, as.character),
      collapse = '\n'
    ),
    attr(x, 'extra')
  )
}

#' @export
hemco_mask <- function(
  id,
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  operator,
  box
) {
  stopifnot(is(operator, 'hemco_mask_operator'))
  structure(list(
    id = id,
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    operator = operator,
    box = box
  ), class = c('hemco_mask', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_mask <- function(x, ...) {
  sprintf(
    '%d %s %s %s %s %s %s %s %s %s',
    x$id,
    x$name,
    x$source_file,
    x$source_variable,
    x$source_time,
    x$cre,
    x$source_dimension,
    x$source_unit,
    as.character(x$operator),
    x$box
  )
}

#' @export
hemco_mask_operator <- function(name = c('multiplication', 'division', 'mirror')) {
  name <- match.arg(name)
  structure(list(
    name = name
  ), class = c('hemco_mask_operator', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_mask_operator <- function(x, ...) {
  output <- c(
    'multiplication' = '1',
    'division' = '-1',
    'mirror' = '3'
  )[x$name]
  names(output) <- NULL
  output
}

#' @export
print.hemco_configuration_part <- function(x, ...) {
  cat(as.character(x, ...))
}

.null_as_dash <- function(value) {
  if (is.null(value)) '-' else value
}
