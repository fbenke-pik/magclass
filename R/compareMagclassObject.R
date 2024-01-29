compareMagclassObject <- function(x, y, tol = 0.5) {

  # compare dimensions names ----

  if (!identical(dim(x), dim(y))) {
    message("# Dimension names are not identical")
    message(paste0("## Dimension ",
                   which(dim(x) != dim(y)), ": ",
                   dim(x)[dim(x) != dim(y)], " != ",
                   dim(y)[dim(x) != dim(y)],
                   collapse = ", "))
  } else {
    message("# Dimensions are identical (/)")
  }

  # compare dimension values ----

  same <- TRUE
  for (i in 1:3) {
    if (!setequal(dimnames(x)[[i]], dimnames(y)[[i]])) {
      message("# Values in dimension ", i, " are not identical")
      same <- FALSE
      onlyX <- setdiff(dimnames(x)[[i]], dimnames(y)[[i]])
      onlyY <- setdiff(dimnames(y)[[i]], dimnames(x)[[i]])

      if (length(onlyX) > 0) {
        message(paste0("## Values only in x: ", paste0(onlyX, collapse = ", ")))
      }

      if (length(onlyY) > 0) {
        message(paste0("## Values only in y: ", paste0(onlyY, collapse = ", ")))
      }
    }
  }

  if (same) {
    message("# All dimension names are identical (/)")
  }

  # continue with the subset of identical values ----

  x <- x[
    intersect(getRegions(x), getRegions(y)),
    intersect(getYears(x), getYears(y)),
    intersect(getNames(x), getNames(y))
  ]

  y <- y[
    intersect(getRegions(x), getRegions(y)),
    intersect(getYears(x), getYears(y)),
    intersect(getNames(x), getNames(y))
  ]

  # compare NA values ----

  if (length(x[is.na(x)]) != length(y[is.na(y)])) {
    message(paste0("# Number of NAs differs: ", length(x[is.na(x)]), " != ", length(y[is.na(y)])))
  } else {
    message("# Number of NAs is identical")
  }

  naX <- magpply(x, function(y) any(is.na(y)), MARGIN = 3)
  naX <- x[, , naX]
  naY <- magpply(y, function(y) any(is.na(y)), MARGIN = 3)
  naY <- y[, , naY]

  if (length(setdiff(getNames(naX), getNames(naY))) > 0) {
    message(paste0("## Items with NA values only in x: ",
                   paste0(setdiff(getNames(naX), getNames(naY)), collapse = ", ")))
  }

  if (length(setdiff(getNames(naY), getNames(naX))) > 0) {
    message(paste0("## Items with NA values only in y: ",
                   paste0(setdiff(getNames(naY), getNames(naX)), collapse = ", ")))
  }

  # compare non-NA values ----
  maxDiff <- max(abs(x - y), na.rm = TRUE)

  # TODO: improve this section

  if (maxDiff != 0) {
    message(paste0("# Maximum value difference in common values: ", round(maxDiff, digits = 2)))

    diff <- (x - y) /  (x + y) / 2
    diff[is.na(diff)] <- 0
    diff <- abs(diff)


    gaps <- magclass::where(diff > tol)
    message("# Dimensions with differences% :")
    print(gaps$true$individual)
  } else {
    message(paste0("# Values are identical (/)"))
  }
}
