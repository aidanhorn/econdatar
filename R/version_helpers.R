# Client-side resolution of version = "latest".
#
# The EconData API resolves `versions=latest` by sorting version strings
# lexicographically, so "1.2.9" is preferred over "1.2.13". Until that is fixed
# server-side, ask for all versions and pick the numerically greatest one per
# (agencyid, id) here. See https://github.com/coderaanalytics/econdatar/issues/46

query_versions <- function(version) {
  if (identical(version, "latest")) {
    "all"
  } else {
    paste(version, collapse = ",")
  }
}

version_components <- function(versions) {
  parts <- strsplit(as.character(versions), ".", fixed = TRUE)
  parts <- lapply(parts, function(p) {
    x <- suppressWarnings(as.integer(p))
    x[is.na(x)] <- -1L
    x
  })
  width <- max(1L, vapply(parts, length, integer(1)))
  do.call(rbind, lapply(parts, function(x) c(x, rep(0L, width - length(x)))))
}

is_latest_version <- function(agencyid, id, version) {
  n <- length(version)
  if (n == 0) {
    return(logical(0))
  }
  components <- version_components(version)
  group <- paste(agencyid, id, sep = "\r")
  keys <- c(list(group), lapply(seq_len(ncol(components)), function(j) {
    -components[, j]
  }))
  ord <- do.call(order, keys)
  latest <- logical(n)
  latest[ord[!duplicated(group[ord])]] <- TRUE
  latest
}

keep_latest_versions <- function(items,
                                 version,
                                 get_ref = function(x) x[[2]]) {
  if (!identical(version, "latest") || length(items) == 0) {
    return(items)
  }
  refs <- lapply(items, get_ref)
  agencyid <- vapply(refs, function(r) as.character(r$agencyid), character(1))
  id <- vapply(refs, function(r) as.character(r$id), character(1))
  ver <- vapply(refs, function(r) as.character(r$version), character(1))
  items[is_latest_version(agencyid, id, ver)]
}
