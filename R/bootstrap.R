find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(current, "_quarto.yml")) ||
      length(list.files(current, pattern = "\\.Rproj$", full.names = TRUE)) > 0
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Nao foi possivel localizar a raiz do projeto.", call. = FALSE)
    }

    current <- parent
  }
}

project_root <- local({
  cached <- NULL

  function() {
    if (is.null(cached)) {
      cached <<- find_project_root()
    }

    cached
  }
})

project_path <- function(...) {
  file.path(project_root(), ...)
}

resolve_existing_path <- function(candidates) {
  existing <- candidates[file.exists(candidates)]

  if (length(existing) > 0) {
    return(existing[[1]])
  }

  candidates[[1]]
}

blog_duckdb_path <- function() {
  resolve_existing_path(
    c(
      project_path("fundos_db.duckdb"),
      project_path("posts", "desempenho_fundos_rf", "fundos_db.duckdb")
    )
  )
}

connect_blog_db <- function(read_only = FALSE) {
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = blog_duckdb_path(),
    read_only = read_only
  )
}
