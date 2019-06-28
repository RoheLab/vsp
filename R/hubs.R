#' @export
get_z_hubs <- function(fa, hubs_per_factor = 10) {
  fa %>%
    get_varimax_z() %>%
    mutate(index = row_number()) %>%
    gather(factor, loading, contains("z"), -index) %>%
    group_by(factor) %>%
    top_n(hubs_per_factor, wt = abs(loading))
}

#' @export
get_y_hubs <- function(fa, hubs_per_factor = 10) {
  fa %>%
    get_varimax_y() %>%
    mutate(index = row_number()) %>%
    gather(factor, loading, contains("y"), -index) %>%
    group_by(factor) %>%
    top_n(hubs_per_factor, wt = abs(loading))
}