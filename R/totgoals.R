#' Return all instances of a team being involved in a game with n goals
#'
#' @param df The results dataframe
#' @param N Number of goals
#' @param teamname teamname
#' @return a dataframe with  matches
#' @importFrom magrittr '%>%'
#' @examples
#' totgoals(england, 10, 'York City')
#' totgoals(england, 12, 'Aston Villa')
#'
#' @export
totgoals <- function(df = NULL, N = NULL, teamname = NULL) {
    df %>%
        mutate(totgoal = hgoal + vgoal) %>%
        filter(totgoal >= N, home == teamname | visitor == teamname) %>%
        select(Date, Season, home, visitor, FT, totgoal, tier) %>%
        arrange(-totgoal, Season)
}
