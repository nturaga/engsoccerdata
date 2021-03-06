#' The number of times a team has played against each opponent
#'
#' @param df The results dataframe
#' @param teamname teamname
#' @return a dataframe with frequency of matches and team.
#' @importFrom magrittr '%>%'
#' @examples
#' opponentfreq(england, 'Aston Villa')
#' opponentfreq(england, 'York City')
#' opponentfreq(england, 'Milton Keynes Dons')
#' @export
opponentfreq <- function(df = NULL, teamname = NULL) {
    temp <- df %>%
        filter(home == teamname) %>%
        select(team = visitor)
    
    temp1 <- df %>%
        filter(visitor == teamname) %>%
        select(team = home)
    
    temp2 <- bind_rows(temp, temp1) %>%
        group_by(team) %>%
        tally() %>%
        arrange(-n)
    
    return(as.data.frame(unclass(temp2)))
    
}
