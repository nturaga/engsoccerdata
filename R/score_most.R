#' Team who has been involved in the most games of each scoreline
#'
#' @param df Results dataframe
#' @param score score
#' @return a dataframe with frequency of matches and team.
#' @importFrom magrittr '%>%'
#' @examples
#' score_most(england, '6-6')
#' score_most(england, '8-0')
#' score_most(england, '9-1')
#' @export

score_most <- function(df, score) {
    
    score1 <- n <- . <- key <- Date <- tier <- home <- team <- visitor <- NULL
    hgoal <- vgoal <- goaldif <- FT <- Season <- division <- result <- maxgoal <- mingoal <- absgoaldif <- NULL
    
    temp <- strsplit(score, split = "-")
    temp <- as.vector(unlist(temp[[1]]))
    score1 <- paste(temp[2], temp[1], sep = "-")
    
    df %>%
        filter(FT == score | FT == score1) %>%
        select(Season, home, visitor) %>%
        tidyr::gather(key, team, home:visitor) %>%
        group_by(team) %>% 
        tally() %>%
        arrange(-n) %>%
        filter(n == max(n))
}
