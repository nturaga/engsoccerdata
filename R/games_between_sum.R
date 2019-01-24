#' Function to List the summary stats of all games ever between two teams
#'
#' @param df The results dataframe
#' @param teamname1 teamname1
#' @param teamname2 teamname2
#' @param type Return 'all' results when type=\code{all} or split by
#'     home and away if type=\code{homeaway}.
#' @return a dataframe with summary of results. Will be
#'     empty dataframe if never played.
#' @importFrom magrittr '%>%'
#' @examples
#'     games_between_sum(england, 'Exeter City', 'York City')
#'     games_between_sum(england, 'Aston Villa', 'York City', 'homeaway')
#'     games_between_sum(england, 'Manchester United', 'Liverpool', 'all')
#' @export
games_between_sum <- function(df = NULL, teamname1 = NULL, teamname2 = NULL, type = "all") {
    
    . <- D <- GA <- GD <- GF <- L <- W <- ga <- gf  <- NULL
    opp <- team <- venue <- tmp1 <- tmp2 <- tmp <- NULL
    home <- visitor <- hgoal <- vgoal <- goaldif <- NULL
    FT <- Season <- division <- result <- maxgoal <- mingoal <- absgoaldif <- NULL
    
    
    tmp <- df %>%
        group_by(home) %>%
        filter(home == teamname1 & visitor == teamname2 | home == teamname2 & visitor == teamname1)
    
    tmp1 <- engsoccerdata::homeaway(tmp) %>%
        group_by(team, opp, venue) %>%
        mutate(goaldif = gf - ga,
                      result = ifelse(gf > ga, "H", ifelse(gf <  ga, "A", "D"))) %>%
        summarise(P = nrow(.),
                         GF = sum(gf),
                         GA = sum(ga),
                         GD = sum(goaldif),
                         W = sum(result == "H"),
                         D = sum(result == "D"),
                         L = sum(result == "A")) %>%
        select(team, opp, venue, W, D, L, GF, GA, GD) %>%
        as.data.frame()
    
    
    tmp2 <- engsoccerdata::homeaway(tmp) %>%
        mutate(venue = "all",
                      goaldif = gf - ga,
                      result = ifelse(gf > ga, "H", ifelse(gf < ga, "A", "D"))) %>%
        group_by(team, opp, venue) %>%
        summarise(P = nrow(.),
                         GF = sum(gf),
                         GA = sum(ga),
                         GD = sum(goaldif),
                         W = sum(result == "H"), 
                         D = sum(result == "D"),
                         L = sum(result == "A")) %>%
        select(team, opp, venue, W, D, L, GF, GA, GD) %>%
        as.data.frame()
    
    if (type == "all") {
        return(tmp2)
    }
    if (type == "homeaway") {
        return(tmp1)
    }
}
