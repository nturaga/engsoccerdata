#' Return all results by team
#'
#' @param df the results dataset
#' @return a dataframe of all games by team
#' @importFrom magrittr '%>%'
#' @examples
#' homeaway(england[england$Season==2015 & england$tier==1,])
#'
#' @export
homeaway <- function(df) {
    hconf <- vconf <- leg <- hgoalaet <- vgoalaet <- hpen <- NULL
    vpen <- ga <- Date <- Season <- team <- opp <- gf <- division <- NULL
    tier <- venue <- home <- visitor <- hgoal <- vgoal <- NULL
    
    if ("division" %in% colnames(df)) {
        bind_rows(
            df %>%
                select(Date, Season, team = home,
                          opp = visitor, gf = hgoal,
                          ga = vgoal, division, tier) %>%
                mutate(venue = "home"),
            df %>%
                select(Date, Season, team = visitor,
                          opp = home, gf = vgoal,
                          ga = hgoal, division, tier) %>%
                mutate(venue = "away")) %>%
            arrange(team, Date)
    } else if ("tier" %in% colnames(df)) {
        bind_rows(
            df %>%
            select(Date, Season, team = home,
                          opp = visitor, gf = hgoal,
                          ga = vgoal, tier) %>%
            mutate(venue = "home"),
            df %>% 
            select(Date, Season, team = visitor,
                          opp = home, gf = vgoal,
                          ga = hgoal, tier) %>%
            mutate(venue = "away")) %>%
            arrange(team, Date)
    } else {
        bind_rows(
            df %>%
            select(Date, Season, team = home, opp = visitor,
                          gf = hgoal, ga = vgoal, teamconf = hconf,
                          oppconf = vconf, round, leg, gfaet = hgoalaet,
                          gaaet = vgoalaet, penfaet = hpen, penaaet = vpen) %>%
            mutate(venue = "home"),
            df %>%
            select(Date, Season, team = visitor, opp = home,
                          gf = vgoal, ga = hgoal, teamconf = vconf,
                          oppconf = hconf, round, leg, gfaet = vgoalaet, gaaet = hgoalaet, 
                          penfaet = vpen, penaaet = hpen) %>%
            mutate(venue = "away")) %>%
            arrange(team, Date)
    }
}
