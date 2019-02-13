#' Make a league table
#'
#' @param df The results dataframe
#' @param Season The Season
#' @param tier The tier
#' @param pts Points for a win. Default is 3.
#' @section Notes:
#' The table that is produced is based upon 3 points for a win (unless otherwise
#' defined), 1 for a draw and 0 for a loss.  The table is sorted based upon descending
#' GD and then descending GF as tie-breakers. Different leagues have had different
#' methods for tie-breaks over the years. This league table is a simple generic one.
#' It also does not evaluate points deducted from teams or if games were  artificially
#' awarded to one side based on games not being played.
#' @return a dataframe with a league table
#' @importFrom magrittr '%>%'
#' @examples
#' maketable(df=england,Season=2013,tier=1,pts=3)
#' @export
maketable <- function(df = NULL, Season = NULL, tier = NULL, pts = 3) {
    
    dfx <- df[(df$Season == Season & df$tier == tier), ]
    
    temp <- bind_rows(
        dfx %>%
        select(team = home, opp = visitor, GF = hgoal, GA = vgoal),
        dfx %>%
        select(team = visitor, opp = home, GF = vgoal, GA = hgoal)) %>%
        mutate(GD = GF - GA) %>%
        group_by(team) %>%
        summarise(GP = sum(GD <= 100),
                         W = sum(GD > 0),
                         D = sum(GD == 0),
                         L = sum(GD < 0),
                         gf = sum(GF),
                         ga = sum(GA),
                         gd = sum(GD)) %>%
        mutate(Pts = (W * pts) + D) %>%
        arrange(-Pts, -gd, -gf) %>% 
        mutate(Pos = rownames(.)) %>% as.data.frame()
    return(temp)
}
