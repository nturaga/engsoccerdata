#' Make an English league table
#'
#' @param df The results dataframe
#' @param Season The Season
#' @param tier The tier - must be included
#' @param division The division
#' @param penalties Whether to account for points penalties
#' @section Notes:
#' This table makes league tables according to the points and tie-breaking procedures
#' that were in place for each league in each year
#' @return a dataframe with a league table
#' @importFrom magrittr '%>%'
#' @examples
#' maketable_eng(df=england,Season=1920,tier=1)
#' maketable_eng(df=england,Season=1947,division='3a',tier=3)
#' maketable_eng(df=england,Season=1975,tier=1)
#' maketable_eng(df=england,Season=1975,tier=2)
#' maketable_eng(df=england,Season=2007,tier=3, penalties=TRUE)
#' @export


maketable_eng <- function(df = NULL, Season = NULL, tier = NULL, division = NULL, penalties = FALSE) {
    
    deductions <- deductions
    
    if (!is.null(division)) {
        df <- df[df$division == division, ]
    }
    
    ## 1981/82 - three points for a win introduced.
    if (Season >= 1981) {
        
        xx <- maketable(df, Season, tier, pts = 3)
        
        if (any(xx$team %in% deductions$team & Season %in% deductions$Season) == T && penalties == T) {
            
            penalty <- deductions[deductions$team %in% xx$team & deductions$Season %in% Season, ]
            ## need if penalties has no rows ... just return xx
            
            if (nrow(penalty) > 0) {
                penalty$newPts <- xx$Pts[match(penalty$team, xx$team)] - penalty$deduction
                newPts <- penalty$newPts[match(xx$team, penalty$team)]
                xx$Pts <- ifelse(!is.na(newPts), newPts, xx$Pts)
                
                ## rearrange by same rules as before...
                xx <- xx %>%
                    arrange(-Pts, -gd, -gf) %>%
                    mutate(Pos = rownames(.)) %>%
                    as.data.frame()
            }
        }
    }
    # 1976/77 - 1980/81 goal difference used in all tiers
    else if (Season >= 1976 & Season < 1981) {
        xx <- maketable(df, Season, tier, pts = 2)
    }
    ## 1974/75 and before goal average.- the number of goals scored divided by the number of goals conceded
    else if (Season <= 1974) {
        xx <- maketable(df, Season, tier, pts = 2)
        xx <- xx %>%
            mutate(gd = gf/ga) %>%
            arrange(-Pts, -gd, -gf) %>%
            mutate(Pos = 1:nrow(xx))
        
        if (any(xx$team %in% deductions$team & Season %in% deductions$Season) == T && penalties == T) {
            penalty <- deductions[deductions$team %in% xx$team & deductions$Season %in% Season, ]
            ## need if penalties has no rows ... just return xx
            if (nrow(penalty) > 0) {
                penalty$newPts <- xx$Pts[match(penalty$team, xx$team)] - penalty$deduction
                newPts <- penalty$newPts[match(xx$team, penalty$team)]
                xx$Pts <- ifelse(!is.na(newPts), newPts, xx$Pts)
                
                ## rearrange by same rules as before...
                xx <- xx %>%
                    arrange(-Pts, -gd, -gf) %>%
                    mutate(Pos = 1:nrow(xx)) %>%
                    as.data.frame()
            }
        }
    } else if (Season == 1975 & tier > 1) {
        xx <- maketable(df, Season, tier, pts = 2)
        xx <- xx %>%
            mutate(gd = gf/ga) %>%
            arrange(-Pts, -gd, -gf) %>%
            mutate(Pos = 1:nrow(xx))
    } else if (Season == 1975 & tier == 1) {
        xx <- maketable(df, Season, tier, pts = 2)
    }
    return(xx)
}
