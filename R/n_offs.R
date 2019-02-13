#' Return Scorelines that only occurred n times in history
#'
#' note don't try this with 0 - won't return infinite possible scorelines
#' If you pick crazy numbers for the number of times e.g. 34223,
#' you'll get an error message this function is meant for small
#' numbers of occurrences
#'
#' @param df The results dataframe
#' @param N The Number of instances
#' @param Tier Tier
#' @return a dataframe with summary of results.
#' @importFrom magrittr '%>%'
#' @examples
#' n_offs(england, 1) #return results that have occurred only once across all four tiers
#' n_offs(england, 2) #return results that have occurred only twice across all four tiers
#' n_offs(england, 3) #return results that have occurred 3 times across all four tiers
#'
#' n_offs(england, 1, 1) #return which results have occurred only once in the top tier
#' n_offs(england, 1, 4) #return which results have occurred only once in the 4th tier
#' n_offs(england, 2, 2) #return which results have occurred twice in the 2nd tier
#' n_offs(england, 5, 3) #return which results have occurred five times in the 3rd tier
#'
#' @export
n_offs <- function(df = NULL, N = NULL, Tier = NULL) {
    
    if (is.null(Tier)) 
        df %>%
            mutate(goaldif = hgoal - vgoal,
                          result = ifelse(hgoal > vgoal,
                                          "H", ifelse(hgoal < vgoal, "A", "D"))) %>%
            group_by(FT) %>% 
            tally() %>%
            filter(n == N) %>%
            select(FT) %>%
            left_join(df) %>%
            select(Date, Season, home, visitor, 
                          FT, division, tier, result) %>%
            arrange(FT, Season)
    else {
        dfTier <- df %>%
            mutate(goaldif = hgoal - vgoal,
                          result = ifelse(hgoal > vgoal,
                                          "H", ifelse(hgoal < vgoal, "A", "D"))) %>%
            filter(tier == Tier)
        
        dfTier %>%
            group_by(FT) %>%
            tally() %>%
            filter(n == N) %>%
            select(FT) %>%
            left_join(dfTier) %>%
            select(Date, Season, home,
                          visitor, FT, division, tier, result) %>%
            arrange(FT, Season)
    }
}
