#' Total number of unique opponents
#'
#' @param df A results dataframe
#' @param Tier Tier
#' @return a dataframe with teams and frequency of unique opponents
#' @importFrom magrittr "%>%"
#' @examples
#' opponents(england)
#' opponents(england,4)
#'
#' @export
opponents<-function(df=NULL,Tier=NULL){
    if(is.null(Tier))
        
        bind_rows(df %>%
              select(team1=home,team2=visitor),
              df %>%
              select(team1=visitor,team2=home)) %>%
            group_by(team1) %>%
            summarise(Opponents=n_distinct(team2)) %>%
            arrange(-Opponents)


    else {
        bind_rows (df %>%
               select(team1=home,team2=visitor,tier),
               df %>%
               select(team1=visitor,team2=home,tier)) %>%
            filter(tier==Tier) %>%
            group_by(team1) %>%
            summarise(Opponents=n_distinct(team2)) %>%
            arrange(-Opponents)
    }
}
