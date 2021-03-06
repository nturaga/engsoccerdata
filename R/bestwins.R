#' Return each team's best wins
#'
#' @param df the results dataset
#' @param teamname team name
#' @param type If \code{=NULL} then all results are returned. If
#' Otherwise valid types are \code{H}, \code{A}
#' relating to home-losses, away-losses
#' @param N The total number of games to return
#' @return a dataframe of games ending in best wins
#' @importFrom magrittr "%>%"
#' @examples
#' bestwins(england,"Everton")
#' bestwins(england,"Aston Villa", type="H")
#' bestwins(england,"York City", type="A")
#' bestwins(england,"Port Vale", N=20)
#'
#' @export
bestwins<-function(df=NULL, teamname=NULL, type=NULL, N=NULL){

    N <- ifelse(is.null(N), 10, N)

    if(is.null(type)) {
        df %>%
            filter(home==teamname & result=="H" | visitor==teamname & result=="A") %>%
            mutate(maxgoal=pmax(hgoal, vgoal), mingoal=pmin(hgoal,vgoal), absgoaldif=abs(hgoal-vgoal)) %>%
            arrange(-absgoaldif,-maxgoal) %>%
            select(Season, home, visitor, FT, division) %>%
            head(N)
    } else {
        df %>%
            filter(home==teamname & result=="H" | visitor==teamname & result=="A") %>%
            mutate(maxgoal=pmax(hgoal, vgoal), mingoal=pmin(hgoal,vgoal), absgoaldif=abs(hgoal-vgoal)) %>%
            arrange(-absgoaldif,-maxgoal) %>%
            filter(result==type) %>%
            select(Season, home, visitor, FT, division) %>%
            head(N)
    }
}
