#' Get current France season data for top tier
#'
#' @return a dataframe with results for current
#' season for top division
#' @param Season the current Season
#' @examples
#' france_current()
#' @export




france_current <- function(Season = 2017) {

    s1 <- s2 <- myseason <- f1 <- f2 <- df1 <- NULL
    myseason <- Season
    s2 <- as.numeric(str_sub(myseason, 3, 4))
    s1 <- s2 + 1

    f1 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", s2, s1, "/F1.csv"))
    df1 <- bind_rows(getCurrentData(f1, "F1", 1, Season = myseason))

    df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")
    fran <- france
    if (identical(max(df1$Date), max(fran$Date)))
        warning("The returned dataframe contains data already included in 'france' dataframe")
    tm <- teamnames
    df1$home <- tm$name[match(df1$home, tm$name_other)]
    df1$visitor <- tm$name[match(df1$visitor, tm$name_other)]
    return(df1)
}
