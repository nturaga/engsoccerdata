#' Get current Greece season data for top tier
#'
#' @return a dataframe with results for current
#' season for top division
#' @param Season the current Season
#' @examples
#' greece_current()
#' @export
greece_current <- function(Season = 2017) {

    myseason <- Season
    s2 <- as.numeric(str_sub(myseason, 3, 4))
    s1 <- s2 + 1

    g1 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", s2, s1, "/G1.csv"))

    df1 <- bind_rows(getCurrentData(g1, "G1", 1, Season = myseason))

    df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")
    gree <- greece
    if (identical(max(df1$Date), max(gree$Date)))
        warning("The returned dataframe contains data already included in 'greece' dataframe")
    tm <- teamnames
    df1$home <- tm$name[match(df1$home, tm$name_other)]
    df1$visitor <- tm$name[match(df1$visitor, tm$name_other)]
    return(df1)
}
