#' Get current Scotland season data for all tiers
#'
#' @return a dataframe with results for current
#' season for top four divisions
#' @param Season the current Season
#' @examples
#' scotland_current()
#' @export
scotland_current <- function(Season = 2017) {
    myseason <- Season
    ss2 <- as.numeric(str_sub(myseason, 3, 4))
    ss1 <- ss2 + 1

    s1 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", ss2, ss1, "/SC0.csv"))
    s2 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", ss2, ss1, "/SC1.csv"))
    s3 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", ss2, ss1, "/SC2.csv"))
    s4 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", ss2, ss1, "/SC3.csv"))

    df1 <- bind_rows(getCurrentData(s1, "SCO", 1, Season = myseason),
                 getCurrentData(s2, "SC1", 2, Season = myseason),
                 getCurrentData(s3, "SC2", 3, Season = myseason),
                 getCurrentData(s4, "SC3", 4, Season = myseason))

    df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")
    scot <- scotland
    if (identical(max(df1$Date), max(scot$Date)))
        warning("The returned dataframe contains data already included in 'scotland' dataframe")
    tm <- teamnames
    df1$home <- tm$name[match(df1$home, tm$name_other)]
    df1$visitor <- tm$name[match(df1$visitor, tm$name_other)]
    return(df1)
}
