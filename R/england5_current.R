#' Get current Enland season data for tier 5
#'
#' @return a dataframe with results for current
#' season for top two divisions
#' @param Season the current Season
#' @examples
#' england5_current()
#' @export
england5_current <- function(Season = 2017) {

    myseason <- Season
    s2 <- as.numeric(str_sub(myseason, 3, 4))
    s1 <- s2 + 1

    e1 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", s2, s1, "/EC.csv"))
    df1 <- getCurrentData(e1, 5, 5, Season = myseason)

    df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")
    e5 <- england5
    if (identical(max(df1$Date, na.rm = T), max(e5$Date, na.rm = T)))
        warning("The returned dataframe contains data already included in 'england5' dataframe")
    tm <- teamnames

    df1$home <- tm$name[match(df1$home, tm$name_other)]
    df1$visitor <- tm$name[match(df1$visitor, tm$name_other)]

    df1 <- df1[c(1:7, 9, 8)]
    return(df1)
}
