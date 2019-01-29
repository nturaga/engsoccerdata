#' Get current Portugal season data for top tier
#'
#' @return a dataframe with results for current
#' season for top division
#' @param Season the current Season
#' @examples
#' portugal_current()
#' @export

portugal_current <- function(Season = 2017) {

    s1 <- s2 <- myseason <- p1 <- df1 <- NULL
    myseason <- Season
    s2 <- as.numeric(str_sub(myseason, 3, 4))
    s1 <- s2 + 1

    p1 = read_csv(paste0("http://www.football-data.co.uk/mmz4281/", s2, s1, "/P1.csv"))
    df1 <- bind_rows(getCurrentData(p1, "P1", 1, Season = myseason))
    df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")
    port <- portugal
    if (identical(max(df1$Date), max(port$Date)))
        warning("The returned dataframe contains data already included in 'portugal' dataframe")
    tm <- teamnames
    df1$home <- tm$name[match(df1$home, tm$name_other)]
    df1$visitor <- tm$name[match(df1$visitor, tm$name_other)]
    return(df1)
}
