#' Get current England season data for all tiers
#'
#' @return a dataframe with results for current
#' season for all top four divisions
#' @param Season the current Season
#' @examples
#' england_current()
#' @export
england_current <- function(Season = 2017) {
    .england_current2(.england_current1(Season = Season))
}

## Internal function
.england_current1 <- function(Season = 2017) {

    ## this function is completely bonkers
    ## because of a weird thing with Forest Green / Lincoln City ....

    ## Season number
    s2 <- as.numeric(str_sub(Season, 3, 4))
    s1 <- s2 + 1

    ## Simplify reading in files
    links <- paste0("http://www.football-data.co.uk/mmz4281/",
                    s2, s1, "/",
                    c('E0', 'E1', 'E2', 'E3'), ".csv")

    df <- links %>% map(read_csv) %>% reduce(bind_rows)

    engl <- england
    if (max(dmy(df$Date)) == max(ymd(engl$Date))) {
        warning("The returned tbl contains data  included in 'england' tbl")
    }

    df1 <- tibble(Date = as.character(as.Date(df$Date, "%d/%m/%y")),
                      Season = Season,
                      home = as.character(df$HomeTeam),
                      visitor = as.character(df$AwayTeam),
                      FT = paste0(df$FTHG, "-", df$FTAG),
                      hgoal = df$FTHG,
                      vgoal = df$FTAG,
                      division = as.numeric(factor(df$Div)),
                      tier = as.numeric(factor(df$Div)),
                      totgoal = df$FTHG + df$FTAG, goaldif = df$FTHG - df$FTAG,
                      result = as.character(
                          ifelse(df$FTHG > df$FTAG, "H",
                                 ifelse(df$FTHG < df$FTAG, "A", "D")))
                      )
    df1
}

## Internal function 2
.england_current2 <- function(df1) {

    weird_names <- c("Forest Green", "Lincoln")

    tm <- teamnames %>% filter(name != "Accrington F.C.")

    df1$home <- ifelse(
        !df1$home %in% weird_names,
        as.character(tm$name[match(df1$home, tm$name_other)]),
        as.character(df1$home)
    )

    df1$visitor <- ifelse(
        !df1$visitor %in% weird_names,
        as.character(tm$name[match(df1$visitor, tm$name_other)]),
        as.character(df1$visitor)
    )

    df1$home[df1$home == "Forest Green"] <- "Forest Green Rovers"
    df1$visitor[df1$visitor == "Forest Green"] <- "Forest Green Rovers"
    df1$home[df1$home == "Lincoln"] <- "Lincoln City"
    df1$visitor[df1$visitor == "Lincoln"] <- "Lincoln City"

    df1$Date <- ymd(df1$Date)

    df1
}
