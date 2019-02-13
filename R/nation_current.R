.get_league_data <-
    function(season, league_code)
{
    football_data_url <- "http://www.football-data.co.uk/mmz4281"
    year1 <- season %% 100
    year2 <- year1 + 1
    path <- paste0(football_data_url, "/",
                   year1, year2, "/",
                   league_code, ".csv")
    read_csv(path)
}


league_current <-
    function(season = 2017, league_code)
{
    df <- bind_rows(
        getCurrentData(.get_league_data(season, league_code),
                       1, Season = season)
    )
}
