covid <- owidR::owid_covid() |>
  as.data.table()

out <- covid[, .(iso_code, location, date, total_cases, new_cases, total_deaths, new_deaths, population)]

readr::write_csv(out, "~/Downloads/covid.csv")

# Connect to the default postgres database
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "postgres", host = "database-1.cshnmell2nzk.eu-west-2.rds.amazonaws.com",
                 user      = "postgres",
                 password      = rstudioapi::askForPassword("Database password"))

dbWriteTable(con, name = "covid", value = covid, overwrite = TRUE)


owidR::owid_search("^gdp")

gdp <- owidR::owid("real-gdp-per-capita-PWT", rename = "gdp_per_capita")


