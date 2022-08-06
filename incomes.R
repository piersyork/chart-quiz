library(ggplot2)

theme_set(owidR::theme_owid(import_fonts = FALSE))

income <- readxl::read_xls("~/Downloads/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls",
                           sheet = 6, skip = 4) |>
  janitor::clean_names() |>
  as.data.table()

income[
  ,
  .(annual_income = mean(net_annual_income_before_housing_costs)),
  by = local_authority_name
][
  order(-annual_income)
]


percentile_income <- readODS::read_ods("~/Downloads/Table_3.1_1920.ods", sheet = 2, range = "A5:J33") |>
  # janitor::clean_names() |>
  as.data.table() |>
  melt(id.vars = "Tax year", variable.name = "percentile", value.name = "income")
colnames(percentile_income)[1] <- "tax_year"
percentile_income[, percentile := gsub(" percentile", "", percentile)]
percentile_income[, income := as.integer(income)]
percentile_income[, tax_year := gsub("-\\d{2}$", "", tax_year) |> as.numeric()]

percentile_income[percentile %in% c("1st", "50th", "99th")] |>
  ggplot(aes(tax_year, income, fill = percentile)) +
  geom_col(position = "dodge")

perc_order <- percentile_income[tax_year == 2019, percentile]

percentile_income[tax_year == 2019] |>
  ggplot(aes(forcats::fct_relevel(percentile, perc_order), income, label = income)) +
  geom_col() +
  geom_text(nudge_y = 6000) +
  labs(title = "UK average income by percentile") +
  scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
  theme(axis.title = element_blank())


