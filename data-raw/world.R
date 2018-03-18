world <- maps::map(plot = FALSE)
world <- tibble::as_tibble(world[c("x", "y")])
usethis::use_data(world)
