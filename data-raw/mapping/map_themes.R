
spp_color <-
  c(
    "Physaria" = "#C0C0C0",  # silver
    "Physaria acutifolia" = "#BDB76B",  # dark khaki
    "Physaria brassicoides" = "#000000",  # black
    "Physaria condensata" =  "#FFFF99",  # canary yellow
    "Physaria dornii" =  "#7FC97F",  # light green
    "Physaria eburniflora" =  "#56B4E9",  # picton blue
    "Physaria integrifolia" =  "#009E73",  # green haze
    "Physaria medicinae" = "#BA55D3",  # orchid
    "Physaria vitulifera" = "#D8BFD8",  # thistle
    "Physaria didymocarpa subsp. didymocarpa" =  "#386CB0",  # azure
    "Physaria didymocarpa subsp. lanata" =  "#666666",  # dark gray
    "Physaria didymocarpa subsp. lyrata" =  "#FDC086",  # salmon
    "Physaria saximontana subsp. dentata" =  "#BF5B17",  # burnt orange
    "Physaria saximontana subsp. saximontana" =  "#F0027F",  # rose
    "Physaria bellii" = "#0000FF",  # blue
    "Physaria chambersii" = "#006400",  # dark green
    "Physaria floribunda subsp. floribunda" = "#8B0000",  # dark red
    "Physaria floribunda subsp. osterhoutii" = "#ff5c5c",  # dark gray
    "Physaria rollinsii" = "#000000",  # black
    "Physaria vitulifera - Carbon" = "#c5ff6e",  # bright green
    "Physaria acutifolia - vitulifera-like" = "#00FFFF",  # cyan
    "Physaria argyraea" = "#000000",
    "Physaria fendleri" = "#000000",
    "Physaria obcordata" = "#000000"
  )

spp_shape <-
  c(
    "Physaria" = 16,  # circle
    "Physaria acutifolia" =  3,  # plus sign
    "Physaria brassicoides" =  16,
    "Physaria condensata" =  18,  # diamond
    "Physaria dornii" =  18,
    "Physaria eburniflora" =  18,
    "Physaria integrifolia" =  16,
    "Physaria medicinae" = 17,
    "Physaria vitulifera" = 18,
    "Physaria didymocarpa subsp. didymocarpa" =  15,  # square
    "Physaria didymocarpa subsp. lanata" =  17,  # triangle
    "Physaria didymocarpa subsp. lyrata" =  17,
    "Physaria saximontana subsp. dentata" =  18,
    "Physaria saximontana subsp. saximontana" = 18,
    "Physaria bellii" = 5, # hollow diamond
    "Physaria chambersii" = 17,
    "Physaria floribunda subsp. floribunda" = 0,  # hollow square
    "Physaria floribunda subsp. osterhoutii" = 5,
    "Physaria rollinsii" = 1,  # hollow circle
    "Physaria acutifolia - vitulifera-like" = 17,
    "Physaria vitulifera - Carbon" = 18,
    "Physaria argyraea" = 5,
    "Physaria fendleri" = 0,
    "Physaria obcordata" = 1
  )

# Save objects as internal R data
usethis::use_data(spp_color, spp_shape, overwrite = TRUE)
