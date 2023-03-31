aesthetics <-
  tibble::tribble(
    ~"species", ~"color", ~"shape", ~"text-color", ~"text-shape",
    "Physaria", "#C0C0C0", 16, "silver", "circle",
    "Physaria acutifolia", "#BDB76B", 3, "dark khaki", "plus sign",
    "Physaria brassicoides", "#000000", 16, "black", "circle",
    "Physaria condensata", "#FFFF99", 18, "canary yellow", "diamond",
    "Physaria dornii", "#7FC97F", 18, "light green", "diamond",
    "Physaria eburniflora", "#56B4E9", 18, "picton blue", "diamond",
    "Physaria integrifolia", "#009E73", 16, "green haze", "circle",
    "Physaria 'medicinae'", "#BA55D3", 17, "orchid", "triangle",
    "Physaria vitulifera", "#D8BFD8", 18, "thistle", "diamond",
    "Physaria didymocarpa subsp. didymocarpa", "#386CB0", 15, "azure", "square",
    "Physaria didymocarpa subsp. lanata", "#666666", 17, "dark gray", "triangle",
    "Physaria didymocarpa subsp. lyrata", "#FDC086", 17, "salmon", "triangle",
    "Physaria saximontana subsp. dentata", "#BF5B17", 18, "burnt orange", "diamond",
    "Physaria saximontana subsp. saximontana", "#F0027F", 18, "rose", "diamond",
    "Physaria bellii", "#0000FF", 5, "blue", "hollow diamond",
    "Physaria chambersii", "#006400", 17, "dark green", "triangle",
    "Physaria floribunda subsp. floribunda", "#8B0000", 0, "dark red", "hollow square",
    "Physaria floribunda subsp. osterhoutii", "#ff5c5c", 5, "dark gray", "hollow diamond",
    "Physaria rollinsii", "#000000", 1, "black", "hollow circle",
    "Physaria vitulifera - Carbon", "#c5ff6e", 18, "bright green", "diamond",
    "Physaria acutifolia - vitulifera-like", "#00FFFF", 17, "cyan", "triangle",
    "Physaria argyraea", "#000000", 5, "black", "hollow diamond",
    "Physaria fendleri", "#000000", 0, "black", "hollow square",
    "Physaria obcordata", "#000000", 1, "black", "hollow circle"
  )

usethis::use_data(aesthetics, overwrite = TRUE)
