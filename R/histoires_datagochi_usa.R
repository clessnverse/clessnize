check_fonts_access <- function() {
  fonts <- c("PixelOperatorSC.ttf", "000webfont.ttf")
  font_paths <- sapply(fonts, function(font) system.file(paste0("fonts/", font), package = "clessnize"))

  # Vérifier si les chemins ne sont pas vides
  results <- sapply(font_paths, function(path) if (path == "") "Not found" else "Found")
  
  # Créer un message récapitulatif
  message <- paste(names(results), ":", results, collapse = "\n")
  
  return(message)
}


## put the datagotchi theme here

