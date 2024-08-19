#' Check if font files are accessible
#'
#' This function checks if the specified font files are accessible via system.file().
#' @return A message indicating whether each font file is found or not.
#' @export
check_fonts_access <- function() {
  fonts <- c("PixelOperatorSC.ttf", "000webfont.ttf")
  font_paths <- sapply(fonts, function(font) system.file(paste0("fonts/", font), package = "clessnize"))

  # Vérifier si les chemins ne sont pas vides
  results <- sapply(font_paths, function(path) if (path == "") "Not found" else "Found")
  
  # Créer un message récapitulatif
  message(paste(names(results), ":", results, font_paths, collapse = "\n"))
  
  return("check_fonts_access is done")
}


## put the datagotchi theme here

