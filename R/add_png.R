library(magick)

#' Add a PNG image on top of another
#'
#' @param base_png_path Path to the base PNG image
#' @param png_to_add_path Path to the PNG image to add on top
#' @param output_path Path where the combined image will be saved
#' @param x_position Position from right edge in pixels
#' @param y_position Position from top edge in pixels
#' @param scale Scaling factor for the image to add (1 = original size, 0.5 = half size)
#'
#' @return The combined image object (invisibly)
#' @export
add_png <- function(base_png_path, png_to_add_path, output_path, x_position = 20, y_position = 20, scale = 1) {
  # First, check if files exist
  if (!file.exists(base_png_path)) {
    stop("Base image file does not exist: ", base_png_path)
  }
  if (!file.exists(png_to_add_path)) {
    stop("Overlay image file does not exist: ", png_to_add_path)
  }
  
  # Clean up memory to avoid cache resources exhaustion
  gc()
  
  # Try to read with explicit density setting to reduce memory use
  tryCatch({
    # Read the base image with lower density
    base_img <- image_read(base_png_path, density = "72x72")
    
    # Read the image to add with lower density
    overlay_img <- image_read(png_to_add_path, density = "72x72")
    
    # Calculate the new dimensions based on scale
    overlay_width <- round(image_info(overlay_img)$width * scale)
    overlay_resized <- image_scale(overlay_img, paste0(overlay_width, "x"))
    
    # Calculate position from right and top
    x_pos <- image_info(base_img)$width - image_info(overlay_resized)$width - x_position
    y_pos <- y_position
    
    # Add the overlay image
    result_img <- image_composite(
      base_img, 
      overlay_resized, 
      offset = paste0("+", x_pos, "+", y_pos)
    )
    
    # Save the result
    image_write(result_img, path = output_path)
    
    # Clean up to free memory
    rm(base_img, overlay_img, overlay_resized, result_img)
    gc()
    
    # Return success
    return(invisible(TRUE))
  }, error = function(e) {
    # Print more informative error
    message("Error processing images: ", e$message)
    message("This may be due to insufficient memory or cache resources.")
    message("Try reducing the image sizes or using another image processing approach.")
    
    # Force garbage collection
    gc()
    
    # Re-throw the error
    stop(e)
  })
}

#' Add multiple PNG images on top of a base image at once
#'
#' @param base_png_path Path to the base PNG image
#' @param output_path Path where the final combined image will be saved
#' @param png_list A list of lists, each containing image info with the following elements:
#'        - path: Path to the PNG image to add
#'        - x_position: Position from right edge in pixels
#'        - y_position: Position from top edge in pixels
#'        - scale: Scaling factor for the image (1 = original size)
#'
#' @return The combined image object (invisibly)
#' @export
add_multiple_pngs <- function(base_png_path, output_path, png_list) {
  # First, check if base file exists
  if (!file.exists(base_png_path)) {
    stop("Base image file does not exist: ", base_png_path)
  }
  
  # Validate png_list format
  if (!is.list(png_list) || length(png_list) == 0) {
    stop("png_list must be a non-empty list of image specifications")
  }
  
  # Clean up memory to avoid cache resources exhaustion
  gc()
  
  # Try to read with explicit density setting to reduce memory use
  tryCatch({
    # Read the base image with lower density
    result_img <- image_read(base_png_path, density = "72x72")
    
    # Process each overlay image in sequence
    for (img_info in png_list) {
      # Check if required fields exist
      if (!("path" %in% names(img_info))) {
        warning("Skipping an image with missing path")
        next
      }
      
      # Set default values if not provided
      if (!file.exists(img_info$path)) {
        warning("Overlay image file does not exist: ", img_info$path)
        next
      }
      
      x_position <- img_info$x_position %||% 20  # Default x position
      y_position <- img_info$y_position %||% 20  # Default y position
      scale <- img_info$scale %||% 1             # Default scale
      
      # Read the overlay with lower density
      overlay_img <- image_read(img_info$path, density = "72x72")
      
      # Calculate the new dimensions based on scale
      overlay_width <- round(image_info(overlay_img)$width * scale)
      overlay_resized <- image_scale(overlay_img, paste0(overlay_width, "x"))
      
      # Calculate position from right and top
      x_pos <- image_info(result_img)$width - image_info(overlay_resized)$width - x_position
      y_pos <- y_position
      
      # Add the overlay image
      result_img <- image_composite(
        result_img, 
        overlay_resized, 
        offset = paste0("+", x_pos, "+", y_pos)
      )
      
      # Clean up to free memory
      rm(overlay_img, overlay_resized)
      gc()
    }
    
    # Save the final result
    image_write(result_img, path = output_path)
    
    # Clean up to free memory
    rm(result_img)
    gc()
    
    # Return success
    return(invisible(TRUE))
  }, error = function(e) {
    # Print more informative error
    message("Error processing images: ", e$message)
    message("This may be due to insufficient memory or cache resources.")
    message("Try reducing the image sizes or using another image processing approach.")
    
    # Force garbage collection
    gc()
    
    # Re-throw the error
    stop(e)
  })
}

# Helper function for NULL coalescing (similar to %||% in tidyverse)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
