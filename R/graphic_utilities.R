#' Generate animated gif from set of static images
#'
#' @param file_names A vector of file names for the image files
#' @param output_file File name/path for output
#' @param delay A number that measures the delay in 1/100 seconds.
#'              Default is 125.
#' @return Returns animated gif saved as output_file
#' @examples
#' animate_plot(c("image1.png", "image2.png", "image3.png"), "animated_image.gif", 100)
#' @export

animate_plot <- function(file_names, output_file, delay = 125) {

  image_read(file_names) %>%
    image_animate(delay = delay) %>%
    image_write(output_file)

}


#' Generate PDF file from grViZ object
#'
#' @param gv An object of type grViz from DiagrammeR package
#' @param output_file File name/path for pdf
#' @return Returns a pdf saved as output_file
#' @examples
#' library(dm)
#' a <- dm(mtcars, iris) %>% dm_draw(rankdir = "LR", view_type = "all")
#' convert_svg_pdf(a, "./example.pdf")
#' @export
#'
convert_svg_pdf <- function(gv, output_file) {

  gv %>% export_svg %>% charToRaw %>% rsvg_pdf(output_file)

}
