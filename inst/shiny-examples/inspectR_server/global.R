# https://annielyu.com/2020/02/04/viscover-shiny/

library(shinydashboard)
library(shinyFiles)
library(shiny)
library(rgl)
library(ggplot2)
library(x3ptools)
library(dplyr)
library(bulletxtrctr)
library(shinybusy)
library(shinyAce)
library(formatR)
library(shinyhelper)
library(cmpsR)
library(ggpubr)
# library(shinyRGL)
# library(rintrojs)


# hard_coded_dir <- "inst/data/write_x3p_data_processed"
# investigatR_obj <<- read_bullet(hard_coded_dir)


# shiny.r$data <<- try(read_bullet(hard_coded_dir))
# dataPar <<- try(data_CheckPar(isolate(shiny.r$data)))
# shiny.r$data$type <<- 'NA'
# shiny.r$data$comments <<- ''


options(shiny.maxRequestSize=2*1024^3,
        shiny.reactlog=TRUE,
        width = 105)


# USE_RGL <- FALSE
USE_RGL <- TRUE
PRE_LOAD <- TRUE

# create a temp folder
userdir <- tempfile()
dir.create(userdir, recursive = TRUE)
sapply(file.path(userdir, dir(userdir)[grep("code_", dir(userdir))]), file.remove)

# cat(fs::path_wd())

# source("inst/shiny-examples/inspectR/R/global_helper.R", local = TRUE)
source("R/global_helper.R", local = TRUE)



if (!exists("investigatR_obj")) {

  if (PRE_LOAD) {
    investigatR_obj <<- read_bullet("inst/data/write_x3p_data_processed")
    NOSHINY_TT <- FALSE
  } else {
    investigatR_obj <- tibble()
    NOSHINY_TT <- TRUE
  }
  # LOADED_SHINY_TT <- TRUE
} else {
  NOSHINY_TT <- FALSE
  # LOADED_SHINY_TT <- FALSE
}

init_code_all_R(userdir, NOSHINY_TT)

if(!NOSHINY_TT) {
  if(!assertthat::has_name(investigatR_obj, "type")) { investigatR_obj$type <- 'NA' }
  if(!assertthat::has_name(investigatR_obj, "comments")) { investigatR_obj$comments <- '' }

}


shiny.r <- reactiveValues(data = investigatR_obj)
dataPar <- data_CheckPar(isolate(shiny.r$data))

global_sig1 <- c()
global_sig2 <- c()

onStop(function() {
  rm(shiny.r, dataPar, envir = globalenv())
  rm(global_sig1, global_sig2, userdir, envir = globalenv())
  rm(NOSHINY_TT, USE_RGL, PRE_LOAD, envir = globalenv())
  if(nrow(investigatR_obj) == 0) { rm(investigatR_obj, envir = globalenv()) }
})

# .global <- new.env()
#
# initResourcePaths <- function() {
#   if (is.null(.global$loaded)) {
#     shiny::addResourcePath(
#       prefix = 'ggpaintr',
#       directoryPath = system.file('www', package='ggpaintr'))
#     .global$loaded <- TRUE
#   }
#   HTML("")
# }
#
# webGLOutput <- function(outputId, width="100%", height="400px"){
#   style <- paste("width:", validateCssUnit(width), ";", "height:",
#                  validateCssUnit(height))
#
#   tagList(
#     singleton(tags$head(
#       initResourcePaths(),
#       tags$script(src = 'shinyRGL/CanvasMatrix.js'),
#       tags$script(src = 'shinyRGL/glbinding.js'))
#     ),
#     div(id=outputId, class="shiny-gl-output",
#         style=style)
#   )
# }
#
# renderWebGL <- function(expr, width="auto", height="auto", env = parent.frame(),
#                         quoted = FALSE){
#   func <- exprToFunction(expr, env, quoted)
#   return(function(shinysession, name, ...) {
#     #Open a null RGL device.
#     open3d(useNULL = TRUE)
#     func()
#     prefix <- "gl_output_"
#
#     # Read in WebGL's width and height from the browser
#     if (width == "auto") width <- shinysession$clientData[[paste(prefix,
#                                                                  name, "_width", sep = "")]]
#     if (height == "auto") height <- shinysession$clientData[[paste(prefix,
#                                                                    name, "_height", sep = "")]]
#
#
#     if (is.null(width) || is.null(height) || width <= 0 ||
#         height <= 0) return(NULL)
#
#     if (is.null(width) || !is.numeric(width)){
#       stop("Can't support non-numeric width parameter. 'width' must be in px.")
#     }
#
#     if (is.null(height) || !is.numeric(height)){
#       stop("Can't support non-numeric height parameter. 'height' must be in px.")
#     }
#
#     # Read in current values as they're updated so that we can regenerate
#     # the graph honoring the user's changes to the view, but isolate() so we
#     # don't force a new graph every time the user interacts with it.
#     zoom <-
#       isolate(shinysession$clientData[[paste(prefix, name, "_zoom", sep="")]])
#     fov <-
#       isolate(shinysession$clientData[[paste(prefix, name, "_fov", sep="")]])
#     pan <-
#       isolate(shinysession$clientData[[paste(prefix, name, "_pan", sep="")]])
#
#     if (!is.null(zoom)){
#       par3d(zoom = zoom)
#     }
#     if (!is.null(fov)){
#       par3d(FOV=fov)
#     }
#     if (!is.null(pan)){
#       mat <- matrix(pan, ncol=4)
#       par3d(userMatrix=mat)
#     }
#
#     #generate a random 10 character sequence to represent this file
#     id <- paste(sample(c(letters, LETTERS), 10), collapse="")
#     tempDir <- paste(tempdir(), "/", id, "/", sep="")
#
#     # Write out a template file containing the prefix.
#     # TODO: Work with RGL guys to clean this up.
#     tempFile <- file(file.path(tempdir(), paste(id,".html", sep="")), "w");
#     writeLines(paste("%", id, "WebGL%", sep=""),
#                tempFile)
#     close(tempFile)
#
#     # Write out the WebGL file and read it back in
#     # TODO: Work with RGL guys to clean this process up.
#     writeWebGL(dir=tempDir, snapshot= FALSE,
#                template=file.path(tempdir(),paste(id,'.html', sep="")),
#                height=height, width=width, prefix=id)
#
#     #read in the file
#     lines <- readLines(paste(tempDir, "/index.html", sep=""))
#     #remove canvasMatrix load -- we'll load it elsewhere
#     lines <- lines[-1]
#
#
#     #remove the temporary directory
#     #TODO: Doesn't seem to work in Windows
#     unlink(tempDir, recursive=TRUE)
#     #remove the template file.
#     unlink(paste(tempdir(), id,".html", sep=""))
#
#     rgl.close()
#
#     #return the HTML lines generated by RGL
#     toRet <- paste(lines, collapse="\n")
#     return(list(prefix=id,html=HTML(toRet)))
#   })
# }

my_x3p_output <- function(id, USE_RGL){
  if(USE_RGL) {
    rglwidgetOutput(id)
  } else {
    tagList(
      tags$head(tags$style(
        type="text/css",
        paste("#", id, " img {max-width: 100%; width: 100%; height: auto}", sep = '')
      )),
      imageOutput(id, height='auto')
    )

  }
}


my_pre <- function(string) {
  HTML(paste0("<p>&emsp;", string, "</p>"))
}



