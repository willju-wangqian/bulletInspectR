ui <- dashboardPage(

  dashboardHeader(title = "bulletInvestigatR",
                  titleWidth = 300),

  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://forensicstats.org/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='csafe-logo-90.png' width = '186'></a>"
        # "<br>",
        # "<a href='https://github.com/heike/bulletxtrctr/'><p style='text-align:center'>bulletxtrctr</p></a>"
      )),
      menuItem("Introduction", tabName = "info", icon = icon("info")),
      menuItem("Data", icon = icon("table"), tabName = "data_related"),
      menuItem("Analysis", icon = icon("pencil-ruler"), startExpanded = TRUE,
               menuSubItem("Bullet Land Inspection", tabName = "investigation"),
               menuSubItem("Signature Comparison", tabName = "sig_compare"))

    )
  ),
  dashboardBody(
    tabItems(

      tabItem(tabName = "info",
              h2("Welcome to bulletInvestigatR!"),
              fluidRow(
                column(6, style='padding-left:2%;',
                       h3("Introduction"),
                       p("bulletInvestigatR is an R shiny app that provides an interactive way of processing bullet land-engraved area data in x3p format and conducting analysis using the Congruent Matching Profile Segments (CMPS) method. "),
                       h3("Data section"),
                       p("The data section allows users to import and preprocess bullet land-engraved area data in x3p format."),
                       p("x3p data are processed so that further investigations can be conducted."),
                       h4("import data"),
                       HTML("<p>&emsp;- you can import data in x3p format by selecting the folder that contains the x3p files.</p>"),
                       HTML("<p>&emsp;- if you have already processed the x3p files in your R global environment, you can import this R object by naming it investigatR_obj</p>"),
                       HTML("<p>&emsp;- for demonstration purposes, some processed x3p files are loaded when the app starts. </p>"), # check this
                       h4("process x3p files and bullet signatures"),
                       HTML("<p>bullet land-engraved area data in x3p format are processed using the <a href='https://github.com/heike/x3ptools'>x3ptools</a> package and the <a href='https://github.com/heike/bulletxtrctr'>bulletxtrctr</a> package</p>"),
                       HTML("<p>the pipeline of obtaining the bullet signatures can be found in <a href='https://projecteuclid.org/journals/annals-of-applied-statistics/volume-11/issue-4/Automatic-matching-of-bullet-land-impressions/10.1214/17-AOAS1080.full'>Hare et al. (2017)</a></p>"),
                       h4("analysis"),
                       p("On the analysis panel, users can: "),
                       my_pre("- manually update the identified groove locations"),
                       my_pre("- mark and leave comments for each x3p file"),
                       my_pre("- output the processed data"),
                       my_pre("- compute bullet signatures"),
                       my_pre("- compare two bullet signatures using the CMPS method (<a href='https://www.sciencedirect.com/science/article/abs/pii/S0379073819303767'>Chen et al. 2019</a>) and R package cmpsR"),
                       my_pre("- more details about the cmpsR package can be found <a href='https://github.com/willju-wangqian/cmpsR'>here</a>"),
                       # p("obtain 'good' bullet signatures"),
                       # p("- process x3p files"),
                       # p("- compute crosscut, ccdata, and grooves"),
                       # p("- check groove locations and update crosscut values if needed"),
                       # p("- save/load your work; output as a csv file (todo)"),
                       # h4("reproducible"),
                       # p("- automatically generate R codes"),
                       # p("- use these R codes one could obtain the exactly same object"),
                       # p("- output/download the R codes (todo)"),
                       # h4("extensible"),
                       # p("- plug in different modules to extend the functionality of this APP"),
                       # p("- modules are essentially functions(x3ptools, bulletxtrctr) calls"),
                )

              ),


      ),

      # Second tab content
      tabItem(tabName = "data_related",
              # h2("play with x3p files"),

              tabsetPanel(
                # tabBox(
                tabPanel("Import",

                         br(),
                         br(),

                         box(
                           p("Please select the folder containing the x3p files."),
                           shinyDirButton("x3pdir", "Import", "Upload"),
                           width = NULL, title = "From x3p files", solidHeader = TRUE),

                         box(
                           p("Please select a rds file of your bullet object."),
                           shinyFilesButton("file1", "Import",
                                            "Please select a rds file (your bullet object); column x3p/grooves is expected",
                                            multiple = FALSE, viewtype = "detail"),
                           verbatimTextOutput("file1_prompt"),
                           width = NULL, title = "From a rds file", solidHeader = TRUE),

                         # box(
                         #   p("If you create investigatR_obj in your current environment ..."),
                         #   width = NULL, title = "From R Session", solidHeader = TRUE),

                ),
                tabPanel("Process x3p",

                         br(),
                         br(),

                         fluidRow(
                           box(fluidRow(
                             column(6,
                                    p(strong("Desired status of a bullet land-engraved area (LEA) in the x3p file:")),
                                    HTML("<img style = 'display: block; margin-left: auto; margin-right: auto;' src='example_LEA.png' width = '100%'>"),
                                    # box(imageOutput('eg_LEA', width = '100px', inline = T))
                             ),

                             column(6, offset = 0,
                                    tags$head(tags$style(
                                      type="text/css",
                                      "#x3prgl2 img {max-width: 100%; width: 100%; height: auto}"
                                    )),
                                    p(strong("Current status:")),
                                    imageOutput('x3prgl2', height = 'auto'),
                                    conditionalPanel(condition = "output.hasname_x3p",
                                                     actionButton("displayx3p2", "Display x3p image")),
                             )
                           ), width = 12)
                         ),

                         fluidRow(
                           column(4,
                                  box(p("Show information about the x3p file"),
                                      x3pActionButtonUI("x3p_show_xml", "calling print on x3p files"),
                                      width = 12)),
                           column(4,
                                  box(p("One of the major changes between the previous two ISO standards is the way the y axis is defined in a scan. The entry (0,0) used to refer to the top left corner of a scan, now it refers to the bottom right corner, which means that all legacy x3p files have to flip their y axis in order to conform to the newest ISO norm."),
                                      p(strong("click the button below to flip the y-axis:")),
                                      x3pActionButtonUI("x3p_flip", "calling x3p_flip_y"),
                                      width = 12)),
                           column(4,
                                  box(p("Sample from an x3p object"),
                                      x3p_sampleUI("x3p_sample", "calling x3p_sample"),
                                      width = 12))
                         ),

                         fluidRow(
                           column(4,
                                  box(p("ISO standard 5436_2 asks for specification of values in meters. For topographic surfaces collected by microscopes values in microns are more readable. Besides scaling the values in the surface matrix, corresponding increments are changed to microns as well."),
                                      p(strong("click the button below to change from meters to microns")),
                                      x3pActionButtonUI("x3p_m_to_mum", "calling x3p_m_to_mum"),
                                      width = 12)),
                           column(4,
                                  box(p("Rotate the surface matrix of an x3p object. Also adjust meta information."),
                                      x3p_rotateUI("x3p_rotate", "calling x3p_rotate"),
                                      # actionButton("nnthing1", "Click for nothing 2"),
                                      width = 12))

                         ),

                ),

                tabPanel("Prepare and Check",

                         br(),
                         br(),

                         fluidRow(
                           column(6,
                                  box(
                                    p("To investigate the data, a crosscut location and groove locations are identified with default settings."),
                                    p("Please make sure that"),
                                    HTML("<p>&emsp;- the x3p files have been changed to the desired status</p>"),
                                    HTML("<p>&emsp;- the unit has been converted to microns</p>"),
                                    p("An R object named investigatR_obj will be created in the environment once the computation is finished."),
                                    p("investigatR_obj collects all x3p objects and the computed results."),
                                    p("This might take some time if a large number of x3p files are processed."),
                                    p(strong("click the button below to compute the crosscut and grooves")),
                                    x3pActionButtonUI("prepare_shinytt", "compute"),
                                    width = NULL
                                  ),
                           ),
                           column(6,
                                  box(
                                    p(strong("Check if all components needed for investigation are properly obtained")),

                                    actionButton("ttcheck", "check", icon = icon("search")),
                                    verbatimTextOutput("checkresult"),
                                    verbatimTextOutput("suggest"),
                                    width = NULL
                                  ),
                           )
                         ),
                )
              ),

              fluidRow(
                column(4,
                       box(
                         actionButton("updateCode", "update code", width = "100%",
                                      style="white-space: normal;text-align:center;"),
                         br(),
                         actionButton("clean_code_window", "clean codes and the loaded object",
                                      width = "100%", style="white-space: normal;text-align:center;"),
                         br(),
                         downloadButton("download_codes", label = "Download R codes"),

                         width = NULL
                       )

                ),

                column(8,
                       aceEditor("myEditor", "", mode = "r", readOnly = TRUE, theme = "chrome")
                )
              ),


              # rglwidgetOutput("x3prgl2"),
              # playwidgetOutput("x3prgl2"),
              # webGLOutput("x3prgl2"),



      ),

      # First tab content
      tabItem(tabName = "investigation",
              # h3("Select a Bullet Land"),

              tabsetPanel(

                tabPanel("Main",

                         fluidRow(
                           column(2,
                                  p("Note that: x3p files can be selected by either an index or its Scan ID"),
                                  uiOutput("selectk"),
                                  uiOutput("selectid"),
                                  br(),
                                  # verbatimTextOutput("ccvalue"),
                                  htmlOutput("ccvalue"),
                                  numericInput("cc", "change crosscut to:", 100, min = 0, max = 1000),
                                  actionButton("updateCC", "Update Crosscut Value", width = "100%",
                                               style="white-space: normal;text-align:center;")
                                  # %>%
                                  #   helper(type = "markdown",
                                  #          content = "test"),
                           ),
                           column(6,
                                  textOutput("groovelocations"),
                                  plotOutput("groovePlot", click = "plot_click", height="300px"),
                                  br(),
                                  actionButton("confirm", "Confirm",
                                               style="white-space: normal;text-align:center;"),
                                  br(),
                                  uiOutput("x3p_type_select_ui"),
                                  uiOutput("x3p_comment_box_ui"),
                                  br(),
                                  actionButton("saveCurrentEnv", "Save your progress!", width = "100%",
                                               style="white-space: normal;text-align:center;"),
                                  br(),
                                  textOutput("saveptp")
                           ),
                           column(4,
                                  conditionalPanel(
                                    condition = "output.hasname_x3p",
                                    # rglwidgetOutput("x3prgl"),
                                    my_x3p_output("x3prgl", USE_RGL),
                                    br(),
                                    actionButton("displayx3p", "Display x3p image", width = "30%",
                                                 style="white-space: normal;text-align:center;"),
                                    # %>%
                                    #   helper(type = "inline",
                                    #          title = "Inline Help",
                                    #          content = c("This helpfile is defined entirely in the UI!",
                                    #                      "This is on a new line.",
                                    #                      "This is some <b>HTML</b>."),
                                    #          size = "s"),
                                    br(),
                                    br(),
                                    plotOutput("sigPlot"),
                                    br(),
                                    actionButton("drawsig", "Draw Signature", width = "30%",
                                                 style="white-space: normal;text-align:center;")

                                  ),
                           )
                         )
                ),

                tabPanel("Output",
                         fluidRow(
                           column(6,
                                  box(
                                    textInput("save_csv_id", "Your Manual Code", value = "", width = NULL,
                                              placeholder = TRUE),
                                    textInput("save_csv_study_name", "Name of the Study", value = "", width = NULL,
                                              placeholder = TRUE),
                                    actionButton("cc_status", "Check Crosscuts", width = "100%",
                                                 style="white-space: normal;text-align:center;"),
                                    downloadButton("save_csv", "Output a CSV file",
                                                   style="white-space: normal;text-align:center;"),
                                    width = NULL, title = "Output as a csv file", solidHeader = TRUE)),
                           column(6,
                                  box(downloadButton("downloadData", "Save the Processed Data as a RDS file.",
                                                     style="white-space: normal;text-align:center;"),
                                      p("It might take a few seconds to prepare for the saving."),
                                      width = NULL, title = "Output as a rds file", solidHeader = TRUE))
                         )

                )

              ),

      ),



      tabItem(tabName = "sig_compare",
              h2("Compute two signatures"),

              fluidRow(
                column(2,
                       uiOutput("sig1_select"),
                       br(),
                       uiOutput("sig2_select"),
                ),
                column(5,
                       box(
                         plotOutput("sig1_plot", height = "300px"),
                         br(),
                         actionButton("sig1_draw", "Sig1: draw signature"),
                         br(),
                         plotOutput("sig2_plot", height = "300px"),
                         br(),
                         actionButton("sig2_draw", "Sig2: draw signature"),
                         width = NULL
                       ),),
                column(5,
                       box(
                         my_x3p_output("sig1_display_rgl", USE_RGL),
                         br(),
                         actionButton("sig1_display", "Sig1: display x3p"),
                         br(),
                         my_x3p_output("sig2_display_rgl", USE_RGL),
                         br(),
                         actionButton("sig2_display", "Sig2: display x3p"),
                         br(),
                         width = NULL
                       ),),
              ),

              h2("Use cmpsR to compare the two signatures and compute the CMPS score"),

              fluidRow(
                column(2,
                       numericInput("cmps_seg_length", "length of a basis segment:", 50, min = 1, max = 3000),
                       textInput("cmps_npeaks_set", "number of peaks", "5, 3, 1"),
                       numericInput("cmps_Tx", "length of the tolerance zone", 25, min = 1, max = 3000),
                       textInput("outlength", "segment length at each scale level", ""),
                       # actionButton("sig_align", "Draw both signatures"),
                       actionButton("compute_cmps", "compute cmps"),
                ),
                column(5,
                       box(
                         plotOutput("cmps_signature_plot", height = "300px"),
                         verbatimTextOutput("cmps_score_text", placeholder = TRUE),
                         width = NULL
                       ),
                ),
                column(5,
                       uiOutput("cmps_segment_ui"),
                )


              )


      )

    )
  )
)



























