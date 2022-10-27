###################################
# compare two signatures
output$sig1_select <- renderUI({
  if(kk() <= dataPar$n) {
    selectInput("sig1_idx","Sig1: investigate kth land:", selected = 1,
                choices=1:dataPar$n,
                width = "100%")
  } else {
    NULL
  }

})

output$sig2_select <- renderUI({
  if(kk() <= dataPar$n) {
    selectInput("sig2_idx","Sig2: investigate kth land:", selected = 1,
                choices=1:dataPar$n,
                width = "100%")
  } else {
    NULL
  }

})

observeEvent(input$sig1_display, {
  req(shiny.r$data)
  req(input$sig1_idx)

  k <- as.numeric(input$sig1_idx)

  tmp <- isolate(shiny.r$data)

  if (USE_RGL) {
    output$sig1_display_rgl <- renderRglwidget({
      if(isolate(dataPar$hasname_crosscut)){
        image_x3p(
          tmp$x3p[k][[1]] %>% x3p_add_hline(
            yintercept = tmp$crosscut[k], size = 10))
      } else {
        image_x3p(tmp$x3p[k][[1]])
      }
    })
  } else {
    output$sig1_display_rgl <- renderImage({
      outfile <- tempfile(fileext = '.png')

      if(isolate(dataPar$hasname_crosscut)){
        image_x3p(tmp$x3p[k][[1]] %>% x3p_add_hline(
          yintercept = tmp$crosscut[k], size = 10),
          file = outfile)
      } else {
        image_x3p(tmp$x3p[k][[1]], file = outfile)
      }

      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  }

})

observeEvent(input$sig2_display, {
  req(shiny.r$data)
  req(input$sig2_idx)

  k <- as.numeric(input$sig2_idx)

  tmp <- isolate(shiny.r$data)

  if (USE_RGL) {
    output$sig2_display_rgl <- renderRglwidget({
      if(isolate(dataPar$hasname_crosscut)){
        image_x3p(
          tmp$x3p[k][[1]] %>% x3p_add_hline(
            yintercept = tmp$crosscut[k], size = 10))
      } else {
        image_x3p(tmp$x3p[k][[1]])
      }
    })
  } else {
    output$sig2_display_rgl <- renderImage({
      outfile <- tempfile(fileext = '.png')

      if(isolate(dataPar$hasname_crosscut)){
        image_x3p(tmp$x3p[k][[1]] %>% x3p_add_hline(
          yintercept = tmp$crosscut[k], size = 10),
          file = outfile)
      } else {
        image_x3p(tmp$x3p[k][[1]], file = outfile)
      }

      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  }

})

observeEvent(input$sig1_draw, {
  k <- as.numeric(isolate(input$sig1_idx))

  # update: compute ccdata if null
  tmp.tt <- shiny.r$data %>% slice(k)

  if (!assertthat::has_name(tmp.tt, "ccdata")) {
    tmp.tt <- tmp.tt %>% mutate(
      ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut),
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)})
    )
  } else {
    tmp.tt <- tmp.tt %>% mutate(
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
      }))
  }

  global_sig1 <<- tmp.tt$sigs[[1]]$sig

  output$sig1_plot <- renderPlot({
    k <- kk()
    p <- tmp.tt$sigs[[1]] %>% filter(!is.na(sig), !is.na(raw_sig)) %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = raw_sig), colour = "grey70") +
      geom_line(aes(y = sig), colour = "grey30") +
      ylab("value") + ylim(c(-7.5, 7.5)) + theme_bw()
    p
  })

})

observeEvent(input$sig2_draw, {
  k <- as.numeric(isolate(input$sig2_idx))

  # update: compute ccdata if null
  tmp.tt <- shiny.r$data %>% slice(k)

  if (!assertthat::has_name(tmp.tt, "ccdata")) {
    tmp.tt <- tmp.tt %>% mutate(
      ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut),
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)})
    )
  } else {
    tmp.tt <- tmp.tt %>% mutate(
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
      }))
  }

  global_sig2 <<- tmp.tt$sigs[[1]]$sig

  output$sig2_plot <- renderPlot({
    k <- kk()
    p <- tmp.tt$sigs[[1]] %>% filter(!is.na(sig), !is.na(raw_sig)) %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = raw_sig), colour = "grey70") +
      geom_line(aes(y = sig), colour = "grey30") +
      ylab("value") + ylim(c(-7.5, 7.5)) + theme_bw()
    p
  })

})


observeEvent(input$compute_cmps, {

  req(global_sig1, global_sig2,
      input$cmps_npeaks_set, input$cmps_seg_length,
      input$cmps_Tx)
  aa <- sig_align(global_sig1, global_sig2)

  sig1 <- aa[['lands']][['sig1']]
  sig2 <- aa[['lands']][['sig2']]

  # browser()

  if (is.null(input$cmps_outlength)) {
    outlength <- NULL
  } else {
    outlength <- input$cmps_outlength %>%
      strsplit(split = ',') %>% unlist() %>% as.numeric()
  }


  if (is.null(input$cmps_npeaks_set)) {
    npeaks_set <- NULL
  } else {
    npeaks_set <- input$cmps_npeaks_set %>%
      strsplit(split = ',') %>% unlist() %>% as.numeric()
  }

  seg_length <- input$cmps_seg_length
  Tx <- input$cmps_Tx

  if (length(npeaks_set) == 0) npeaks_set <- NULL
  if (length(outlength) == 0) outlength <- NULL

  global_cmps_results <<- extract_feature_cmps(
    sig1, sig2,
    seg_length = seg_length,
    npeaks_set = npeaks_set,
    outlength = outlength,
    Tx = Tx,
    include = 'full_result'
  )

  output$cmps_signature_plot <- renderPlot({

    req(global_cmps_results)
    signature_plot <- cmps_signature_plot(global_cmps_results)

    signature_plot$signature_shift_plot

  })

  output$cmps_score_text <- renderText({
    req(global_cmps_results)

    paste("The CMPS score is:", global_cmps_results$CMPS_score)

  })

  output$cmps_segment_ui <- renderUI({

    req(global_cmps_results)
    nseg <- global_cmps_results$nseg

    tagList(
      numericInput('cmps_segment_idx',
                   'choose a basis segment for further investigation',
                   value = 1, min = 1, max = nseg),
      actionButton('cmps_segment_draw_plot', 'investigate'),
      plotOutput("cmps_segment_plot", height = "300px"),
    )

  })

})

observeEvent(input$cmps_segment_draw_plot, {
  req(global_cmps_results, input$cmps_segment_idx)

  num_levels <- length(global_cmps_results$parameters$npeaks_set)

  segment_plot <- cmps_segment_plot(global_cmps_results, seg_idx = round(input$cmps_segment_idx))
  pp <- ggarrange(plotlist = unlist(segment_plot, recursive = FALSE), nrow = num_levels, ncol = 2)

  output$cmps_segment_plot <- renderPlot({
    pp
  })

})



observeEvent(input$sig_align, {

  sig1_shift <- isolate(input$sig1_align_num)
  sig2_shift <- isolate(input$sig2_align_num)

  if(is.null("global_sig1") | is.null("global_sig2")) {
    p <- NULL
  } else if(!is.null(global_sig1) & !is.null(global_sig2)) {
    aa <- sig_align(global_sig1, global_sig2)
    p <- aa$lands %>% ggplot() +
      geom_line(aes(x = x + sig1_shift, y=sig1), color = "red") +
      geom_line(aes(x = x + sig2_shift, y=sig2), color = "blue") + theme_bw()
  }


  output$sig_align_plot <- renderPlot({
    p
  })



})


























