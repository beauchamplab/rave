
dashboardControl = function (...,
                             disable = FALSE,
                             collapsed = FALSE)
{
  dataValue <- shiny::restoreInput(id = "sidebarCollapsed",
                                   default = collapsed)
  if (disable)
    dataValue <- TRUE
  dataValueString <- if (dataValue)
    "true"
  else
    "false"
  tagList(
    tags$aside(
      class = "control-sidebar control-sidebar-dark",
      `data-collapsed` = dataValueString,
      div(
        class = 'tab-content',
        ...
      )
    ),
    div(class = "control-sidebar-bg")
  )
}


dashboardHeader <- function (..., title = NULL, titleWidth = NULL, disable = FALSE, btn_text_right = 'Controls',
                             .list = NULL)
{
  items <- .list
  titleWidth <- validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_",
                                                 titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n        .main-header > .navbar {\n          margin-left: _WIDTH_;\n        }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  }
  tags$header(
    class = "main-header", custom_css, style = if (disable)
      "display: none;", span(class = "logo", title),
    tags$nav(
      class = "navbar navbar-static-top",
      role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
      div(
        class = 'navbar-collapse pull-left collapse',
        id="navbar-collapse", `aria-expanded`="false",
        tags$ul(
          class = 'nav navbar-nav',
          tags$li(
            a(href = "#", class = "nav-item nav-link force-recalculate",
              `data-toggle` = "offcanvas",
              role = "button", span(class = "sr-only", "Toggle navigation"),
              shiny::icon('th'), span('Modules')
            )
          ),
          tags$li(
            a(href = "#", class = "nav-item nav-link force-recalculate",
              `data-toggle` = "rave-toggle-inputs",
              role = "button", span(class = "sr-only", "Toggle input panel"),
              shiny::icon('keyboard-o'), span('Input Panels')
            )
          ),
          ...
        )
      ),
      
      div(class = "navbar-custom-menu",
          tags$ul(
            class = "nav navbar-nav",
            # items %>%
            #   tagList(),
            tagList( items ),
            tags$li(
              a(href = "#", class = "nav-item nav-link force-recalculate",
                `data-toggle` = "control-sidebar",
                role = "button",
                span(class = "sr-only", "Toggle control"),
                span(btn_text_right)
              )
            )
          )
      )
    )
  )
}

dashboardPage <- function (
  header, sidebar, control, body, title = NULL,
  skin = c("blue", "black", "purple", "green", "red", "yellow"), controlbar_opened = FALSE,
  initial_mask = NULL
){
  skin <- match.arg(skin)
  extractTitle <- function(header) {
    x <- header$children[[2]]
    if (x$name == "span" && !is.null(x$attribs$class) &&
        x$attribs$class == "logo" && length(x$children) !=
        0) {
      x$children[[1]]
    }
    else {
      ""
    }
  }
  title <- ifelse(is.null(title), extractTitle(header), title)
  content <- div(class = "wrapper",
                 header, sidebar, body, control)
  
  findAttribute = function (x, attr, val)
  {
    if (is.atomic(x))
      return(FALSE)
    if (!is.null(x$attribs[[attr]])) {
      if (identical(x$attribs[[attr]], val))
        return(TRUE)
      else return(FALSE)
    }
    if (length(x$children) > 0) {
      return(any(unlist(lapply(x$children, findAttribute, attr,
                               val))))
    }
    return(FALSE)
  }
  
  
  collapsed <- findAttribute(sidebar, "data-collapsed", "true")
  
  
  
  # Modified addDeps:
  addDeps = function (x)
  {
    if (getOption("shiny.minified", TRUE)) {
      adminLTE_js <- "app.min.js"
      shinydashboard_js <- "shinydashboard.min.js"
      adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
    }
    else {
      adminLTE_js <- "app.js"
      shinydashboard_js <- "shinydashboard.js"
      adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
    }
    dashboardDeps <- list(
      # Load customized style and scripts
      htmltools::htmlDependency(
        "Dipterix", "0.0.1",
        c(file = system.file('assets/', package = 'rave')),
        script = c(
          'dipterix.js',
          'dipterix_inputs.js'
        ), stylesheet = 'dipterix.css'
      ),
      
      # load AdminLTE
      htmltools::htmlDependency(
        "AdminLTE", "2.0.6",
        c(file = system.file("AdminLTE", package = "shinydashboard")),
        script = adminLTE_js, stylesheet = adminLTE_css),
      htmltools::htmlDependency(
        "shinydashboard",
        as.character(utils::packageVersion("shinydashboard")),
        c(file = system.file(package = "shinydashboard")),
        script = shinydashboard_js,
        stylesheet = "shinydashboard.css"))
    
    appendDependencies = function (x, value)
    {
      if (inherits(value, "html_dependency"))
        value <- list(value)
      old <- attr(x, "html_dependencies", TRUE)
      htmltools::htmlDependencies(x) <- c(old, value)
      x
    }
    
    # shinydashboard:::
    appendDependencies(x, dashboardDeps)
    
    
  }
  
  
  if(controlbar_opened){
    cls = ' control-sidebar-open'
  }else{
    cls = ''
  }
  
  
  addDeps(
    tags$body(
      class = paste0("skin-", skin, cls), # if you want control-sidebar to be opened, add " control-sidebar-open"
      style = "min-height: 611px;",
      tags$head(tags$link(rel = "icon", type = "image/x-icon", href = to_datauri(system.file('assets/images/favicon.ico', package = 'rave')))),
      shiny::bootstrapPage(
        shinyjs::useShinyjs(),
        div(
          id = '__rave__mask__',
          class = ifelse(is.null(initial_mask), 'hidden', ''),
          div(class = 'loading_info',
              initial_mask)
        ),
        content,
        title = title
      )
    )
  )
}


box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                background = NULL, width = 12, height = NULL, collapsible = TRUE,
                collapsed = FALSE, headerColor = '#f4f4f4', box_link = NULL)
{
  # Always collapsible
  collapsible <- TRUE
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- sprintf('border-top-color: %s; ', headerColor)
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status
    buttonStatus %?<-% "default"
    collapseIcon <- if (collapsed)
      "plus"
    else "minus"
    collapseTag <- div(
      class = "box-tools pull-right", 
      if (is.null(box_link)) NULL else tags$a(
        class = paste0("btn btn-box-tool"),
        href = box_link,
        target = "_blank",
        shiny::icon('question-circle')
      ),
      tags$button(class = paste0("btn btn-box-tool"),
                  `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, collapseTag, style = sprintf('background-color: %s; ', headerColor))
  }
  div(class = if (!is.null(width))
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style))
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer))
        div(class = "box-footer", footer)))
}


tabBox <- function (..., id = NULL, selected = NULL, title = NULL, width = 6, 
                    height = NULL, side = c("left", "right"), box_link = NULL) 
{
  side <- match.arg(side)
  content <- shiny::tabsetPanel(..., id = id, selected = selected)
  content$attribs$class <- "nav-tabs-custom"
  if (!is.null(height)) {
    content <- shiny::tagAppendAttributes(
      content, style = paste0("height: ", shiny::validateCssUnit(height)))
  }
  if (side == "right") {
    content$children[[1]] <- shiny::tagAppendAttributes(content$children[[1]], 
                                                 class = "pull-right")
  }
  if (!is.null(title)) {
    title = shiny::span(
      title, if (is.null(box_link)) NULL else tags$a(
        class = paste0("btn btn-box-tool"),
        href = box_link,
        target = "_blank",
        shiny::icon('question-circle')
      )
    )
    if (side == "left") 
      titleClass <- "pull-right"
    else titleClass <- "pull-left"
    content$children[[1]] <- shiny::tagAppendChild(content$children[[1]], 
                                                   shiny::tags$li(class = paste("header", titleClass), title))
  }
  div(class = paste0("col-sm-", width), content)
}
