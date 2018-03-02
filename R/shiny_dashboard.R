# Shiny dashboard-adminLTE overrides

#' @import shiny
#' @export
dashboardControl = function (..., disable = FALSE, width = NULL, collapsed = FALSE)
{
  width <- validateCssUnit(width)
  custom_css <- NULL
  if (!is.null(width)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_",
width, fixed = TRUE,
"
.control-sidebar, .right-side {
width: _WIDTH_;
}
@media (min-width: 768px) {
.content-wrapper,
.left-side,
.main-footer {
margin-right: _WIDTH_;
}
.control-sidebar,
.right-side {
width: _WIDTH_;
}
}
@media (max-width: 767px) {
.sidebar-open .content-wrapper,
.sidebar-open .right-side,
.sidebar-open .main-footer {
-webkit-transform: translate(_WIDTH_, 0);
-ms-transform: translate(_WIDTH_, 0);
-o-transform: translate(_WIDTH_, 0);
transform: translate(_WIDTH_, 0);
}
}
@media (max-width: 767px) {
.main-sidebar,
.left-side {
-webkit-transform: translate(-_WIDTH_, 0);
-ms-transform: translate(-_WIDTH_, 0);
-o-transform: translate(-_WIDTH_, 0);
transform: translate(-_WIDTH_, 0);
}
}
@media (min-width: 768px) {
.sidebar-collapse .main-sidebar,
.sidebar-collapse .left-side {
-webkit-transform: translate(-_WIDTH_, 0);
-ms-transform: translate(-_WIDTH_, 0);
-o-transform: translate(-_WIDTH_, 0);
transform: translate(-_WIDTH_, 0);
}
}
"))))
  }
  dataValue <- shiny::restoreInput(id = "sidebarCollapsed",
                                   default = collapsed)
  if (disable)
    dataValue <- TRUE
  dataValueString <- if (dataValue)
    "true"
  else "false"
  tagList(
    tags$aside(class = "control-sidebar control-sidebar-dark",
               `data-collapsed` = dataValueString,
               custom_css, tags$section(class = "sidebar", `data-disable` = if (disable)
                 1
                 else NULL, list(...))),
    div(class="control-sidebar-bg")
  )
}

#' @import shiny
#' @export
dashboardHeader = function (..., title = NULL, titleWidth = NULL, disable = FALSE,
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
          ...
        )
      ),

      div(class = "navbar-custom-menu",
          tags$ul(
            class = "nav navbar-nav",
            items %>%
              tagList(),
            tags$li(
              a(href = "#", class = "nav-item nav-link force-recalculate",
                `data-toggle` = "control-sidebar",
                role = "button",
                span(class = "sr-only", "Toggle control"),
                shiny::icon('cogs'), span('Controls')
              )
            )
          )
        )
      )
  )
}


#' @import shiny
#' @export
dashboardPage = function (
  header, sidebar, control, body, title = NULL,
  skin = c("blue", "black", "purple", "green", "red", "yellow"), controlbar_opened = FALSE)
{
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
  collapsed <- shinydashboard:::findAttribute(sidebar, "data-collapsed", "true")



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
        script = 'dipterix.js', stylesheet = 'dipterix.css'),

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
    shinydashboard:::appendDependencies(x, dashboardDeps)


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
      shiny::bootstrapPage(shinyjs::useShinyjs(), content, title = title)
    )
  )
}


