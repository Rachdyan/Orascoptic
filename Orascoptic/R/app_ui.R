#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(bslib)
library(dplyr)

# Load Orascoptic loupe data
orascoptic_data <- readxl::read_excel("data/Orascoptic_loupe_data.xlsx")

# Load dental data

dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
  filter(`Laser Mfg` != "") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)))


# theming options
orascoptic_theme <- bs_theme(version = 5,
                        base_font  = font_google("Work Sans"),
                        bg = "white",
                        fg = "#1f0900",
                        primary = "#004793",
                        secondary = "#FFB300")
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fluid(
      theme = orascoptic_theme,
               card(card_header(inverse = T,
                                fluidRow(
                 column(6,
                        align = 'left',
                        a(href = "https://www.orascoptic.com/en-us?utm_medium=cpc&utm_source=google&utm_term=orascoptic%20dental%20loupes&utm_campaign=MM:+Orascoptic+NA+-+Loupes+-+Brand+-+Search+-+GGL&hsa_acc=8101871646&hsa_cam=19358180543&hsa_grp=145066458215&hsa_ad=642478728693&hsa_src=g&hsa_tgt=kwd-323440679163&hsa_kw=orascoptic%20dental%20loupes&hsa_mt=e&hsa_net=adwords&hsa_ver=3&gclid=EAIaIQobChMImbX-3dzZ_wIVtHxMCh1evQTpEAAYASAAEgJElfD_BwE",
                        img(src = "icons/orascoptic_logo.png", width = "270px"))
                        ),
                 column(6, align= 'right',

                        h5("CustomerCare@Orascoptic.com"),
                        h5("800.369.3698")

                        )))
                 ,fluidRow(column(12,align='center',
                                  h2(strong("Search eye protection by selecting a loupe style and laser device"))))
               ),
                 fluidRow(
                   column(
                     4,
                     align = 'center',
                     selectInput(
                       inputId = "loupestyle",
                       label = h4(strong("Loupe Style")),
                       choices = sort(orascoptic_data$`Orascoptic Frame`),
                       selected = NULL
                     )
                   ),
                 column(
                   4,
                   align = 'center',
                   selectInput(
                     inputId = "mfg",
                     label = h4(strong("Manufacturer")),
                     choices = sort(dental_data$`Laser Mfg`),
                     selected = NULL
                   )
                 ),
                 column(
                   4,
                   align = 'center',
                   selectInput(
                     inputId = "mod",
                     label = h4(strong("Model")),
                     choices = dental_data$`Laser Model`,
                     selected = NULL
                   )
                 )),
                 fluidRow(

                 column(
                   12,
                   align = "center",
                   br(),
                   actionButton("run",
                                icon = icon("magnifying-glass"),
                                style='padding-left:50px;padding-right:50px;padding-top:1px;padding-bottom:1px; font-size:80%',
                                h5(strong("Search")),
                                class = "btn-secondary"))
                 ),
      br(),
                  fluidRow(
                    column(12,
                   p("Your information not available in the dropdowns? Contact Innovative Optics at (763)425-7789"))
                   ),
               conditionalPanel(
                 condition = "input.run",
                 card(fluidRow(column(12, align = "center",
                                           h3(style = {
                                             "color: #004793;"
                                           },
                                           em("Device Information")),
                                           tableOutput("userInfo"))),
                           fluidRow(column(12,
                                           align = "center",
                                           h3(style = {
                                             "color: #004793;"
                                           },
                                           em("Compatible Innovative Optics Product")),
                                           tableOutput("tableInfo"))),
                           fluidRow(column(6, align = 'center',
                                           imageOutput("productImageF")),
                                    column(6, align = 'center',
                                           imageOutput("productImageB")))),
                 card(class = "box-shadow",
                   fluidRow(column(12,
                                           align = 'center',
                                           h3(style = {
                                             "color: #004793;"
                                           },
                                             em("Frequently Purchased Together")))),
                 fluidRow(
                             column(4, align = 'center',
                                    imageOutput("rec1"),
                                    tableOutput("tableRec1")),
                             column(4, align = 'center',
                                    imageOutput("rec2"),
                                    tableOutput("tableRec2")),
                             column(4, align = 'center',
                                    imageOutput("rec3"),
                                    tableOutput("tableRec3")))
                 )),
               card(card_footer(h5(
                 style = {
                   "color: #FFB300;
                         text-shadow: 1px 1px 1px black;"
                 },
                 "Powered by Innovative Optics")))
    ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Orascoptic"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
