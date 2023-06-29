#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(dplyr)
# list loupe image paths to filter from
loupe_image_paths <- tibble("LoupeImages" = list.files(path = "www/OrascopticLoupeImages/"))
# Load Orascoptic loupe data
orascoptic_data <- readxl::read_excel("data/Orascoptic_loupe_data.xlsx")
# Load dental data

dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
  filter(`Laser Mfg` != "") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)))

app_server <- function(input, output, session) {
  # The application server logic
  observeEvent(input$mfg,{
    # filter dental data to select mfg
    mfg_filtered_dental_data <- dental_data %>%
      filter(`Laser Mfg` == input$mfg)
    # update select input - laser model
    updateSelectInput(inputId = "mod",
                      choices = sort(mfg_filtered_dental_data$`Laser Model`))
  })


  loupe_insert <- eventReactive(input$loupestyle,{
    orascoptic_data %>%
      filter(`Orascoptic Frame` == input$loupestyle)
  })
  selected_data <- eventReactive(input$mod,{
    req(input$mfg)
    dental_data %>%
      filter(`Laser Mfg` == input$mfg,
             `Laser Model` == input$mod)
  })
  user_info <- eventReactive(input$run,{
    tibble(
      "Andau Loupe Style" = loupe_insert()$`Orascoptic Frame`,
      "Laser Information" = glue::glue_safe(selected_data()$`Laser Mfg`, " ", selected_data()$`Laser Model`),
      "Laser Specifications" = selected_data()$Wavelengths)
  })
  output$userInfo <- renderTable(bordered = T,
                                 align = "l",
                                 striped=T,
                                 {
                                   user_info()
                                 })
  table_info <- eventReactive(input$run,{
    if (input$loupestyle == "Triumph" | input$loupestyle == "Tempo"){
    tibble("Part Number" = "Ease In Shield",
           "Purchasing Information" = "https://www.orascoptic.com/en-us/ease-in-shields")}
    else {tibble("INVO Part Number" = if_else(selected_data()$`Eyewear Lens Compatible` == "Gi1",
                                        glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`),
                                        glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`, ".2B")),
           "Optical Density Specifications" = selected_data()$`Optical Density`,
           "Visible Light Transmission" = selected_data()$VLT)}
  })

  output$tableInfo <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  height="100%",
                                  {
                                    table_info()
                                  })
  rec1_table <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`Rec1`)
  })
  output$tableRec1 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    rec1_table()
                                  })
  rec2_table <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`Rec2`)
  })
  output$tableRec2 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    rec2_table()
                                  })
  rec3_table <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`Rec3`)
  })
  output$tableRec3 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    rec3_table()
                                  })
  image_location <- eventReactive(input$run,{
    req(input$loupestyle)
    req(input$mfg)
    req(input$mod)
    if (input$loupestyle == "Triumph" | input$loupestyle == "Tempo"){
      c("www/OrascopticLoupeImages/Triumph.Easein.Back.png",
        if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
                glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
        ),
        if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
                glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
        ),
        glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"),
        "www/OrascopticLoupeImages/Triumph.Easein.Front.png")
      }
    else {
    loupe_rec <- loupe_image_paths %>%
      filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "", input$loupestyle)) &
               stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
                          )
             )

    c(glue::glue_safe("www/OrascopticLoupeImages/", loupe_rec$LoupeImages[[2]]),
      if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
              glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
              glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
      ),
      if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
              glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
              glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
      ),
      glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"),
      glue::glue_safe("www/OrascopticLoupeImages/", loupe_rec$LoupeImages[[1]]))
    }
  })
  output$productImageF <- renderImage({
    list(src = image_location()[[1]],
         width = "400px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)
  output$productImageB <- renderImage({
    list(src = image_location()[[5]],
         width = "400px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec1 <- renderImage({
    list(src = image_location()[[2]],
         height = "300px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec2 <- renderImage({
    list(src = image_location()[[3]],
         height = "300px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec3 <- renderImage({
    list(src = image_location()[[4]],
         height = "300px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)
}
