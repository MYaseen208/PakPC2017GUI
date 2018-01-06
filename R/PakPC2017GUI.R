#' @name    PakPC2017GUI
#' @aliases PakPC2017GUI
#' @title  GUI for Pakistan Population Census 2017
#' @description GUI for Pakistan Population Census 2017.
#' @return A GUI for visualizing data from \code{dataset}.
#' @author  \enumerate{
#'  \item Muhammad Yaseen (\email{myaseen208@@gmail.com})
#'  \item Muhammad Arfan Dilber (\email{pbsfsd041@gmail.com})
#'  }
#'
#' @references \enumerate{
#' \item Pakistan Population Census 2017 (\url{http://www.pbscensus.gov.pk/}).
#'  }
#'
#' @import shiny
#' @import PakPC2017
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import tidyr
#' @import reshape2
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#' @importFrom utils head
#'
#' @examples
#'
#' if(interactive()){
#' PakPC2017GUI()
#' }
#'
#' @export

if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(
        "Census"
      , "City"
      , "District"
      , "Division"
      , "Pop1998"
      , "Pop2017"
      , "Population"
      , "Province"
      , "Tehsil"
    )
  )
}

PakPC2017GUI <- function() {

    PakPC2017Tehsil <- PakPC2017::PakPC2017Tehsil
    PakPC2017City10 <- PakPC2017::PakPC2017City10

    ui <-
      fluidPage(
        headerPanel(
          title = "Pakistan Population Census 2017"
        )
        , sidebarPanel(
            width = 3
          , conditionalPanel(
               condition = "input.tabs == 'data'"
            ,  h4("Population by")
            , selectInput(
                inputId = "AdminUnit"
              , label   = "Please Select Admin Unit"
              , choices = c(
                               "Province"
                             , "Division"
                             , "District"
                             , "Tehsil"
                             , "City"
                             )
              , selected ="Province"
              )
            )
          , conditionalPanel(
               condition = "input.tabs == 'chart'"
            ,  h4("Chart by Administrative Unit")
            ,  selectInput(
                    inputId = "PlotUnit"
                  , label   = "Select Admin Unit"
                  , choices = c(
                                  "Provinces"
                                , "Divisions"
                                , "Districts"
                                , "Tehsils"
                                , "Cities"
                              )
                  , "Provinces"
                )
            , conditionalPanel(
                     condition = "input.PlotUnit == 'Divisions'||
                                  input.PlotUnit == 'Districts'||
                                  input.PlotUnit == 'Tehsils'"
                  ,  selectInput(
                            inputId  = "Province"
                          , label    = "Select one or more Provinces first"
                          , choices  = levels(as.factor(PakPC2017Tehsil$Province))
                          , multiple = TRUE
                          )
                  )
            , conditionalPanel(
                     condition = "input.PlotUnit == 'Divisions'||
                                  input.PlotUnit == 'Districts'||
                                  input.PlotUnit == 'Tehsils'"
                  , selectInput(
                          inputId  = "Div"
                        , label    = "Select one or more Divisions"
                        , choices  = ""
                        , multiple = TRUE
                        )
                  )
            , conditionalPanel(
                     condition = "input.PlotUnit == 'Districts'||
                                  input.PlotUnit == 'Tehsils'"
                  , selectInput(
                          inputId  = "District"
                        , label    = "Select one or more Districts"
                        , choices  = ""
                        , multiple = TRUE
                        )
                  )
            , conditionalPanel(
                     condition = "input.PlotUnit == 'Tehsils'"
                  ,  selectInput(
                              inputId  = "Tehsil"
                            , label    = "Select one or more Tehsils"
                            , choices  = ""
                            , multiple = TRUE
                            )
                  )
            )
          )
        , mainPanel(
                  width = 9
                , tabsetPanel(
                    type = "tabs"
                  , tabPanel(
                      title = "data"
                    , dataTableOutput("myTable")
                    )
                  , tabPanel(
                           title = "chart"
                         , mainPanel(
                              width = 12
                            , downloadButton(
                                    outputId = "download_plot_PDF"
                                  , label    = "Download pdf of figure"
                                  )
                            , plotOutput("myMap")
                            )
                         )
                  , id = "tabs"
                  )
                )
        )

  server <-
    function(input, output, session){
      observe({
        divi <- dplyr::filter(PakPC2017Tehsil, Province %in% input$Province)
        levelDivision <- levels(as.factor(divi$Division))
        updateSelectInput(
            session = session
          , inputId = "Div"
          , label   = "Division"
          , choices = levelDivision
          )
        reactive(input$Div)
      })

      observe({
        distt <- dplyr::filter(PakPC2017Tehsil, Division %in% input$Div)
        levelDistrict <- levels(as.factor(distt$District))
        updateSelectInput(
            session = session
          , inputId = "District"
          , label   = "District"
          , choices = levelDistrict
          )
        reactive(input$District)
      })

      observe({
        tehs <- dplyr::filter(PakPC2017Tehsil, District %in% input$District)
        levelTehsil <- levels(as.factor(tehs$Tehsil))
        updateSelectInput(
            session = session
          , inputId = "Tehsil"
          , label   = "Tehsil"
          , choices = levelTehsil
          )
      })

      adminUnit <-
        reactive(
          {
            switch(
                    input$AdminUnit
                  , "Province" = PakPC2017Tehsil %>%
                                  dplyr::group_by(Province) %>%
                                  dplyr::summarize(
                                      Pop2017 = sum(Pop2017, na.rm = TRUE)
                                    , Pop1998 = sum(Pop1998, na.rm = TRUE)
                                    )
                  , "Division" = PakPC2017Tehsil %>%
                                  dplyr::group_by(Province, Division) %>%
                                  dplyr::summarize(
                                              Pop2017 = sum(Pop2017, na.rm = TRUE)
                                            , Pop1998 = sum(Pop1998, na.rm = TRUE)
                                            )
                  , "District" = PakPC2017Tehsil %>%
                                  dplyr::group_by(Province, Division, District) %>%
                                  dplyr::summarize(
                                                    Pop2017 = sum(Pop2017, na.rm = TRUE)
                                                  , Pop1998 = sum(Pop1998, na.rm = TRUE)
                                                  )
                  , "Tehsil"= PakPC2017Tehsil
                  , "City"  = PakPC2017City10
                  )
            }
          )

      output$myTable <- renderDataTable({adminUnit()})

      output$myMap <-
        renderPlot({
          plotType <- input$PlotUnit
          if(plotType == "Provinces") {

        plotUnit <-
          PakPC2017Tehsil %>%
          dplyr::group_by(Province) %>%
          dplyr::summarize(
                    Pop2017 = sum(Pop2017, na.rm = TRUE)
                  , Pop1998 = sum(Pop1998, na.rm = TRUE)
                  ) %>%
          tidyr::gather(
                    key   = "Census"
                  , value = "Population"
                  , -Province
                  )

           ggplot(
              data = plotUnit
              , mapping = aes(x = reorder(Province, -Population), y = Population, fill = Census)) +
             geom_bar(stat = "identity", position = position_dodge(width = .9))+
             scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
             geom_text(aes(label=scales::comma(Population)),
                       position=position_dodge(width = 0.9), vjust = -0.5) +
             labs(x = "Province") +
             theme(legend.position = "top")
      }
      else if(plotType == "Divisions"){
        plotUnit <-
          PakPC2017Tehsil %>%
          dplyr::group_by(Province, Division) %>%
          dplyr::summarize(
              Pop2017 = sum(Pop2017, na.rm = TRUE)
            , Pop1998 = sum(Pop1998, na.rm = TRUE)
          ) %>%
          dplyr::filter(Division %in% input$Div) %>%
          tidyr::gather(
              key   = "Census"
            , value = "Population"
            , -Province, - Division
          )

        ggplot(
          data = plotUnit
          , mapping = aes(x = Division, y = Population, fill = Census)) +
          geom_bar(stat = "identity", position = position_dodge(width = .9))+
          scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
          geom_text(aes(label=scales::comma(Population)),
                    position=position_dodge(width = 0.9), vjust = -0.5) +
          facet_wrap(~Province, scales = "free_x") +
          theme(legend.position = "top")
      }

      else if(plotType == "Districts"){

        plotUnit <-
          PakPC2017Tehsil %>%
          dplyr::group_by(Province, Division, District) %>%
          dplyr::summarize(
              Pop2017 = sum(Pop2017, na.rm = TRUE)
            , Pop1998 = sum(Pop1998, na.rm = TRUE)
          ) %>%
          dplyr::filter(District %in% input$District) %>%
          tidyr::gather(
              key   = "Census"
            , value = "Population"
            , -Province, - Division, -District
          )


        ggplot(
          data = plotUnit
          , mapping = aes(x = District, y = Population, fill = Census)) +
          geom_bar(stat = "identity", position = position_dodge(width = .9))+
          scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
          geom_text(aes(label=scales::comma(Population)),
                    position=position_dodge(width = 0.9), vjust = -0.5) +
          facet_grid(Division ~ Province, scales = "free_x") +
          theme(legend.position = "top")
      }

      else if(plotType == "Tehsils"){

        plotUnit <-
          PakPC2017Tehsil %>%
          tidyr::gather(
              key   = "Census"
            , value = "Population"
            , -Province, - Division, -District, -Tehsil
          )  %>%
          dplyr::filter(Tehsil %in% input$Tehsils)

        ggplot(
          data = plotUnit
          , mapping = aes(x = Tehsil, y = Population, fill = Census)) +
          geom_bar(stat = "identity", position = position_dodge(width = .9))+
          scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
          geom_text(aes(label=scales::comma(Population)),
                    position=position_dodge(width = 0.9), vjust = -0.5) +
          facet_grid(Division ~ Province, scales = "free_x") +
          theme(legend.position = "top")

      }

      else{
        plotUnit <-
          PakPC2017City10 %>%
          arrange(desc(Pop2017)) %>%
          tidyr::gather(
              key   = "Census"
            , value = "Population"
            , -City
          )


        ggplot(
          data = plotUnit
          , mapping = aes(x = reorder(City, -Population), y = Population, fill = Census)) +
          geom_bar(stat = "identity", position = position_dodge(width = .9))+
          scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
          geom_text(aes(label=scales::comma(Population)),
                    position=position_dodge(width = 0.9), vjust = -0.5) +
          labs(x = "Cities") +
          theme(legend.position = "top")

        }
          }
      )
    }

  shinyApp(ui, server)
}
