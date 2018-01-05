#' @name    PakPC2017GUI
#' @aliases PakPC2017GUI
#' @title  GUI for Pakistan Population Census 2017
#' @description GUI for Pakistan Population Census 2017.
#' @param \code{dataset} A dataset (optional).
#' @return A GUI for visualizing data from \code{dataset}.
#' @import shiny
#' @import PakPC2017
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import magrittr
#' @import reshape2
#' @export
PakPC2017GUI <- function( dataset = NA ) {

    PakPC2017Tehsil <- PakPC2017::PakPC2017Tehsil
    PakPC2017City10 <- PakPC2017::PakPC2017City10

    ui <- fluidPage(
    headerPanel("Pakistan Population Census 2017"),
    sidebarPanel(width = 3,
                 conditionalPanel(
                   condition = "input.tabs == 'data'",
                   h4("Population by"),

                   selectInput("AdminUnit", "Please Select Admin Unit",
                               choices = c("Province", "Division", "District", "Tehsil", "City"),
                               "Tehsil")
                 ),
                 conditionalPanel(
                   condition = "input.tabs == 'chart'",
                   h4("Chart by Administrative Uinit"),

                   selectInput("PlotUnit","Select Admin Unit",
                               choices = c("Provinces","Divisions","Districts","Tehsils","Cities"),
                               "Provinces"),
                   conditionalPanel(
                     condition = "input.PlotUnit == 'Divisions'||
                     input.PlotUnit == 'Districts'||
                     input.PlotUnit == 'Tehsils'",
                     selectInput(
                       "Province","Select one or more Provinces first",choices = levels(as.factor(PakPC2017Tehsil$Province)),multiple = TRUE
                     )),
                   conditionalPanel(
                     condition = "input.PlotUnit == 'Divisions'||
                     input.PlotUnit == 'Districts'||
                     input.PlotUnit == 'Tehsils'",
                     selectInput(
                       "Div","Select one or more Divisions",choices = "",multiple = TRUE
                     )),
                   conditionalPanel(
                     condition = "input.PlotUnit == 'Districts'||
                     input.PlotUnit == 'Tehsils'",
                     selectInput(
                       "District","Select one or more Districts",choices = "",multiple = TRUE
                     )
                   ),
                   conditionalPanel(
                     condition = "input.PlotUnit == 'Tehsils'",
                     selectInput(
                       "Tehsil","Select one or more Tehsils",choices = "",multiple = TRUE
                     )
                   )
    )
    ),
    mainPanel(width = 8,
              tabsetPanel(
                type = "tabs",
                tabPanel("data", dataTableOutput("myTable")),
                tabPanel("chart",
                         mainPanel(width = 12,
                                   downloadButton("download_plot_PDF",
                                                  "Download pdf of figure"),
                                   plotOutput("myMap"))
                ),
                id = "tabs"
              )

    )
    )

  server <-
    function(input,output,session){
      observe({
        divi <- PakPC2017Tehsil[PakPC2017Tehsil$Province %in% input$Province,]
        levelDivision <- levels(as.factor(divi$Division))
        updateSelectInput(session, "Div",choices = levelDivision)
        reactive(input$Div)
      })

      observe({
        distt <- PakPC2017Tehsil[PakPC2017Tehsil$Division %in% input$Div,]
        levelDistrict <- levels(as.factor(distt$District))
        updateSelectInput(session, "District",choices = levelDistrict)
        reactive(input$District)
      })
      observe({
        tehs <- PakPC2017Tehsil[PakPC2017Tehsil$District %in% input$District,]
        levelTehsil <- levels(as.factor(tehs$Tehsil))
        updateSelectInput(session, "Tehsil",choices = levelTehsil)
      })
      adminUnit <- reactive({switch(input$AdminUnit,
                                  "Province"=PakPC2017Tehsil%>%
                                    group_by(Province)%>%
                                    summarize(Pop2017 = sum(Pop2017, na.rm = TRUE),
                                              Pop1998 = sum(Pop1998, na.rm = TRUE)),
                                  "Division" = PakPC2017Tehsil%>%
                                    group_by(Province, Division)%>%
                                    summarize(Pop2017 = sum(Pop2017, na.rm = TRUE),
                                              Pop1998 = sum(Pop1998, na.rm = TRUE)),
                                  "District"=PakPC2017Tehsil%>%
                                    group_by(Province, Division, District)%>%
                                    summarize(Pop2017 = sum(Pop2017, na.rm = TRUE),
                                              Pop1998 = sum(Pop1998, na.rm = TRUE)),
                                  "Tehsil"= PakPC2017Tehsil,
                                  "City"  = PakPC2017City10
      )
      })

      output$myTable <- renderDataTable({adminUnit()})

      output$myMap <- renderPlot({plotType<-input$PlotUnit
      if(plotType=="Provinces"){

        plotUnit1 <-
          PakPC2017Tehsil %>%
          group_by(Province) %>%
          summarize(
                    Pop2017 = sum(Pop2017, na.rm = TRUE)
                  , Pop1998 = sum(Pop1998, na.rm = TRUE)
                  )
        plotUnit <-
          reshape2::melt(
            data          = plotUnit1
          , variable.name = "Census"
          , value.name    = "Population"
          )

           ggplot(plotUnit,
               aes(x = Province,
                   y = Population,
                   group=Census,
                   fill=Census)) + geom_point()+
          geom_bar(stat="identity", position=position_dodge(width = .9))+
          geom_text(aes(label=scales::comma(Population)),
                    position=position_dodge(width = .9), vjust=-.5)+
          scale_y_continuous(labels= scales::comma)
      }
      else if(plotType=="Divisions"){
        plotUnit1<-PakPC2017Tehsil%>%
          group_by(Province,Division)%>%
          summarize(Pop2017 = sum(Pop2017, na.rm = TRUE),
                    Pop1998 = sum(Pop1998,na.rm = TRUE))
        plotUnit4<-plotUnit1[plotUnit1$Division %in% input$Div,]
        plotUnit<-melt(plotUnit4, variable.name="Census",value.name = "Population")


        ggplot(plotUnit,
               aes(x = Division,
                   y = Population,
                   fill=Census,
                   colour=Census))+geom_point()+facet_wrap(~Province, scales = "free_x")+
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label=scales::comma(Population)), position=position_dodge(width = .9), vjust=-.5)+
          scale_y_continuous(labels= scales::comma)
      }


      else if(plotType=="Districts"){
        plotUnit1<-PakPC2017Tehsil%>%
          group_by(Province,Division,District)%>%
          summarize(Pop2017 = sum(Pop2017, na.rm = TRUE),
                    Pop1998 = sum(Pop1998,na.rm = TRUE))
        plotUnit4<-plotUnit1[plotUnit1$District %in% input$District,]
        plotUnit<-melt(plotUnit4, variable.name="Census",value.name = "Population")


        ggplot(plotUnit,
               aes(x = District,
                   y = Population,fill=Census,colour=Census))+geom_point()+
          facet_wrap(Division~Province, scales = "free_x")+
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label=scales::comma(Population)), position=position_dodge(width = .9), vjust=-.5)+
          scale_y_continuous(labels= scales::comma)
      }
      else if(plotType=="Tehsils"){
        plotUnit1<-PakPC2017Tehsil%>%
          group_by(Province,Tehsil)%>%
          summarize(Pop2017 = sum(Pop2017, na.rm = TRUE),
                    Pop1998 = sum(Pop1998,na.rm = TRUE))
        plotUnit4<-plotUnit1[plotUnit1$Tehsil %in% input$Tehsil,]
        plotUnit<-melt(plotUnit4, variable.name="Census",value.name = "Population")


        ggplot(plotUnit,
               aes(x = Tehsil,
                   y = Population,
                   fill=Census,
                   colour=Census))+ geom_point() +
          facet_wrap(~Province, scales = "free_x")+
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label=scales::comma(Population)), position=position_dodge(width = .9), vjust=-.5)+
          scale_y_continuous(labels= scales::comma)
      }



      else{
        plotUnit1 <- PakPC2017City10
        plotUnit1 <- plotUnit1 %>% arrange(desc(Pop2017))
        plotUnit1 <- head(plotUnit1, 5)
        plotUnit  <- melt(plotUnit1, variable.name="Census",value.name = "Population")


        ggplot(plotUnit,
               aes(x = City,
                   y = Population,
                   fill=Census,
                   colour=Census))+geom_point()+
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label=scales::comma(Population)), position=position_dodge(width = .9), vjust=-.5)+
          scale_y_continuous(labels= scales::comma)
      }                              })

    }

  shinyApp(ui,server)
}
