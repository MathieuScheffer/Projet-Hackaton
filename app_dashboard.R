library(shiny)
library(bs4Dash)
library(DT)
library(highcharter)
library(htmltools)
library(fresh)
library(RCurl)
library(geodata)
library(raster)
library(sf)
library(dplyr)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(shinyjs)

theme <- create_theme(
  bs4dash_color(
    lime = "#405265",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)

couleurs <- list("Resident" = "#1f77b4", "Etranger" = "#ff7f0e")

# Upload des resultats de github
df_depense_publique_cate <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/df_depense_publique_cate.csv")) %>%
  dplyr::rename(`Autres dépenses` = "Autres.dépenses",
                `Autres dépenses sociales` = "Autres.dépenses.sociales")

df_depense_publique_cate_milleur <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/df_depense_publique_cate_milleur.csv")) %>%
  dplyr::rename(`Autres dépenses` = "Autres.dépenses",
                `Autres dépenses sociales` = "Autres.dépenses.sociales")

df_ratio1_tf <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/df_ratio1_tf.csv"))

df_ratio2_tf <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/df_ratio2_tf.csv"))

df_pop2100 <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/df_long.csv"))

df_pop2100_65plus <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/df_pop2100_65plus.csv"))

result_lm <- read.csv(text = getURL("https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/result_lm.csv"))

#########################################################################################################################################
# To get polygon (spatial data) at CANTON level for Luxembourg from GADM source
##########################################################################################################################################
lu2 <- geodata::gadm(country  = "LUX", level = 2, path = tempdir())

# dplyr::rename all CANTON name in order to match with STATEC labels
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Clervaux", "Canton Clervaux", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Diekirch", "Canton Diekirch", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Redange", "Canton Redange", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Vianden", "Canton Vianden", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Wiltz", "Canton Wiltz", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Echternach", "Canton Echternach", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Grevenmacher", "Canton Grevenmacher", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Remich", "Canton Remich", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Capellen", "Canton Capellen", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Esch-sur-Alzette", "Canton Esch", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Luxembourg", "Canton Luxembourg", lu2$NAME_2)
lu2$NAME_2 <- ifelse(lu2$NAME_2 == "Mersch", "Canton Mersch", lu2$NAME_2)

lu2 <- as(lu2, "Spatial")
##########################################################################################################################################
##########################################################################################################################################

# Creation du 1er leaflet par canton - 65 ans et + en % de la pop.
lu_sf <- st_as_sf(lu2)

lu_sf_ratio1 <- lu_sf %>%
  left_join(df_ratio1_tf, by = c("NAME_2" = "geo_name_fr"))

pal <- colorNumeric(
  palette = RColorBrewer::brewer.pal(9, "Reds")[3:9],
  domain = lu_sf$pct_65_plus,
  na.color = "transparent"
)

lu_sf_ratio1$popup <- paste0(
  "<strong>", lu_sf_ratio1$NAME_2, "</strong><br/>",
  "65 ans et plus : ", ifelse(is.na(lu_sf_ratio1$pct_65_plus), "NA", paste0(round(lu_sf_ratio1$pct_65_plus, 1), "%"))
)

map_pct_65_plus_canton <- leaflet(lu_sf_ratio1) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(pct_65_plus),
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
    label = lapply(lu_sf_ratio1$popup, HTML)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~pct_65_plus,
    title = "% 65 ans et plus",
    labFormat = labelFormat(suffix = "%"),
    opacity = 0.9
  )

# Creation du 2eme leaflet par canton - % immigrés par canton
lu_sf_ratio2 <- lu_sf %>%
  left_join(df_ratio2_tf, by = c("NAME_2" = "geo_name_fr"))

lu_sf_ratio2$immigre <- as.numeric(gsub(",", ".", lu_sf_ratio2$immigre))

pal_ratio2 <- colorNumeric(
  palette = RColorBrewer::brewer.pal(9, "Greens")[3:9],
  domain = lu_sf_ratio2$immigre,
  na.color = "transparent"
)

lu_sf_ratio2$immigre_label <- paste0(
  "<strong>", lu_sf_ratio2$NAME_2, "</strong><br/>",
  "% d'immigrés : ", ifelse(is.na(lu_sf_ratio2$immigre), "NA", paste0(round(lu_sf_ratio2$immigre, 1), "%"))
)

map_immigre_canton <- leaflet(lu_sf_ratio2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal_ratio2(immigre),
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
    label = lapply(lu_sf_ratio2$immigre_label, HTML)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_ratio2,
    values = ~immigre,
    title = "% d'immigrés",
    labFormat = labelFormat(suffix = "%"),
    opacity = 0.9
  )

ui <- bs4Dash::dashboardPage(
  title = "Hackathon STATEC",
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  # Header
  header = bs4Dash::dashboardHeader(
    status = "bleu_header",
    title = bs4Dash::dashboardBrand(
      title = "Hackathon STATEC",
      color = "olive"
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE
  ),
  
  # Sidebar
  sidebar = bs4Dash::dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Analysis", tabName = "Tab_analysis", icon = icon("bar-chart"))
    )
  ),
  
  # Control bar
  controlbar = bs4Dash::dashboardControlbar(),
  
  # Footer
  footer = bs4Dash::dashboardFooter(
    left = "@Artemis Information management SA",
    right = "2025 - version 1.0"
  ),
  
  # Body
  body = bs4Dash::dashboardBody(
    tabItems(
      tabItem(
        tabName = "Tab_analysis",
        
        # 2 leaflets
        fluidRow(
          column(
            width = 6,
            tabBox(
              width = 12,
              type = "tabs",
              status = "olive",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              tabPanel(
                tabName = "map65",
                title = "% 65 ans et plus",
                active = TRUE,
                leafletOutput("map_65pct"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_map65",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              ),
              
              tabPanel(
                tabName = "mapImm",
                title = "Immigration",
                leafletOutput("map_immigre"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_map_immi",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              )
            )
          ),
          
          # Graphique dépenses publiques
          column(
            width = 6,
            tabBox(
              width = 12,
              type = "tabs",
              status = "olive",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              tabPanel(
                tabName = "graph_dep_pub",
                title = "Dépenses publiques (en %)",
                active = TRUE,
                highchartOutput("graph_depense_publique_cate"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_dep_publique",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              ),
              
              tabPanel(
                tabName = "graph_dep_pub_milleur",
                title = "Dépenses publiques (en millions euro)",
                active = TRUE,
                highchartOutput("graph_depense_publique_cate_millions_eur"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_dep_publique_millions_eur",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            tabBox(
              width = 12,
              type = "tabs",
              status = "olive",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              # Onglet 1 : population totale
              tabPanel(
                tabName = "pop2010",
                title = "Pop. jusque 2100",
                active = TRUE,
                highchartOutput("df_pop_jsq_2100"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_pop",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              ),
              
              # Onglet 2 : population 65+ ans
              tabPanel(
                tabName = "pop2010_65ans_plus",
                title = "Pop. 65 ans et plus jusque 2100",
                highchartOutput("df_pop_jsq_2100_65plus"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_pop65",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              )
            )
          ),
          
          column(
            width = 6,
            tabBox(
              width = 12,
              type = "tabs",
              status = "olive",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              tabPanel(
                tabName = "graph_res_etr",
                title = "Part des résidents et étrangers dans les pensions",
                active = TRUE,
                highchartOutput("graph_result_lm"),
                br(),
                div(
                  style = "text-align: right; padding: 10px;",
                  actionButton(
                    inputId = "info_button_res_etr",
                    label = "Informations",
                    icon = icon("info-circle"),
                    class = "btn btn-info"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {

  output$map_65pct<- renderLeaflet({
    map_pct_65_plus_canton
  })
  
  # Bouton information map par canton pct 65 ans et plus
  observeEvent(input$info_button_map65, {
    showModal(modalDialog(
      title = "% 65 ans et plus par canton",
      size = "l",
      tags$div(
        style = "max-width: 700px; white-space: normal; word-wrap: break-word;",
        p("Lien: https://lustat.statec.lu/vis?lc=en&pg=0&snb=1&df[ds]=ds-release&df[id]=DSD_CENSUS_GROUP1_3%40DF_B1607&df[ag]=LU1&df[vs]=1.0&dq=..A10.._T.................&pd=2021%2C2021&to[TIME_PERIOD]=false"),
        p("Date de publication: 13 décembre 2024"),
        p("Source: STATEC")
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  output$map_immigre <- renderLeaflet({
      map_immigre_canton
  })
  
  # Bouton information map par canton - immigration
  observeEvent(input$info_button_map_immi, {
    showModal(modalDialog(
      title = "% de personnes d'origine étrangère",
      size = "l",
      tags$div(
        style = "max-width: 700px; white-space: normal; word-wrap: break-word;",
        p("Lien: https://lustat.statec.lu/vis?lc=en&pg=0&snb=1&df%5bds%5d=ds-release&df%5bid%5d=DSD_CENSUS_GROUP7_10%40DF_B1626&df%5bag%5d=LU1&df%5bvs%5d=1.0&dq=..A10._T..................&pd=2021%2C2021&to%5bTIME_PERIOD%5d=false"),
        p("Date de publication: 18 décembre 2024"),
        p("Source: STATEC")
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  output$graph_depense_publique_cate <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%   
      hc_xAxis(categories = df_depense_publique_cate$TIME_PERIOD) %>%
      hc_yAxis(title = list(text = "% du total"), min = 0, max = 100) %>%
      hc_plotOptions(
        column = list(stacking = "normal") 
      ) %>%
      hc_add_series(name = "Santé", data = df_depense_publique_cate$Santé, color = "#4daf4a") %>%
      hc_add_series(name = "Vieillesse", data = df_depense_publique_cate$Vieillesse, color = "#e41a1c") %>%
      hc_add_series(name = "Autres dépenses sociales", data = df_depense_publique_cate$`Autres dépenses sociales`, color = "#377eb8") %>%
      hc_add_series(name = "Autres dépenses", data = df_depense_publique_cate$`Autres dépenses`, color = "#27F5EE")
  })
  
  # Bouton depenses publiques pct
  observeEvent(input$info_button_dep_publique, {
    showModal(modalDialog(
      title = "Dépenses publiques luxembourgeoise par catégorie (% du total)",
      size = "l",
      tags$div(
        style = "max-width: 700px; white-space: normal; word-wrap: break-word;",
        p("Lien: https://ec.europa.eu/eurostat/databrowser/view/gov_10a_exp/default/table?lang=fr"),
        p("Date de publication: 21 octobre 2025"),
        p("Source: Eurostat"),
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  output$graph_depense_publique_cate_millions_eur <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%   
      hc_xAxis(categories = df_depense_publique_cate_milleur$TIME_PERIOD) %>%
      hc_yAxis(title = list(text = "Dépenses en millions d'euro")) %>%
      hc_plotOptions(
        column = list(stacking = "normal") 
      ) %>%
      hc_add_series(name = "Santé", data = df_depense_publique_cate_milleur$Santé, color = "#4daf4a") %>%
      hc_add_series(name = "Vieillesse", data = df_depense_publique_cate_milleur$Vieillesse, color = "#e41a1c") %>%
      hc_add_series(name = "Autres dépenses sociales", data = df_depense_publique_cate_milleur$`Autres dépenses sociales`, color = "#377eb8") %>%
      hc_add_series(name = "Autres dépenses", data = df_depense_publique_cate_milleur$`Autres dépenses`, color = "#27F5EE")
  })
  
  # Bouton depenses publiques millions euro
  observeEvent(input$info_button_dep_publique_millions_eur, {
    showModal(modalDialog(
      title = "Dépenses publiques luxembourgeoise par catégorie (millions euro)",
      size = "l",
      tags$div(
        style = "max-width: 700px; white-space: normal; word-wrap: break-word;",
        p("Lien: https://ec.europa.eu/eurostat/databrowser/view/gov_10a_exp/default/table?lang=fr"),
        p("Date de publication: 21 octobre 2025"),
        p("Source: Eurostat"),
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  
  output$df_pop_jsq_2100 <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_title(text="Evolution de la population") %>%
      hc_xAxis(
        plotLines = list(list(
          color="#000000",
          width=2,
          value=2024,
          dashstyle="Dash",
          label=list(text="Fin des données historiques", verticalAlign="top",textAlign="left")
        ))) %>%
      hc_yAxis(title=list(text="Nombre de personne")) %>%
      hc_tooltip(shared=TRUE, crosshairs=TRUE, valueDecimals=0) %>%
      hc_add_series_list(
        df_pop2100 %>%
          group_split(projection) %>%
          map(~ list(
            name = unique(.x$projection),
            data = list_parse2(.x %>% dplyr::select(x=year, y=population)),
            dashStyle=ifelse(unique(.x$projection) == "Historique", "Solid", "Dash")
          ))
      ) %>%
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
        )
      )
  })
  
  output$df_pop_jsq_2100_65plus <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_title(text="Evolution de la population agée de 65 ans et plus") %>%
      hc_xAxis(
        plotLines = list(list(
          color="#000000",
          width=2,
          value=2024,
          dashstyle="Dash",
          label=list(text="Fin des données historiques", VerticalAlign="top", textAlign="left")
        ))) %>%
      hc_yAxis(title=list(text="Nombre de personne")) %>%
      hc_tooltip(shared=TRUE, crosshairs=TRUE, valueDecimals=0) %>%
      hc_add_series_list(
        df_pop2100_65plus %>%
          group_split(projection) %>%
          map(~ list(
            name = unique(.x$projection),
            data = list_parse2(.x %>% dplyr::select(x=year, y=population)),
            dashStyle=ifelse(unique(.x$projection) == "Historique", "Solid", "Dash")
          ))
      )
  })
  
  # Bouton Evolution de la population 
  observeEvent(input$info_button_pop, {
    showModal(modalDialog(
      title = "Evolution de la population avec projection jusque 2100",
      size = "l",
      tags$div(
        style = "max-width: 700px; white-space: normal; word-wrap: break-word;",
        p("Lien: https://ec.europa.eu/eurostat/databrowser/view/demo_pjangroup/default/table?lang=en"),
        p("Date de publication: 28 juin 2023"),
        p("Source: Eurostat"),
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  # Bouton Evolution de la population 65 ans et plus
  observeEvent(input$info_button_pop65, {
    showModal(modalDialog(
      title = "Evolution de la population de 65 ans et plus avec projection jusque 2100",
      size = "l",
      tags$div(
        style = "max-width: 700px; white-space: normal; word-wrap: break-word;",
        p("Lien: https://ec.europa.eu/eurostat/databrowser/view/proj_23np/default/table?lang=en"),
        p("Date de publication: 28 juin 2023"),
        p("Source: Eurostat"),
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  # Graphique Part des résidents et étrangers dans les pensions
  output$graph_result_lm <- renderHighchart({
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = " ") %>%
      hc_xAxis(
        plotLines = list(
          list(
            color = "#000000",
            width = 2,
            value = 2023,
            dashStyle = "Dash",
            label = list(
              text = "Fin des données historiques",
              verticalAlign = "top",
              textAlign = "left"
            )
          )
        )
      ) %>%
      hc_yAxis(title = list(text = "Repartition"), min = 0, max = 100) %>%
      hc_tooltip(shared = TRUE, valueDecimals = 0) %>%
      hc_add_series_list(
        result_lm %>%
          mutate(Valeur = round(100*Valeur, digit = 1)) %>%
          group_by(Categorie, Type) %>%  # Groupe par catégorie ET type
          group_split() %>%
          map(~ list(
            name = paste(unique(.x$Categorie), "-", unique(.x$Type)),
            data = list_parse2(.x %>% dplyr::select(x = Annee, y = Valeur)),
            dashStyle = ifelse(unique(.x$Type) == "Historique", "Solid", "Dash"),
            color = couleurs[[unique(.x$Categorie)]],
            marker = list(enabled = FALSE)  
          ))
      )
  })
  
  # Bouton information graphique Part des résidents et étrangers dans les pensions
  observeEvent(input$info_button_res_etr, {
    showModal(modalDialog(
      title = "Evolution du nombre des pensions résidents et des pensions transférées par catégorie de pension",
      p("Lien : https://data.public.lu/fr/datasets/series-statistiques-sur-les-pensions/#/resources/dd3f0107-80cc-4bf5-b297-0a8a739f18ef"),
      p("Date de publication: 20 juin 2025"),
      p("Source: Data.public.lu"),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
}

shinyApp(ui, server)