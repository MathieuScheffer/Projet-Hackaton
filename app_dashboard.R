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
  dplyr::rename("Autres dépenses" = "Autres.dépenses",
         "Autres dépenses sociales" = "Autres.dépenses.sociales")

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
        
        # FluidRow : cartes et graphique côte à côte
        fluidRow(
          # Colonne gauche : TabBox avec les cartes
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
                title = "65 ans et plus",
                active = TRUE,
                leafletOutput("map_65pct")
              ),
              
              tabPanel(
                tabName = "mapImm",
                title = "Immigration",
                leafletOutput("map_immigre")
              )
            )
          ),
          
          # Colonne droite : graphique dépenses publiques
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
                title = "Dépenses publiques luxembourgeoise par catégorie",
                active = TRUE,
                highchartOutput("graph_depense_publique_cate")
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
              
              tabPanel(
                tabName = "pop2010",
                title = "Pop. jusque 2100",
                active = TRUE,
                highchartOutput("df_pop_jsq_2100")
              ),
              
              tabPanel(
                tabName = "pop2010_65ans_plus",
                title = "Pop. 65 ans et plus jusque 2100",
                highchartOutput("df_pop_jsq_2100_65plus")
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
                highchartOutput("graph_result_lm")
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
  
  output$map_immigre <- renderLeaflet({
      map_immigre_canton
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
      hc_yAxis(title = list(text = "Valeur"), min = 0, max = 100) %>%
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
  
}

shinyApp(ui, server)