#' Analise Malha Input
#' 
#' @import shiny
#' @import shinydashboard
#' 

analiseMalhaInput = function(id) {
    ns = NS(id)
    tagList(
        fluidRow(
            column(
                width = 4,
                offset = 4,
                align = "center",
                valueBoxOutput(
                    width = NULL,
                    outputId = ns("intersecaoKM")
                )
            )
            
        ),
        fluidRow(
            box(width = 12,
                title = "Malha Cicloviária da Cidade do Recife",
                solidHeader = T,
                status = "primary",
               
                leafletOutput(
                    outputId = ns("myMap"),
                    height = 800
                )
            )
        )
    )
}

#' Analise Malha Server
#' 
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import stringr
#' @import sf

analiseMalhaServer = function(id, nome_variavel) {
    stopifnot(is.reactive(nome_variavel))
    
    moduleServer(
        id,
        function(input, output, session) {
            # shapefile da malha cicloviaria permanente:
            malhaPermanente <- reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/malhaPermanente.rds"),
                valueFunc = function() readRDS("data/malhaPermanente.rds")
            )
            
            # shapefile da malha cicloviaria permanente:
            malhaOperacional <- reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/malhaOperacional.rds"),
                valueFunc = function() readRDS("data/malhaOperacional.rds")
            )

            # shapefile da interseção:
            intersecao = reactive({
                calcIntersectionMalhas(
                    malhaPermanente = malhaPermanente(), 
                    malhaOperacional = malhaOperacional()
                )
            })
            
            # strava FDS:
            stravaFDS = readRDS("data/stravaFDS.rds")
            
            # filtered data:
            filteredData = reactive({
                dplyr::select(stravaFDS, "name", nome_variavel(), "geometry") %>% 
                tidyr::drop_na(nome_variavel())
            })
            
            # intersecao:
            output$intersecaoKM = renderValueBox({
                # total KM:
                totalKM = intersecao() %>% 
                    dplyr::summarize(total = sum(length)) %>% 
                    dplyr::pull(total)
                # formatting
                totalKM = format(round(totalKM/1e3, 2), decimal.mark = ",", big.mark = ".", nsmall=2)
                
                valueBox(
                    value = str_c(totalKM, " KM"),
                    subtitle = "Extensão da interseção entre malhas",
                    color = 'red',
                    icon = icon("road", lib = "font-awesome", verify_fa = F)
                )
            })
            
            
            # Mapa com as malhas Permanente e Operacional:
            output$myMap = renderLeaflet({
                 # icon BikePE
                iconBikePE = makeIcon(
                    iconUrl = "www/faviconBikePE.ico",
                    iconWidth = 20,
                    iconHeight = 20
                )
                
                # icon MercadoPublico
                iconMercado = makeIcon(
                    iconUrl = "www/faviconMercadoPublico.ico",
                    iconWidth = 32,
                    iconHeight = 32
                )
                
                # icon pontos turísticos
                iconSights = makeIcon(
                    iconUrl = "www/faviconSights.ico",
                    iconWidth = 32,
                    iconHeight = 32
                )
                
                # icon sava Bike
                iconSalvaBike = makeIcon(
                    iconUrl = "www/faviconSalvaBike.ico",
                    iconWidth = 35,
                    iconHeight = 35
                )
                
                # criando paleta de cores:
                pal = colorFactor(
                    palette = c("#ef3b2c", "#cb181d", "#67000d"),
                    levels = c("baixo", "médio", "alto")
                )
                
                # plot
                leaflet(filteredData()) |> 
                    addTiles() |> 
                    setView(lat = -8.0663, lng = -34.9321, zoom = 13) %>% 
                    addLayersControl(
                        overlayGroups = c("Tráfego Reportado no Domingo (Strava)",
                                          "Malha Permanente", 
                                          "Malha Operacional",
                                          "Interseção entre malhas",
                                          "Estação Bike PE",
                                          "Estação Salva Bike",
                                          "Mercados Públicos", 
                                          "Pontos Turísticos"),
                        options = layersControlOptions(collapsed = FALSE)
                    ) %>% 
                    addPolylines(
                        data = st_as_sf(filteredData()),
                        color = ~pal(get(nome_variavel())),
                        label = ~name,
                        popup = ~paste(name, "<br>", "Nível tráfego:", get(nome_variavel())),
                        labelOptions = labelOptions(direction = "top"),
                        group = "Tráfego Reportado no Domingo (Strava)"
                    ) %>% 
                    addPolylines(
                        data = malhaPermanente(), 
                        color = "green", 
                        group = "Malha Permanente",
                        label = ~Logradouro,
                        popup = ~paste(Nome, "<br>", "Sentido:", Sentido),
                    ) |> 
                    addPolylines(
                        data = malhaOperacional(), 
                        group = "Malha Operacional",
                        label = ~Nome,
                        popup = ~paste(Nome, "<br>", "Sentido:", Sentido),
                    ) |> 
                    addPolylines(
                        data = dplyr::filter(intersecao(), Sentido == Sentido.1), 
                        color = "#000000", 
                        label = "Interseção",
                        popup = ~paste(Logradouro, "<br>", "Extensão: ", round(length/1e3, 2), "KM"),
                        group = "Interseção entre malhas"
                    ) |> 
                    addMarkers(
                        data = bikePE,
                        lng = ~longitude, lat = ~latitude,
                        label = ~nome2, popup = ~content,
                        icon = iconBikePE,
                        group = "Estação Bike PE"
                    ) %>% 
                    addMarkers(
                        data = salvaBike,
                        lng = ~longitude, lat = ~latitude,
                        label = ~local,
                        icon = iconSalvaBike,
                        group = "Estação Salva Bike"
                    ) %>%
                    addMarkers(
                        data = mercados, icon = iconMercado,
                        label = ~nome,
                        lng = ~longitude,
                        lat = ~latitude,
                        group = "Mercados Públicos"
                    ) |>
                    addMarkers(
                        data = dplyr::filter(pontosTuristicos, !str_detect(PTurist, "Mercado")),
                        icon = iconSights,
                        label = ~PTurist,
                        group = "Pontos Turísticos"
                    ) %>% 
                    addLegend(
                        "bottomright",
                        title= "Nível de Tráfego",
                        pal = pal,
                        values = ~get(nome_variavel()),
                        opacity = 1,
                        group = "Tráfego Reportado no Domingo (Strava)"
                    ) %>%
                    hideGroup(
                        group = c("Mercados Públicos", 
                                  "Pontos Turísticos",
                                  "Estação Bike PE",
                                  "Estação Salva Bike")
                    )
            })

        }
    )
}


#' Analise Malha App
#' 
#' @import shiny
#' @import shinydashboard
#' 
analiseMalhaApp = function() {
    ui = dashboardPage(
        header = dashboardHeader(title = "Turismo e Lazer"),
        sidebar = dashboardSidebar(
            sidebarMenu(
                id = "sidebarid",
                menuItem(
                    text = "Ciclo de Turismo e Lazer",
                    tabName = "analise_malha",
                    icon = icon("road", lib = "font-awesome", verify_fa=F)
                ),
                conditionalPanel(
                    'input.sidebarid == "analise_malha"',
                    selectInput(
                        inputId = "nome_variavel", 
                        "Selecionar variável:", 
                        choices = c("Número de viagens" = "trip_count_cat", 
                                    "Número de viagens (trabalho)" = "commute_trip_count_cat",
                                    "Número de viagens (lazer)" = "leisure_trip_count_cat",
                                    "Número de viagens (manhã)" = "morning_trip_count_cat",
                                    "Número de viagens (noite)" = "evening_trip_count_cat",
                                    "Número de pessoas" = "people_count_cat",
                                    "Número de pessoas (homens)" = "male_people_count_cat",
                                    "Número de pessoas (mulher)" = "female_people_count_cat",
                                    "Número de pessoas (idade 13-19)" = "age_13_19_people_count_cat",
                                    "Número de pessoas (idade 20-34)" = "age_20_34_people_count_cat",
                                    "Número de pessoas (idade 35-54)" = "age_35_54_people_count_cat",
                                    "Número de pessoas (idade 55-64)" = "age_55_64_people_count_cat",
                                    "Número de pessoas (idade 65+)" = "age_65_plus_people_count_cat"
                                    )
                    )
                )
            )
        ),
        body = dashboardBody(
            waiter::use_waiter(),
            waiter::autoWaiter(
                color = waiter::transparent(.5),
                html = waiter::spin_3() # use a spinner
            ),
            tabItems(
                tabItem(
                    tabName = "analise_malha",
                    analiseMalhaInput("id1")
                )
            )
        )
    )
    server = function(input, output, session){
        var = reactive(input$nome_variavel)
        analiseMalhaServer("id1", nome_variavel = var)
    }
    shinyApp(ui, server)
}

