DataManager <- R6Class(
  "DataManager",
  private = list(
    con = NULL,
    plants = NULL,
    search_cols = c("scientific_name", "genus"),
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    count = 0,
    clean_plant = function(plant) {
      plant <- plant[, -1]
      clean = function(x) {
        ifelse(x == "false", FALSE, ifelse(x == "true", TRUE, x))
      }
      
      split <- function(x) {
        res <- strsplit(x, ",")[[1]]
        if (length(res) == 0) return(NA)
        res
      }
      # browser()
      p <- map(plant, clean)
      new_p <- list(
        vegetable = p$vegetable,
        synonyms = split(p$synonyms),
        distributions = split(p$distributions),
        common_names = split(p$common_names),
        flower = list(
          color = p$flower_color,
          conspicuous = p$flower_conspicuous
        ),
        foliage = list(
          color = p$foliage_color,
          texture = p$foliage_texture
        ),
        fruit = list(
          color = p$fruit_color,
          conspicuous = p$fruit_conspicuous
        ),
        growth = list(
          form = p$growth_form,
          habit = p$growth_habit,
          months = p$growth_months,
          rate = p$growth_rate,
          average_height = p$average_height_cm,
          maximum_height = p$maximum_height_cm,
          minimum_height = p$minimum_height
        ),
        edible = list(
          is_edible = p$edible,
          part = p$edible_part
        ),
        planting = list(
          light = p$light,
          soil_nutriments = p$soil_nutriments,
          soil_salinity = p$soil_salinity,
          anaerobic_tolearne = p$anaerobic_tolerance,
          humidity = p$atmospheric_humidity,
          ph_minimum = p$ph_minimum,
          ph_maximum = p$ph_maximum,
          minimum_root_depth = p$minimum_root_depth_cm,
          days_to_harvest = p$planting_day_to_harvest,
          description = p$planting_description,
          sowing = p$planting_sowing_description,
          row_spacing = p$planting_row_spacing,
          spread = p$planting_spread_cm
        ),
        links = list(
          usda = p$url_usda,
          tropicos = p$url_tropicos,
          tela_botanica = p$url_tela_botanica,
          powo = p$url_powo,
          plantnet = p$url_plantnet,
          gbif = p$url_gbif,
          openfarm = p$url_openfarm,
          catminat = p$url_catminat,
          wikipedia = p$url_wikipedia_en
        )
      )
      c(
        p[1:10],
        new_p
      )
    }
  ),
  public = list(
    initialize = function(db) {
      con <- DBI::dbConnect(RSQLite::SQLite(), db)
      private$con <- dplyr::tbl(con, "plants")
      
      private$plants <- list()
      private$reactiveDep <- function(x) NULL
    },
    
    reactive = function() {
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    },
    
    search = function(query) {
      box::use(purrr[map])
      query <- glue::glue("%{query}%")
      result <- private$con %>% 
        dplyr::filter(
          scientific_name %like% query |
          genus %like% query |
          common_name %like% query
        ) %>% 
        dplyr::collect()
    },
    
    add = function(id) {
      id <- id[!id %in% names(private$plants)]
      walk(id, function(i) {
        plant <- private$con %>% dplyr::filter(id == i) %>% dplyr::collect()
        private$plants[[as.character(i)]] <- private$clean_plant(plant)
      })
      private$invalidate()
    },
    
    remove = function(id) {
      private$plants[[id]] <- NULL
      private$invalidate()
    },
    
    get_plants = function(id = NULL) {
      if (is.null(id)) return(private$plants)
      private$plants[id]
    }
  )
)