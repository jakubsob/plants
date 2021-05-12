box::use(
  R6[...],
  dplyr[...],
  DBI[...],
  RSQLite[...],
  glue[...],
  shiny[...],
  purrr[...]
)

#' @export
DataManager <- R6Class(
  "DataManager",
  public = list(
    initialize = function(pool) {
      private$tbl <- tbl(pool, "plants")

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
      query <- glue("%{query}%")
      private$tbl %>% 
        filter(
          scientific_name %like% query |
            genus %like% query |
            common_name %like% query
        ) %>% 
        collect()
    },
    
    add = function(id) {
      if (length(id) == 0) {
        stop("Provide id of length > 0")
      }
      private$check_id(id)
      id <- id[!id %in% names(private$plants)]
      walk(id, function(i) {
        private$plants[[as.character(i)]] <- private$tbl %>%
          filter(id == i) %>%
          collect() %>%
          private$clean_plant() 
      })
      private$invalidate()
    },
    
    remove = function(id) {
      if (length(id) != 1) stop("Can only remove one element at a time")
      private$check_id(id)
      private$plants[[as.character(id)]] <- NULL
      private$invalidate()
    },
    
    get = function(id = NULL) {
      if (is.null(id)) return(private$plants)
      if (length(id) != 1) stop("Can only retrieve one element")
      private$plants[[as.character(id)]] 
    },
    
    get_df = function() {
      map_dfr(private$plants, `[`, c("id", "scientific_name"))
    },
    
    empty = function() {
      length(private$plants) == 0
    }
  ),
  private = list(
    tbl = NULL,
    plants = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    count = 0,
    check_id = function(id) {
      if (any(!grepl("^[0-9]+$", id))) {
        stop("id must be a number")
      }
    },
    clean_plant = function(p) {
      split <- function(x) {
        res <- strsplit(x, ",")[[1]]
        if (length(res) == 0) return(NA)
        res
      }
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
          rate = p$growth_rate,
          average_height_cm = p$average_height_cm,
          maximum_height_cm = p$maximum_height_cm,
          minimum_root_depth_cm = p$minimum_root_depth_cm,
          bloom_months = p$bloom_months
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
          minimum_root_depth_cm = p$minimum_root_depth_cm,
          days_to_harvest = p$planting_days_to_harvest,
          description = p$planting_description,
          sowing = p$planting_sowing_description,
          row_spacing_cm = p$planting_row_spacing_cm,
          spread = p$planting_spread_cm
        ),
        links = list(
          usda = p$url_usda,
          tropicos = p$url_tropicos,
          powo = p$url_powo,
          plantnet = p$url_plantnet,
          gbif = p$url_gbif,
          wikipedia = p$url_wikipedia_en
        )
      )
      c(
        p[1:11],
        new_p
      )
    }
  )
)

if (is.null(box::name())) {
  box::use(./`__tests__`)
}