box::use(../data_manager)

pool <- pool::dbPool(
  drv = RSQLite::SQLite(),
  dbname = file.path("..", "..", config::get("db_loc"))
)

test_that("Searching works", {
  dm <- data_manager$DataManager$new(pool)
  res <- dm$search("rhipsalis")
  expect_equal(nrow(res), 63)
  expect_equal(ncol(res), 49)
})

test_that("Adding plant using numeric id works", {
  dm <- data_manager$DataManager$new(pool)
  dm$add(174297)
  expect_equal(length(dm$get()), 1)
})

test_that("Adding plant using character id works", {
  dm <- data_manager$DataManager$new(pool)
  dm$add("174297")
  expect_equal(length(dm$get()), 1)
})

test_that("Adding multiple plants works", {
  dm <- data_manager$DataManager$new(pool)
  dm$add(c(174297, 295307))
  expect_equal(length(dm$get()), 2)
})

test_that("Adding plant with invalid id throws an error", {
  dm <- data_manager$DataManager$new(pool)
  expect_error(dm$add("xxx"))
})

test_that("Adding plant with NULL id throws an error", {
  dm <- data_manager$DataManager$new(pool)
  expect_error(dm$add(NULL))
})

test_that("Removing plant works", {
  dm <- data_manager$DataManager$new(pool)
  dm$add("174297")
  dm$remove("174297")
  expect_equal(length(dm$get()), 0)
})

test_that("Getting data frame from added plants works", {
  dm <- data_manager$DataManager$new(pool)
  dm$add(c(174297, 295307))
  expect_equal(nrow(dm$get_df()), 2)
  expect_equal(ncol(dm$get_df()), 2)
  expect_equal(dm$get_df()$id, c(174297, 295307))
  expect_true(inherits(dm$get_df(), "data.frame"))
})

test_that("Checking if plant registry is empty works", {
  dm <- data_manager$DataManager$new(pool)
  expect_true(dm$empty()) 
  dm$add(174297)
  expect_false(dm$empty())
  dm$remove(174297)
  expect_true(dm$empty())
})

pool$close()