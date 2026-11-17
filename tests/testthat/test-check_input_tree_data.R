test_that("Check all possible messages from check_input_tree_data", {

  expect_error_exact(check_input_tree_data(verbose = FALSE),"Input 'tree_data' must be a data.frame object")


  df <- data.frame()
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Input data.frame 'tree_data' must contain a column labelled 'idplot'")


  df <- data.frame(idplot = 1:2)
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Column 'idplot' in data.frame 'tree_plot' must be of character type")
  df <- data.frame(idplot = c("1", as.character(1:2)))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Index in 'idplot' column must be unique for each row")


  df <- data.frame(idplot = as.character(1:2), x = 3:4)
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Input coordinates must be present as columns 'x' and 'y' in 'tree_data'")
  df <- data.frame(idplot = as.character(1:2), x = 3:4, y = c("3", "8"))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Input coordinates 'x' and 'y' must be of numeric type")


  df <- data.frame(idplot = as.character(1:2), x = 3:4, y = c(3, 8))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Input data.frame 'tree_data' must contain at least one column labelled 'seedlings', 'saplings' or 'trees'")


  df <- data.frame(idplot = as.character(1:2), x = 3:4, y = c(3, 8), seedlings = 1:2)
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "'seedlings' must contain data.frames in each row")
  df <- data.frame(idplot = as.character(1:2), x = 3:4, y = c(3, 8), saplings = 1:2)
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "'saplings' must contain data.frames in each row")
  df <- data.frame(idplot = as.character(1:2), x = 3:4, y = c(3, 8), trees = 1:2)
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "'trees' must contain data.frames in each row")


  df <- data.frame(idplot = as.character(1:2), x = 3:4, y = c(3, 8), seedlings = I(replicate(2, data.frame(), simplify = FALSE)))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'seedlings' cannot be empty")
  names(df)[names(df) == "seedlings"] <- "saplings"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'saplings' cannot be empty")
  names(df)[names(df) == "saplings"] <- "trees"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'trees' cannot be empty")


  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = c(3, 8),
                   seedlings = I(replicate(2, data.frame(dummy = 1:2, species = c("as", "bb")), simplify = FALSE)))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'seedlings' must have two columns 'n' and 'species'")
  names(df)[names(df) == "seedlings"] <- "saplings"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'saplings' must have two columns 'n' and 'species'")
  names(df)[names(df) == "saplings"] <- "trees"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'trees' must have two columns 'dbh' and 'species'")


  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   seedlings = I(list(data.frame(dbh = c("a", "b"), species = letters[1:2]), data.frame(n = c("a", "b"), species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'seedlings' must have two columns 'n' and 'species'")
  names(df)[names(df) == "seedlings"] <- "saplings"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'saplings' must have two columns 'n' and 'species'")
  names(df)[names(df) == "saplings"] <- "trees"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "data.frames 'trees' must have two columns 'dbh' and 'species'")


  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   seedlings = I(list(data.frame(n = c("a", "b"), species = letters[1:2]), data.frame(n = c("a", "b"), species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Wrong column types in 'seedlings'")
  names(df)[names(df) == "seedlings"] <- "saplings"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Wrong column types in 'saplings'")
  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   trees = I(list(data.frame(dbh = c("a", "b"), species = letters[1:2]), data.frame(dbh = c("a", "b"), species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Wrong column types in 'trees'")


  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   seedlings = I(list(data.frame(n = 1:2, species = rep("", 2)), data.frame(n = 1:2, species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Column 'species' in 'seedlings' cannot have empty strings")
  names(df)[names(df) == "seedlings"] <- "saplings"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Column 'species' in 'saplings' cannot have empty strings")
  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   trees = I(list(data.frame(dbh = 1:2, species = rep("", 2)), data.frame(dbh = 1:2, species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Column 'species' in 'trees' cannot have empty strings")


  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   seedlings = I(list(data.frame(n = 1:2, species = letters[1:2]), data.frame(n = c(-1,2), species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df), "Column 'n' in 'seedlings' must be >= 0")
  names(df)[names(df) == "seedlings"] <- "saplings"
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Column 'n' in 'saplings' must be >= 0")
  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   trees = I(list(data.frame(dbh = 1:2, species = letters[1:2]), data.frame(dbh = c(-1,2), species = letters[1:2]))))
  expect_error_exact(check_input_tree_data(df, verbose = FALSE), "Column 'dbh' in 'trees' must be >= 0")


  df <- data.frame(idplot = as.character(1:2),
                   x = 3:4, y = 5:6,
                   trees = I(list(data.frame(dbh = 1:2, species = letters[1:2]), data.frame(dbh = 3:4, species = letters[1:2]))))
  expect_no_condition(check_input_tree_data(df, verbose = FALSE))

})
