test_that("add_choice works as expected", {
  # Adding a new choice
  expect_equal(add_choice("apple banana", "orange"), "apple banana orange")

  # Adding an existing choice should not duplicate it
  expect_equal(add_choice("apple banana", "apple"), "apple banana")

  # Adding to NA or empty string
  expect_equal(add_choice(NA, "orange"), "orange")
  expect_equal(add_choice("", "orange"), "orange")

  # Vectorized inputs
  expect_equal(
    add_choice(c("apple", "banana", NA), "orange"),
    c("apple orange", "banana orange", "orange")
  )
})

test_that("remove_choice works as expected", {
  # Removing a choice
  expect_equal(remove_choice("apple banana orange", "banana"), "apple orange")

  # Removing a choice that isn't present
  expect_equal(remove_choice("apple banana", "orange"), "apple banana")

  # Removing from NA or empty string
  expect_equal(remove_choice(NA, "orange"), "")
  expect_equal(remove_choice("", "orange"), "")

  # Vectorized inputs
  expect_equal(
    remove_choice(c("apple orange", "banana orange", NA), "orange"),
    c("apple", "banana", "")
  )
})

test_that("get_ref_question works as expected", {
  expect_equal(get_ref_question("${age} > 18"), "age")
  expect_equal(get_ref_question("some random string"), NA_character_)
  expect_equal(get_ref_question(NA), NA_character_)

  # Vectorized inputs
  expect_equal(
    get_ref_question(c("${gender} == 'male'", "${age} > 18", "no_braces")),
    c("gender", "age", NA_character_)
  )
})
