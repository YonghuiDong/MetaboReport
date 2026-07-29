test_that("instrument sequence formatting is deterministic", {
  randomized <- data.frame(
    Group = c("A", "B"),
    Sample = c("A_1", "B_1"),
    Position = c("A:A1", "A:A2")
  )

  orbitrap <- MetaboReport:::format_injection_sequence(randomized, "Orbitrap")
  waters <- MetaboReport:::format_injection_sequence(randomized, "Waters")

  expect_equal(nrow(orbitrap), 2)
  expect_identical(orbitrap$`File Name`, randomized$Sample)
  expect_identical(orbitrap$Position, randomized$Position)
  expect_identical(waters$Bottle, randomized$Position)
})
