test_that("VR12_scale rescales GH1", {
  expect_equal(
    data.frame(GH1 = 5:1) %>% VR12_scale(),
    data.frame(GH1 = c(0, 35, 60, 85, 100))
  )
})

test_that("VR12_scale rescales PF2", {
  expect_equal(
    data.frame(PF2 = 1:2) %>% VR12_scale(),
    data.frame(PF2 = c(0, 50))
  )
})

test_that("VR12_scale rescales PF4", {
  expect_equal(
    data.frame(PF4 = 1:2) %>% VR12_scale(),
    data.frame(PF4 = c(0, 50))
  )
})

test_that("VR12_scale rescales RP2", {
  expect_equal(
    data.frame(RP2 = 5:1) %>% VR12_scale(),
    data.frame(RP2 = c(0, 25, 50, 75, 100))
  )
})

test_that("VR12_scale rescales RP3", {
  expect_equal(
    data.frame(RP3 = 5:1) %>% VR12_scale(),
    data.frame(RP3 = c(0, 25, 50, 75, 100))
  )
})

test_that("VR12_scale rescales RE2", {
  expect_equal(
    data.frame(RE2 = 5:1) %>% VR12_scale(),
    data.frame(RE2 = c(0, 25, 50, 75, 100))
  )
})

test_that("VR12_scale rescales RE3", {
  expect_equal(
    data.frame(RE3 = 5:1) %>% VR12_scale(),
    data.frame(RE3 = c(0, 25, 50, 75, 100))
  )
})

test_that("VR12_scale rescales BP2", {
  expect_equal(
    data.frame(BP2 = 5:1) %>% VR12_scale(),
    data.frame(BP2 = c(0, 25, 50, 75, 100))
  )
})

test_that("VR12_scale rescales MH3", {
  expect_equal(
    data.frame(MH3 = 6:1) %>% VR12_scale(),
    data.frame(MH3 = c(0, 20, 40, 60, 80, 100))
  )
})

test_that("VR12_scale rescales VT2", {
  expect_equal(
    data.frame(VT2 = 6:1) %>% VR12_scale(),
    data.frame(VT2 = c(0, 20, 40, 60, 80, 100))
  )
})

test_that("VR12_scale rescales MH4", {
  expect_equal(
    data.frame(MH4 = 1:6) %>% VR12_scale(),
    data.frame(MH4 = c(0, 20, 40, 60, 80, 100))
  )
})

test_that("VR12_scale rescales SF2", {
  expect_equal(
    data.frame(SF2 = 1:5) %>% VR12_scale(),
    data.frame(SF2 = c(0, 25, 50, 75, 100))
  )
})

test_VR12_frame <- data.frame(
  GH1 = 2:5, PF2 = rep(1:2, 2), PF4 = c(1, 1, 2, 2), RP2 = 1:4, RP3 = 4:1,
  RE2 = 2:5, RE3 = 5:2, BP2 = c(1:2, 4:5), MH3 = 3:6, VT2 = 1:4, MH4 = 2:5,
  SF2 = 1:4)

test_that("VR12_score computes Phone PCS", {
  expect_equal(
    test_VR12_frame %>% VR12_score(everything(), "PCS") %>% pull("PCS"),
    c(41.89309, 39.83210, 31.65715, 28.81364),
    tolerance = 1e-5
  )
})

test_that("VR12_score computes Phone MCS", {
  expect_equal(
    test_VR12_frame %>% VR12_score(everything(), "MCS") %>% pull("MCS"),
    c(31.86148, 31.71162, 34.14205, 34.00135),
    tolerance = 1e-5
  )
})
