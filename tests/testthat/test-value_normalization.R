# Value normalization is pure computation on an already retrieved table, so these tests run
# offline on synthetic data shaped like what the vector and table pathways hand to
# normalize_cansim_values(). Vector and coordinate data carries UOM columns since 0.4.5, which
# makes percentage normalization apply to it the same way it always has to full table downloads.

vector_style_data <- function(language=c("eng","fra")) {
  language <- match.arg(language)
  if (language=="eng") {
    data <- dplyr::tibble(
      REF_DATE=c("2020-01","2020-01","2020-01"),
      VALUE=c(50,50,2),
      SCALAR_ID=c("0","0","3"),
      UOM=c("Percentage","Percentage points","Dollars"),
      COORDINATE=c("1.1","1.2","1.3")
    )
  } else {
    data <- dplyr::tibble(
      REF_DATE=c("2020-01","2020-01","2020-01"),
      VALEUR=c(50,50,2),
      SCALAR_ID=c("0","0","3"),
      UOM=c("Pourcentage","Pourcent","Dollars"),
      COORDINATE=c("1.1","1.2","1.3")
    )
    names(data) <- c(paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),
                     "VALEUR",
                     "IDENTIFICATEUR SCALAIRE",
                     paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),
                     paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  }
  attr(data,"language") <- language
  attr(data,"cansimTableNumber") <- "99-10-0001"
  data
}

test_that("percentage units are normalized and relabelled", {
  result <- normalize_cansim_values(vector_style_data("eng"), factors=FALSE, internal=TRUE)

  # anything whose unit starts with "Percent" is divided by 100 in val_norm, everything else only
  # picks up its scalar factor
  expect_equal(result$val_norm, c(50/100, 50/100, 2*10^3))
  # the raw value column is left alone
  expect_equal(result$VALUE, c(50, 50, 2))
  # the unit follows the value, a divided percentage is no longer a percentage
  expect_equal(result$UOM, c("Rate","Rate","Dollars"))
})

test_that("percentage normalization can be turned off", {
  result <- normalize_cansim_values(vector_style_data("eng"), normalize_percent=FALSE,
                                    factors=FALSE, internal=TRUE)

  expect_equal(result$val_norm, c(50, 50, 2*10^3))
  expect_equal(result$UOM, c("Percentage","Percentage points","Dollars"))
})

test_that("percentage units in French tables are relabelled in French", {
  result <- normalize_cansim_values(vector_style_data("fra"), factors=FALSE, internal=TRUE)
  uom_column <- paste0("UNIT",intToUtf8(0x00C9)," DE MESURE")

  expect_equal(result$val_norm, c(50/100, 50/100, 2*10^3))
  expect_equal(result[[uom_column]], c("Taux","Taux","Dollars"))
})
