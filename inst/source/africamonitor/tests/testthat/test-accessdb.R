
if(Sys.getenv("NCRAN") == "TRUE") {

data <- am_data(ctry = c("UGA", "KEN", "TZA", "RWA", "BDI", "SSD"),
                series = c("NGDP_RPCH", "PCPIPCH"), from = 2000)

expect_equal(names(data), c("ISO3", "Date", "NGDP_RPCH", "PCPIPCH"))

# Test Reshaping
expect_equal(names(am_pivot_longer(data)), c("ISO3", "Date", "Series", "Label", "Value"))
expect_equal(names(am_pivot_longer(data, label_name = NULL)), c("ISO3", "Date", "Series", "Value"))
expect_equal(data, am_pivot_wider(am_pivot_longer(data)))

# Test expand date
expect_equal(names(am_expand_date(data)), c("ISO3", "Date", "Year", "Quarter", "Month", "NGDP_RPCH", "PCPIPCH"))
expect_equal(names(am_expand_date(data, gen = c("Year", "Month"), keep.date = FALSE)), c("ISO3", "Year", "Month", "NGDP_RPCH", "PCPIPCH"))
expect_equal(names(am_expand_date(data, gen = c("Year", "Month"), keep.date = FALSE)),
             names(am_data(ctry = c("UGA", "KEN"),
                     series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, expand.date = TRUE,
                     gen = c("Year", "Month"), keep.date = FALSE)))

# Single country
data1c <- am_data(ctry = "UGA", series = c("NGDP_RPCH", "PCPIPCH"), from = 2000)
expect_equal(names(data1c), c("Date", "NGDP_RPCH", "PCPIPCH"))
data1c <- am_data(ctry = "UGA", series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, wide = FALSE)
expect_equal(names(data1c), c("Date", "Series", "Label", "Value"))
data1c <- am_data(ctry = "UGA", series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, drop.1iso3c = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "NGDP_RPCH", "PCPIPCH"))
data1c <- am_data(ctry = "UGA", series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, wide = FALSE, drop.1iso3c = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "Series", "Label", "Value"))

# Single series
data1c <- am_data(ctry = c("UGA", "KEN"), series = c("NGDP_RPCH", "PCPIPCH"), from = 2000)
expect_equal(names(data1c), c("ISO3", "Date", "NGDP_RPCH", "PCPIPCH"))
data1c <- am_data(ctry = c("UGA", "KEN"), series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, wide = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "Series", "Label", "Value"))
data1c <- am_data(ctry = c("UGA", "KEN"), series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, drop.1iso3c = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "NGDP_RPCH", "PCPIPCH"))
data1c <- am_data(ctry = c("UGA", "KEN"), series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, wide = FALSE, drop.1iso3c = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "Series", "Label", "Value"))

# Both
data1c <- am_data(ctry = "UGA", series = "PCPIPCH", from = 2000)
expect_equal(names(data1c), c("Date", "PCPIPCH"))
data1c <- am_data(ctry = "UGA", series = "PCPIPCH", from = 2000, wide = FALSE)
expect_equal(names(data1c), c("Date", "Series", "Label", "Value"))
data1c <- am_data(ctry = "UGA", series = "PCPIPCH", from = 2000, drop.1iso3c = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "PCPIPCH"))
data1c <- am_data(ctry = "UGA", series = c("NGDP_RPCH", "PCPIPCH"), from = 2000, wide = FALSE, drop.1iso3c = FALSE)
expect_equal(names(data1c), c("ISO3", "Date", "Series", "Label", "Value"))

}

