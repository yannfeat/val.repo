context("Test main api functions")
library(tibble)
httptest::with_mock_api({
  test_that("Ask for api key before making any request", {
    expect_error(get_installation_by_id(8077), "You have to set apikey first! See set_apikey function.")
    expect_error(get_nearest_installations(50.11670, 19.91429), "You have to set apikey first! See set_apikey function.")
    expect_error(get_nearest_measurements(50.11670, 19.91429), "You have to set apikey first! See set_apikey function.")
    expect_error(get_point_measurements(50.11670, 19.91429), "You have to set apikey first! See set_apikey function.")
    expect_error(get_installation_measurements(8077), "You have to set apikey first! See set_apikey function.")
    expect_error(get_indexes(), "You have to set apikey first! See set_apikey function.")
    expect_error(get_measurements_info(), "You have to set apikey first! See set_apikey function.")
    expect_error(remaining_requests(), "You have to set apikey first! See set_apikey function.")
  })
})


test_that("Apikey is set correctly", {
  set_apikey("testkey")
  key <- .get_apikey()
  expect_equal(key, "testkey")
})

test_that("Api key takes only string as argument", {
  expect_error(set_apikey(123456789), "apikey must be a string")
})


httptest::with_mock_api({
  test_that("Get installation by id is working", {
    set_apikey("testkey")
    station <- get_installation_by_id(8077)
    expected <- tibble(id = 8077,
                       elevation = 2137,
                       is_airly = TRUE,
                       location = tibble(latitude = 50,
                                         longitude = 19),
                       address = tibble(country = "Poland",
                                        city = "Kraków",
                                        street = "Mikołajska",
                                        number = "4",
                                        displayAddress1 = "Kraków",
                                        displayAddress2 = "Mikołajska"),
                       sponsor = tibble(id = 489,
                                        name ="Chatham Financial",
                                        description = "Airly Sensor's sponsor",
                                        logo = "https://cdn.airly.eu/some.jpg",
                                        link = "https://crossweb.pl")
                       )
    expect_equal(station, expected)
  })
})


httptest::with_mock_api({
  test_that("Get nearest installation is working", {
    set_apikey("testkey")
    station <- get_nearest_installations(lat = 50, lng = 19)[2,]
    expected <- tibble(id = 8077,
                       elevation = 2137,
                       is_airly = TRUE,
                       location = tibble(latitude = 50,
                                         longitude = 19),
                       address = tibble(country = "Poland",
                                        city = "Kraków",
                                        street = "Mikołajska",
                                        number = "4",
                                        displayAddress1 = "Kraków",
                                        displayAddress2 = "Mikołajska"),
                       sponsor = tibble(id = 489,
                                        name ="Chatham Financial",
                                        description = "Airly Sensor's sponsor",
                                        logo = "https://cdn.airly.eu/some.jpg",
                                        link = "https://crossweb.pl")
    )
    expect_equal(station, expected)
  })
})


httptest::with_mock_api({
  test_that("Get nearest measurements is working", {
    set_apikey("testkey")
    current <- get_nearest_measurements(lat = 21, lng = 37)$current
    history <- get_nearest_measurements(lat = 21, lng = 37)$history[1:7,]
    forecast <- get_nearest_measurements(lat = 21, lng = 37)$forecast[1:7,]
    exp_curr <- tibble(time = tibble(from = as.POSIXct(strptime("2020-03-11T10:43:29.983Z", format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime("2020-03-11T11:43:29.983Z", format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM1 = 4.58,
                                        PM25 = 6.83,
                                        PM10 = 12.54,
                                        PRESSURE = 1010.3,
                                        HUMIDITY = 77.69,
                                        TEMPERATURE = 10.82),
                       index = tibble(AIRLY_CAQI = 12.54)
                       )

    exp_hist <- tibble(time = tibble(from = as.POSIXct(strptime(c("2020-03-10T11:00:00.000Z",
                                                                  "2020-03-10T12:00:00.000Z",
                                                                  "2020-03-10T13:00:00.000Z",
                                                                  "2020-03-10T14:00:00.000Z",
                                                                  "2020-03-10T15:00:00.000Z",
                                                                  "2020-03-10T16:00:00.000Z",
                                                                  "2020-03-10T17:00:00.000Z"),
                                                                format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime(c("2020-03-10T12:00:00.000Z",
                                                                "2020-03-10T13:00:00.000Z",
                                                                "2020-03-10T14:00:00.000Z",
                                                                "2020-03-10T15:00:00.000Z",
                                                                "2020-03-10T16:00:00.000Z",
                                                                "2020-03-10T17:00:00.000Z",
                                                                "2020-03-10T18:00:00.000Z"),
                                                              format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM1 = c(9.87, 8.40, 7.45, 7.70, 7.99, 8.52, 13.64),
                                        PM25 = c(14.16, 11.86, 10.53, 10.99, 11.60, 11.89, 19.93),
                                        PM10 = c(25.74, 21.66, 19.16, 20.31, 21.60, 21.42, 37.58),
                                        PRESSURE = c(1013.37, 1012.38, 1011.55, 1010.89, 1010.23, 1010.26, 1010.37),
                                        HUMIDITY = c(66.01, 58.89, 54.92, 53.60, 55.20, 59.38, 64.65),
                                        TEMPERATURE = c(10.75, 11.50, 11.91, 11.96, 11.39, 10.24, 8.92)),
                       index = tibble(AIRLY_CAQI = c(21.66, 21.66, 19.16, 20.31, 21.60, 21.42,37.58))
                       )

    exp_fore <- tibble(time = tibble(from = as.POSIXct(strptime(c("2020-03-11T11:00:00.000Z",
                                                                  "2020-03-11T12:00:00.000Z",
                                                                  "2020-03-11T13:00:00.000Z",
                                                                  "2020-03-11T14:00:00.000Z",
                                                                  "2020-03-11T15:00:00.000Z",
                                                                  "2020-03-11T16:00:00.000Z",
                                                                  "2020-03-11T17:00:00.000Z"),
                                                                format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime(c("2020-03-11T12:00:00.000Z",
                                                                "2020-03-11T13:00:00.000Z",
                                                                "2020-03-11T14:00:00.000Z",
                                                                "2020-03-11T15:00:00.000Z",
                                                                "2020-03-11T16:00:00.000Z",
                                                                "2020-03-11T17:00:00.000Z",
                                                                "2020-03-11T18:00:00.000Z"),
                                                              format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM25 = c(2.07, 0.38, 0.10, 0.10, 0.45, 2.70, 5.33),
                                        PM10 = c(7.82, 5.75, 4.28, 5.09, 7.35, 10.37, 13.86)),
                       index = tibble(AIRLY_CAQI = c(7.82, 5.75, 4.28, 5.09, 7.35, 10.37, 13.86))
    )
    expect_equal(history, exp_hist)
    expect_equal(current, exp_curr)
    expect_equal(forecast, exp_fore)
  })
})

httptest::with_mock_api({
  test_that("Get id measurements is working", {
    set_apikey("testkey")
    current <- get_installation_measurements(id = 5)$current
    history <- get_installation_measurements(id = 5)$history[1:7,]
    forecast <- get_installation_measurements(id = 5)$forecast[1:7,]

    exp_curr <- tibble(time = tibble(from = as.POSIXct(strptime("2020-03-11T18:58:36.380Z", format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime("2020-03-11T19:58:36.380Z", format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM1 = 8.73,
                                        PM25 = 12.37,
                                        PM10 =  23.29,
                                        PRESSURE = 1014.23,
                                        HUMIDITY = 81.05,
                                        TEMPERATURE = 8.27),
                       index = tibble(AIRLY_CAQI = 23.29)
    )

    exp_hist <- tibble(time = tibble(from = as.POSIXct(strptime(c("2020-03-10T19:00:00.000Z",
                                                                  "2020-03-10T20:00:00.000Z",
                                                                  "2020-03-10T21:00:00.000Z",
                                                                  "2020-03-10T22:00:00.000Z",
                                                                  "2020-03-10T23:00:00.000Z",
                                                                  "2020-03-11T00:00:00.000Z",
                                                                  "2020-03-11T01:00:00.000Z"),
                                                                format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime(c("2020-03-10T20:00:00.000Z",
                                                                "2020-03-10T21:00:00.000Z",
                                                                "2020-03-10T22:00:00.000Z",
                                                                "2020-03-10T23:00:00.000Z",
                                                                "2020-03-11T00:00:00.000Z",
                                                                "2020-03-11T01:00:00.000Z",
                                                                "2020-03-11T02:00:00.000Z"),
                                                              format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM1 = c(16.30, 10.72,  9.97,  7.42,  2.95,  4.12, 3.17),
                                        PM25 = c(24.18, 15.42, 14.13, 10.53,  4.55,  6.23, 4.87),
                                        PM10 = c(46.03, 28.37, 25.84, 19.07,  8.57, 11.55, 9.33),
                                        PRESSURE = c(1009.94, 1009.13, 1008.49, 1007.75,  1007.17, 1006.81, 1006.01),
                                        HUMIDITY = c(66.36, 66.55, 67.21, 69.24, 71.58, 73.79, 77.09),
                                        TEMPERATURE = c(7.84, 7.42, 7.03, 7.04, 7.19, 7.42, 7.40)),
                       index = tibble(AIRLY_CAQI = c(46.03, 28.37, 25.84, 19.07,  8.57, 11.55, 9.33))
    )

    exp_fore <- tibble(time = tibble(from = as.POSIXct(strptime(c("2020-03-11T19:00:00.000Z",
                                                                  "2020-03-11T20:00:00.000Z",
                                                                  "2020-03-11T21:00:00.000Z",
                                                                  "2020-03-11T22:00:00.000Z",
                                                                  "2020-03-11T23:00:00.000Z",
                                                                  "2020-03-12T00:00:00.000Z",
                                                                  "2020-03-12T01:00:00.000Z"),
                                                                format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime(c("2020-03-11T20:00:00.000Z",
                                                                "2020-03-11T21:00:00.000Z",
                                                                "2020-03-11T22:00:00.000Z",
                                                                "2020-03-11T23:00:00.000Z",
                                                                "2020-03-12T00:00:00.000Z",
                                                                "2020-03-12T01:00:00.000Z",
                                                                "2020-03-12T02:00:00.000Z"),
                                                              format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM25 = c(10.22, 9.22, 7.53, 5.57, 3.82, 2.47, 1.57),
                                        PM10 = c(18.57, 16.76, 13.75, 10.00, 6.64, 4.16, 2.44)),
                       index = tibble(AIRLY_CAQI = c(18.57, 16.76, 13.75, 10.00, 6.64, 4.16, 2.62))
    )
    expect_equal(history, exp_hist)
    expect_equal(current, exp_curr)
    expect_equal(forecast, exp_fore)
  })
})


httptest::with_mock_api({
  test_that("Get point measurements is working", {
    set_apikey("testkey")
    current <- get_point_measurements(50.11670, 19.91429)$current
    history <- get_point_measurements(50.11670, 19.91429)$history[1:7,]
    forecast <- get_point_measurements(50.11670, 19.91429)$forecast[1:7,]

    exp_curr <- tibble(time = tibble(from = as.POSIXct(strptime("2020-03-11T18:58:36.380Z", format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime("2020-03-11T19:58:36.380Z", format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM1 = 8.73,
                                        PM25 = 12.37,
                                        PM10 =  23.29,
                                        PRESSURE = 1014.23,
                                        HUMIDITY = 81.05,
                                        TEMPERATURE = 8.27),
                       index = tibble(AIRLY_CAQI = 23.29)
    )

    exp_hist <- tibble(time = tibble(from = as.POSIXct(strptime(c("2020-03-10T19:00:00.000Z",
                                                                  "2020-03-10T20:00:00.000Z",
                                                                  "2020-03-10T21:00:00.000Z",
                                                                  "2020-03-10T22:00:00.000Z",
                                                                  "2020-03-10T23:00:00.000Z",
                                                                  "2020-03-11T00:00:00.000Z",
                                                                  "2020-03-11T01:00:00.000Z"),
                                                                format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime(c("2020-03-10T20:00:00.000Z",
                                                                "2020-03-10T21:00:00.000Z",
                                                                "2020-03-10T22:00:00.000Z",
                                                                "2020-03-10T23:00:00.000Z",
                                                                "2020-03-11T00:00:00.000Z",
                                                                "2020-03-11T01:00:00.000Z",
                                                                "2020-03-11T02:00:00.000Z"),
                                                              format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM1 = c(16.30, 10.72,  9.97,  7.42,  2.95,  4.12, 3.17),
                                        PM25 = c(24.18, 15.42, 14.13, 10.53,  4.55,  6.23, 4.87),
                                        PM10 = c(46.03, 28.37, 25.84, 19.07,  8.57, 11.55, 9.33),
                                        PRESSURE = c(1009.94, 1009.13, 1008.49, 1007.75,  1007.17, 1006.81, 1006.01),
                                        HUMIDITY = c(66.36, 66.55, 67.21, 69.24, 71.58, 73.79, 77.09),
                                        TEMPERATURE = c(7.84, 7.42, 7.03, 7.04, 7.19, 7.42, 7.40)),
                       index = tibble(AIRLY_CAQI = c(46.03, 28.37, 25.84, 19.07,  8.57, 11.55, 9.33))
    )

    exp_fore <- tibble(time = tibble(from = as.POSIXct(strptime(c("2020-03-11T19:00:00.000Z",
                                                                  "2020-03-11T20:00:00.000Z",
                                                                  "2020-03-11T21:00:00.000Z",
                                                                  "2020-03-11T22:00:00.000Z",
                                                                  "2020-03-11T23:00:00.000Z",
                                                                  "2020-03-12T00:00:00.000Z",
                                                                  "2020-03-12T01:00:00.000Z"),
                                                                format = "%Y-%m-%dT%H:%M:%OSZ")),
                                     to = as.POSIXct(strptime(c("2020-03-11T20:00:00.000Z",
                                                                "2020-03-11T21:00:00.000Z",
                                                                "2020-03-11T22:00:00.000Z",
                                                                "2020-03-11T23:00:00.000Z",
                                                                "2020-03-12T00:00:00.000Z",
                                                                "2020-03-12T01:00:00.000Z",
                                                                "2020-03-12T02:00:00.000Z"),
                                                              format = "%Y-%m-%dT%H:%M:%OSZ"))),
                       measure = tibble(PM25 = c(10.22, 9.22, 7.53, 5.57, 3.82, 2.47, 1.57),
                                        PM10 = c(18.57, 16.76, 13.75, 10.00, 6.64, 4.16, 2.44)),
                       index = tibble(AIRLY_CAQI = c(18.57, 16.76, 13.75, 10.00, 6.64, 4.16, 2.62))
    )
    expect_equal(history, exp_hist)
    expect_equal(current, exp_curr)
    expect_equal(forecast, exp_fore)
  })
})

httptest::with_mock_api({
  test_that("get_indexes return valid index information", {
    indexes <- get_indexes()
    expected <- data.frame(
      minValue = c(0.0, 25.0, 50.0, 75.0, 87.5, 100.0, 125.0, 0.0, 25.0, 50.0, 75.0, 100.0, 0.0, 1.0, 3.0 , 5.0, 7.0, 10.0),
      maxValue = c(25.0, 50.0, 75.0, 87.5, 100.0, 125.0, NA, 25.0, 50.0, 75.0, 100.0, NA, 1.0, 3.0, 5.0, 7.0, 10.0, NA),
      description = c("Very Low","Low","Medium","High","Very High","Extreme","Airmageddon!", "Very Low","Low",
      "Medium","High","Very High","Very Good","Good","Moderate","Satisfactory", "Bad","Very Bad"),
      color = c("#6BC926", "#D1CF1E", "#EFBB0F", "#EF7120", "#EF2A36", "#B00057", "#770078", "#79BC6A", "#B9CE45", "#EDC100", "#F69208",
                "#960018", "#57B108", "#B0DD10", "#FFD911", "#E58100", "#E50000", "#990000"),
      name = c("AIRLY_CAQI", "AIRLY_CAQI", "AIRLY_CAQI", "AIRLY_CAQI", "AIRLY_CAQI", "AIRLY_CAQI", "AIRLY_CAQI",
               "CAQI","CAQI","CAQI", "CAQI","CAQI","PIJP","PIJP","PIJP","PIJP","PIJP","PIJP" ),
      stringsAsFactors = FALSE
      )
    expect_equal(indexes, expected)
  })
})

httptest::with_mock_api({
  test_that("get_measurements_info return valid index information", {
    measures <- get_measurements_info()
    expected <- data.frame(
      name = c("PM1", "PM25", "PM10", "TEMPERATURE", "HUMIDITY", "PRESSURE", "WIND_SPEED", "WIND_BEARING", "NO2", "O3", "SO2", "CO", "H2S", "NO"),
      label = c("PM1", "PM2.5", "PM10", "Temperature", "Humidity", "Pressure", "Wind speed", "Wind bearing", "NO₂", "O₃", "SO₂", "CO", "H₂S", "NO"),
      unit = c("µg/m³", "µg/m³", "µg/m³", "°C", "%", "hPa", "km/h", "°", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³"),
      stringsAsFactors = FALSE)
    expect_equal(measures, expected)
  })
})


test_that("remaining request are not set", {
  expect_warning(remaining_requests(), "You should make at least one request. Check me after making first call.")
})

