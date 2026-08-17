# A successful WDS data record of the shape every vector data method answers with, for tests that
# exercise the handling around the data rather than the data itself. The vectorId has to match the
# vector a test asks for, or the renaming the tests below cover would have nothing to match.
mock_vector_data_record <- function(vectorId=41690973, productId=18100004,
                                    coordinate="2.2.0.0.0.0.0.0.0.0") {
  list(status="SUCCESS", object=list(
    responseStatusCode=0, vectorId=vectorId, productId=productId, coordinate=coordinate,
    vectorDataPoint=list(list(refPer="2020-01-01", refPer2="", value=1, decimals=0,
                              scalarFactorCode=0, symbolCode=0, statusCode=0,
                              securityLevelCode=0, releaseTime="2020-01-01T08:30",
                              frequencyCode=12))))
}

# stands in for the per-table metadata lookup, which would otherwise ask StatCan
mock_metadata_for_coordinates <- function(cansimTableNumber, coordinates, language) {
  tibble::tibble(cansimTableNumber=cansimTableNumber, COORDINATE=coordinates)
}
