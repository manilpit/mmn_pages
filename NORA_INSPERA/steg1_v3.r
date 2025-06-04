library(httr)
library(jsonlite)

# Funksjon for å bestille en eksport
order_file_export <- function(assessment_run_id, api_key = NULL) {
  auth_result <- authenticate_inspera(api_key)
  token <- auth_result$token
  
  base_url <- "https://nokut.inspera.no/api"
  endpoint <- "/v1/fileExport/order"
  
  # Korrekt formatert body
  body_data <- list(
    resourceType = "AllResults",
    parameters = list(
      assessmentRunId = assessment_run_id
    )
  )
  
  # Vis request body for debugging
  cat("Request body:\n")
  print(toJSON(body_data, auto_unbox = TRUE, pretty = TRUE))
  
  response <- POST(
    url = paste0(base_url, endpoint),
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token)
    ),
    body = toJSON(body_data, auto_unbox = TRUE),
    encode = "json",
    verbose()
  )
  
  if (http_status(response)$category == "Success") {
    result <- fromJSON(rawToChar(response$content))
    cat("\nResponse:\n")
    print(result)
    return(result)
  } else {
    stop("Feil ved bestilling av eksport: ", http_status(response)$message)
  }
}

# Test med den spesifikke assessment_run_id
result <- order_file_export("350743770")