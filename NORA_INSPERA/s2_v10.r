library(httr)
library(jsonlite)
library(insperaNokut)

follow_export_status <- function(job_id, api_key = NULL, max_attempts = 30, sleep_time = 5) {
  # Bruk insperaNokut sin authenticate_inspera
  auth_result <- insperaNokut::authenticate_inspera(api_key)
  token <- auth_result$token
  
  for (i in 1:max_attempts) {
    cat(sprintf("\nSjekker status (forsøk %d/%d):\n", i, max_attempts))
    
    status_url <- sprintf("https://nokut.inspera.no/api/v1/fileExport/status/%s", job_id)
    
    status_response <- GET(
      url = status_url,
      add_headers(
        "Authorization" = paste("Bearer", token)
      ),
      verbose()
    )
    
    status_data <- fromJSON(rawToChar(status_response$content))
    cat("\nFullstendig status-respons:\n")
    print(status_data)
    
    if (!is.null(status_data$status)) {
      cat("\nStatus:", status_data$status, "Progress:", status_data$progress, "%\n")
    } else {
      stop("Ugyldig respons: mangler status.")
    }
    
    if (status_data$status == "success" && status_data$progress == 100) {
      cat("\nSjekker exportInfo:\n")
      print(status_data$exportInfo)
      
      download_url <- status_data$exportInfo$signedResponseUrl
      destfile <- sprintf("eksport_%s.json", job_id)
      
      cat("\nLaster ned eksportfil til:", destfile, "\n")
      download.file(download_url, destfile = destfile, mode = "wb")
      
      cat("Leser innholdet i eksportfilen...\n")
      eksport_data <- fromJSON(destfile)
      return(eksport_data)
    } else if (status_data$status == "failed") {
      stop("Eksport feilet")
    }
    
    if (i < max_attempts) {
      cat("Venter", sleep_time, "sekunder før neste sjekk...\n")
      Sys.sleep(sleep_time)
    }
  }
  
  stop("Maksimalt antall forsøk nådd uten suksess.")
}

# Kjør eksportstatus-funksjonen
eksport_data <- follow_export_status(job_id = "1936953499")

# Se på resultatet
print(eksport_data)