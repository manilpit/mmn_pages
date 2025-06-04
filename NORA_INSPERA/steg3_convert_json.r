library(jsonlite)
library(dplyr)
library(tidyr)

parse_inspera_json <- function(json_file) {
  # Les JSON-filen
  json_data <- fromJSON(json_file)
  
  # Debug: Skriv ut struktur
  cat("JSON struktur:\n")
  str(json_data$candidates$evaluationRounds[[1]], max.level = 2)
  
  # Lag en liste for å lagre alle kandidat-rader
  all_rows <- list()
  
  # Iterer gjennom hver rad i candidates data.frame
  for (i in 1:nrow(json_data$candidates)) {
    cat("\nBehandler kandidat", i, "\n")
    
    # Grunnleggende kandidatinfo
    base_info <- list(
      assessmentRunId = json_data$assessmentRunId,
      assessmentRunTitle = json_data$assessmentRunTitle,
      assessmentRunStartTime = json_data$assessmentRunStartTime,
      assessmentRunEndTime = json_data$assessmentRunEndTime,
      userId = json_data$candidates$userId[i],
      candidateId = json_data$candidates$candidateId[i]
    )
    
    # Legg til feideId hvis det finnes
    if (!is.null(json_data$candidates$externalUserIds[[i]])) {
      feide_info <- json_data$candidates$externalUserIds[[i]]
      if (is.data.frame(feide_info) && "externalUserId" %in% names(feide_info)) {
        base_info$feideId <- feide_info$externalUserId[1]
      }
    }
    
    # Behandle evalueringsrunder
    eval_rounds <- json_data$candidates$evaluationRounds[[i]]
    cat("Evalueringsrunder struktur:\n")
    str(eval_rounds)
    
    if (length(eval_rounds) > 0) {
      # Hvis eval_rounds er en dataframe
      if (is.data.frame(eval_rounds)) {
        rounds <- list(eval_rounds)
      } else {
        rounds <- eval_rounds
      }
      
      for (round_idx in seq_along(rounds)) {
        round <- rounds[[round_idx]]
        cat("Behandler runde:", round_idx, "\n")
        str(round)
        
        round_info <- base_info
        
        # Sjekk om round er en dataframe
        if (is.data.frame(round)) {
          round_info$roundId <- round$roundId[1]
          round_info$automaticQuestionScore <- round$automaticQuestionScore[1]
          
          # Behandle roundGrades hvis det finnes
          if ("roundGrades" %in% names(round) && !is.null(round$roundGrades[[1]])) {
            grades <- round$roundGrades[[1]]
            if (is.data.frame(grades)) {
              for (j in 1:nrow(grades)) {
                round_info[[paste0("sensor_", grades$evaluatorUserId[j], "_score")]] <- grades$totalScore[j]
              }
            }
          }
          
          # Behandle questions hvis det finnes
          if ("questions" %in% names(round) && !is.null(round$questions[[1]])) {
            questions <- round$questions[[1]]
            if (is.data.frame(questions)) {
              for (j in 1:nrow(questions)) {
                question_info <- round_info
                question_info$questionNumber <- questions$questionNumber[j]
                question_info$questionName <- questions$questionName[j]
                question_info$questionId <- questions$questionId[j]
                question_info$userQuestionId <- questions$userQuestionId[j]
                question_info$questionWeight <- questions$questionWeight[j]
                
                # Behandle evaluations
                if ("evaluations" %in% names(questions) && !is.null(questions$evaluations[[j]])) {
                  evals <- questions$evaluations[[j]]
                  if (is.data.frame(evals)) {
                    for (k in 1:nrow(evals)) {
                      if (evals$evaluatorId[k] == -1) {
                        question_info$auto_score <- evals$score[k]
                      } else {
                        question_info[[paste0("evaluator_", evals$evaluatorId[k], "_score")]] <- evals$score[k]
                      }
                    }
                  }
                }
                
                all_rows[[length(all_rows) + 1]] <- question_info
              }
            }
          }
        }
      }
    }
  }
  
  # Konverter listen til dataframe
  if (length(all_rows) > 0) {
    df <- bind_rows(lapply(all_rows, as.data.frame))
    
    # Rydd opp i kolonnetyper
    df <- df %>%
      mutate(
        assessmentRunId = as.numeric(assessmentRunId),
        assessmentRunStartTime = as.POSIXct(assessmentRunStartTime),
        assessmentRunEndTime = as.POSIXct(assessmentRunEndTime),
        userId = as.numeric(userId),
        candidateId = as.character(candidateId),
        roundId = as.numeric(roundId),
        questionNumber = as.character(questionNumber),
        questionId = as.numeric(questionId),
        userQuestionId = as.numeric(userQuestionId),
        questionWeight = as.numeric(questionWeight)
      )
    
    return(df)
  } else {
    stop("Ingen data kunne ekstraheres")
  }
}

# Test funksjonen med feilhåndtering
tryCatch({
  df <- parse_inspera_json("eksport_1936953499.json")
  print(head(df))
  write.csv(df, "inspera_resultater.csv", row.names = FALSE)
}, error = function(e) {
  cat("Feil:", conditionMessage(e), "\n")
})