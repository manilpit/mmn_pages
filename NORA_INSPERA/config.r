# Last inn pakken
library(insperaNokut)
library(activeDir)

set_wd_to_current()

# Konfigurer API-nøkkel for gjeldende sesjon
configure_api_key("")

# Kjør en funksjon uten å angi API-nøkkel direkte
test_info <- get_test_info("337279121")
