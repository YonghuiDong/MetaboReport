#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  state <- new_app_state()
  ids <- app_module_ids

  mod_01_home_server(ids[["home"]])
  input_data <- mod_02_uploadData_server(ids[["upload"]], sfData = state)
  mod_03_preprocess_server(ids[["preprocess"]], sfData = state)
  results <- mod_04_viewResult_server(ids[["statistics"]], sfData = state)
  mod_05_downloadReport_server(
    ids[["report"]],
    sfData = state,
    inputData = input_data,
    resultList = results
  )
  mod_06_repCheck_server(ids[["replicate_check"]])
  mod_07_randomizer_server(ids[["randomizer"]])
  mod_08_lcTool_server(ids[["lc_tool"]])
  mod_09_msTool_server(ids[["ms_tool"]])
  mod_11_idTool_server(ids[["identification"]])
  mod_10_contact_server(ids[["contact"]])
}
