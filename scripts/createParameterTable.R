createParameterTable <- function(output_filename="table", s, type="pdf_document"){
  # output_filename ... user-define name of output file 
  # s ... parameter master
  # type ... can be "word_document" or "pdf_document"
  
  require(rmarkdown)
  filename="scripts/Table.Rmd"
  rmarkdown::render(input = filename, output_file = output_filename, output_format = type, output_dir = getwd())
}