separar_estado_municipio <- function(codigo) {
  if (nchar(codigo) == 4) {
    estado <- substr(codigo, 1, 1)
    municipio <- substr(codigo, 2, 4)
  } else if (nchar(codigo) == 5) {
    estado <- substr(codigo, 1, 2)
    municipio <- substr(codigo, 3, 5)
  }
  return(list(estado = estado, municipio = municipio))
}