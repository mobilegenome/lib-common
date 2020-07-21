all.files <- list.files(".", pattern = "*.R$", recursive = T, full.names = T)
selected.files <- all.files[grep("tests|lib-common", all.files, invert = T)]
lapply(selected.files, source)
