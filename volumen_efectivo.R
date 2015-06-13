datos.embalses <- read.csv("embalses.csv",
                           header=TRUE,
                           comment.char="#")

attach(datos.embalses)

altura.efectiva <- desborde - control 