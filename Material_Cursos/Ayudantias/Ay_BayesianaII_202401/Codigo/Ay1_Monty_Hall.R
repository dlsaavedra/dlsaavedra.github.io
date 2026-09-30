# Función para simular el juego de Monty Hall
simular_monty_hall <- function(cambiar_eleccion, n_puertas) {
  #n_puertas = 3
  #cambiar_eleccion = TRUE
  premio <- sample(1:n_puertas, 1)  # Puerta con el premio
  eleccion_inicial <- sample(1:n_puertas, 1)  # Elección inicial del concursante
  
  # Puertas restantes excluyendo la elección inicial y la puerta con el premio
  puertas_restantes <- setdiff(1:n_puertas, c(eleccion_inicial, premio))
  
  # Si la elección inicial es la puerta con el premio, el presentador puede abrir cualquiera de las otras dos puertas
  # Si la elección inicial no es la puerta con el premio, el presentador abre la otra puerta sin el premio
  puerta_abierta <- if (length(puertas_restantes) == 1) puertas_restantes else sample(x= puertas_restantes, size = n_puertas - 2, replace = FALSE)
  
  # Elección final dependiendo de si el concursante cambia su elección o no
  if (cambiar_eleccion){
    puerta_restantes_final = setdiff(1:n_puertas, c(eleccion_inicial, puerta_abierta))
    eleccion_final <- if (length(puerta_restantes_final) == 1) puerta_restantes_final else sample(x= puerta_restantes_final, size = 1)
  }
  else {eleccion_final = eleccion_inicial}
  
  # El concursante gana si la elección final es la puerta con el premio
  ganar <- eleccion_final == premio
  
  return(ganar)
}


# Simulación de Monty Hall
num_intentos <- 10000  # Número de intentos
n_puertas = 3
ganancias_cambiar <- mean(replicate(num_intentos, simular_monty_hall(TRUE,n_puertas)))
ganancias_no_cambiar <- mean(replicate(num_intentos, simular_monty_hall(FALSE, n_puertas)))

# Resultados
cat("Ganancias si cambia la elección:", ganancias_cambiar, "\n")
cat("Ganancias si no cambia la elección:", ganancias_no_cambiar, "\n")
