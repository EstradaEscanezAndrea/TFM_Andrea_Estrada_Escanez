# Librerías
{
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(reshape2)
  library(RColorBrewer)
  library(tidyr)
}

# Generación de la base de datos
{
  erroresEcuacionesAlgebraicas = data.frame(
    Categoria = rep(c(
      rep("Aplicación de resultados", 4),
      rep("Enunciados de resultados", 6),
      rep("Errores conceptuales", 6),
      rep("Errores en demostraciones", 5),
      rep("Nomenclatura y redacción", 4)
    ), 4),
    
    Subtipo = rep(c(
      # Aplicación
      "Teorema de Galois", "Eisenstein", 
      "Morfismos no justificados", "Irreducibilidad modular",
      
      # Enunciados
      "Teorema de Moore",
      "Confusión teoremas Galois",
      "Hipótesis Gran Teorema Galois",
      "Hipótesis Teorema Fundamental Galois",
      "Omisión de hipótesis en otros resultados",
      "Conectores lógicos incorrectos",
      
      # Conceptuales
      "Irreducibilidad mal comprendida",
      "Extensiones mal definidas",
      "Automorfismos y grupos de Galois",
      "Polinomio mínimo y cuerpo de escisión",
      "Definiciones de estructuras algebraicas",
      "Cuerpo algebraicamente cerrado",
      
      # Demostraciones
      "Inclusión no doble",
      "Polinomio mínimo mal justificado",
      "Inducción mal aplicada",
      "Parámetros o símbolos ambiguos",
      "Afirmaciones sin justificar",
      
      # Nomenclatura y redacción
      "Símbolos incorrectos o ambiguos",
      "Lenguaje informal o incorrecto",
      "Falta de justificación o claridad",
      "Errores de notación en cuerpos, grupos, etc."
    ), times = 4),
    
    Convocatoria = rep(c("Enero 2023", "Febrero 2023", 
                         "Enero 2024", "Enero 2025"), each = 25),
    Presentados = rep(c(43, 34, 35, 53), each = 25),
    
    Frecuencia = c(
      # Ene 2023
      4, 4, 20, 1,
      2, 2, 4, 9, 5, 0,
      9, 7, 10, 0, 9, 2,
      0, 6, 5, 10, 19,
      4, 5, 6, 3,
      
      # Feb 2023
      4, 3, 12, 1,
      2, 3, 2, 1, 0, 0,
      5, 3, 18, 0, 2, 1,
      4, 5, 0, 5, 12,
      3, 4, 5, 4,
      
      # Ene 2024
      5, 4, 14, 1,
      5, 2, 2, 0, 0, 0,
      9, 2, 20, 0, 6, 1,
      2, 8, 1, 4, 5,
      3, 2, 6, 2,
      
      # Ene 2025
      22, 6, 0, 0,
      29, 0, 2, 29, 2, 2,
      3, 29, 3, 5, 26, 18,
      0, 0, 0, 0, 61,
      3, 4, 9, 2
    )
  )
  
  
  # Guardamos la base de datos
  save(erroresEcuacionesAlgebraicas, 
       file = "erroresEcuacionesAlgebraicas.RData")
}

# Paleta de colores
{
  mis_colores = c(
    "#0450b4", # Azul oscuro
    "#d63384", # Rosa fucsia
    "#04b486", # Verde azulado
    "#900C3F", # Granate
    "#fd7e14", # Naranja intenso
    "#fea802", # Naranja fuerte
    "#C70039", # Rojo brillante
    "#FFC300", # Amarillo
    "#6f42c1", # Púrpura
    "#20c997", # Verde menta
    "#1184a7", # Azul petróleo
    "#343a40", # Gris oscuro neutro
    "#f28cb1"  # Rosa
  )
}

# Capítulo 4
{
  # Sección 4.1. Presentación de datos de exámenes
  {
    # Ordenar niveles de convocatoria
    erroresEcuacionesAlgebraicas$Convocatoria = factor(
      erroresEcuacionesAlgebraicas$Convocatoria,
      levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
    )
    
    # Totales por categoría y convocatoria
    totales_categoria = aggregate(Frecuencia ~ Categoria + Convocatoria,
                                  data = erroresEcuacionesAlgebraicas,
                                  sum)
    
    # Totales por convocatoria (para globos)
    totales = aggregate(Frecuencia ~ Convocatoria, 
                        data = erroresEcuacionesAlgebraicas, sum)
    
    # Crear gráfico
    ggplot(totales_categoria, aes(x = Convocatoria, y = Frecuencia, 
                                  fill = Categoria)) +
      geom_bar(stat = "identity") +
      
      # Etiquetas internas
      geom_text(aes(label = Frecuencia),
                position = position_stack(vjust = 0.5),
                size = 10, color = "black", fontface = "bold") +
      
      # Globo con total por convocatoria
      geom_label(data = totales,
                 aes(x = Convocatoria, y = Frecuencia + 2, label = Frecuencia),
                 inherit.aes = FALSE,
                 size = 10, fill = "#ffadad", fontface = "bold", label.size = 0) +
      
      labs(title = "Distribución de errores por convocatoria",
           x = "Convocatoria", y = "Número de errores") +
      
      theme_minimal(base_size = 28) +
      theme(
        plot.title = element_text(size = 32, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 28, face = "italic"),
        axis.title.y = element_text(size = 28, face = "italic"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 24),
        legend.position = "bottom",
        legend.box = "horizontal"
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      scale_fill_manual(values = mis_colores)
  }
  
  # Sección 4.2. Tipología de errores detectados
  {
    # Aplicación de resultados
    {
      # Filtrar solo la categoría correspondiente
      errores_aplicacion = subset(erroresEcuacionesAlgebraicas, 
                                  Categoria == "Aplicación de resultados")
      
      # Ordenar convocatorias si aún no lo están
      errores_aplicacion$Convocatoria = factor(
        errores_aplicacion$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      )
      
      # Añadir altura para globos
      errores_aplicacion$y_pos = errores_aplicacion$Frecuencia + 0.7
      
      # Gráfico
      ggplot(errores_aplicacion, aes(x = Convocatoria, y = Frecuencia, 
                                     fill = Subtipo)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        
        # Globos por subtipo y convocatoria
        geom_label(aes(y = y_pos, label = Frecuencia),
                   position = position_dodge(width = 0.9),
                   size = 9, fontface = "bold", label.size = 0, color = "white",
                   show.legend = FALSE) +
        
        labs(title = "Errores en aplicación de resultados por convocatoria",
             x = "Convocatoria", y = "Número de errores", 
             fill = "Tipo de error") +
        
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
    }
    
    # Errores en enunciados
    {
      # Filtrar solo la categoría correspondiente
      errores_enunciado = subset(erroresEcuacionesAlgebraicas, 
                                 Categoria == "Enunciados de resultados")
      
      # Ordenar convocatorias si aún no lo están
      errores_enunciado$Convocatoria = factor(
        errores_enunciado$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      )
      
      # Posición de agrupamiento
      pos = position_dodge(width = 0.9)
      
      # Gráfico
      ggplot(errores_enunciado, aes(x = Convocatoria, y = Frecuencia, 
                                    fill = Subtipo)) +
        geom_bar(stat = "identity", position = pos) +
        
        # Globos con frecuencia
        geom_label(aes(label = Frecuencia),
                   position = pos,
                   vjust = -0.3, size = 8,
                   fontface = "bold", label.size = 0,
                   color = "white", show.legend = FALSE) +
        
        labs(title = "Errores en enunciados de resultados por convocatoria",
             x = "Convocatoria", y = "Número de errores", 
             fill = "Tipo de error") +
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE))
    }
    
    # Errores conceptuales
    {
      # Filtrar solo la categoría correspondiente
      errores_conceptuales = subset(erroresEcuacionesAlgebraicas, 
                                    Categoria == "Errores conceptuales")
      
      # Ordenar convocatorias si aún no lo están
      errores_conceptuales$Convocatoria = factor(
        errores_conceptuales$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      )
      
      # Añadir columna para globos
      errores_conceptuales$y_pos = errores_conceptuales$Frecuencia + 0.7
      
      # Posición para agrupamiento de barras
      pos = position_dodge(width = 0.9)
      
      # Gráfico
      ggplot(errores_conceptuales, aes(x = Convocatoria, y = Frecuencia,
                                       fill = Subtipo)) +
        geom_bar(stat = "identity", position = pos) +
        
        # Globos con frecuencia
        geom_label(aes(label = Frecuencia),
                   position = pos,
                   vjust = -0.3, size = 8,
                   fontface = "bold", label.size = 0,
                   color = "white", show.legend = FALSE) +
        
        labs(title = "Errores conceptuales por convocatoria",
             x = "Convocatoria", y = "Número de errores", fill = "Tipo de error") +
        
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
    }
    
    # Errores en demostraciones
    {
      # Filtrar solo la categoría correspondiente
      errores_demostracion = subset(erroresEcuacionesAlgebraicas, 
                                    Categoria == "Errores en demostraciones")
      
      # Ordenar convocatorias si aún no lo están
      errores_demostracion$Convocatoria = factor(
        errores_demostracion$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      )
      
      # Creamos posición agrupada
      pos = position_dodge(width = 0.9)
      
      # Añadimos posición vertical para globos
      errores_demostracion$y_pos = errores_demostracion$Frecuencia + 0.5
      
      # Gráfico
      ggplot(errores_demostracion, aes(x = Convocatoria, y = Frecuencia, 
                                       fill = Subtipo)) +
        geom_bar(stat = "identity", position = pos) +
        
        # Globos con frecuencia
        geom_label(aes(label = Frecuencia),
                   position = pos,
                   vjust = -0.3, size = 8,
                   fontface = "bold", label.size = 0,
                   color = "white", show.legend = FALSE) +
        
        labs(title = "Errores en demostraciones por convocatoria",
             x = "Convocatoria", y = "Número de errores", 
             fill = "Tipo de error") +
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
    }
    
    # Errores de redacción y nomenclatura
    {
      # Filtrar solo la categoría correspondiente
      errores_nomenclatura = subset(erroresEcuacionesAlgebraicas, 
                                    Categoria == "Nomenclatura y redacción")
      
      # Ordenar convocatorias si aún no lo están
      errores_nomenclatura$Convocatoria = factor(
        errores_nomenclatura$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      )
      
      # Ajustar altura para globos
      errores_nomenclatura$y_pos = errores_nomenclatura$Frecuencia + 0.7
      
      # Posición agrupada
      pos = position_dodge(width = 0.9)
      
      # Crear gráfico
      ggplot(errores_nomenclatura, aes(x = Convocatoria, y = Frecuencia, 
                                       fill = Subtipo)) +
        geom_bar(stat = "identity", position = pos) +
        
        # Globos con frecuencia
        geom_label(aes(label = Frecuencia),
                   position = pos,
                   vjust = -0.3, size = 8,
                   fontface = "bold", label.size = 0,
                   color = "white", show.legend = FALSE) +
        
        labs(title = "Errores de nomenclatura y redacción por convocatoria",
             x = "Convocatoria", y = "Número de errores", 
             fill = "Tipo de error") +
        
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
    }
    
  }
  
  # Sección 4.3. Exploración de patrones y evolución de errores
  {
    # Evolución temporal de los errores
    {
      # Sumar errores por convocatoria y categoría
      errores_temporales = aggregate(Frecuencia ~ Categoria + Convocatoria, 
                                     data = erroresEcuacionesAlgebraicas, sum)
      # Ordenar convocatorias si aún no lo están
      errores_temporales$Convocatoria = factor(
        errores_temporales$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      )
      
      # Gráfico lineal por categoría
      GrafEvolucionErrores = ggplot(errores_temporales, 
                                    aes(x = Convocatoria, 
                                        y = Frecuencia, 
                                        color = Categoria, 
                                        group = Categoria)) +
        geom_line(size = 3) +
        geom_point(size = 2) +
        labs(title = "Evolución temporal de errores por categoría",
             x = "Convocatoria",
             y = "Frecuencia total de errores") +
        theme_minimal() +
        geom_text(aes(label = Frecuencia),
                  size = 10, color = "black", fontface = "bold") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", 
                                     angle = 45, hjust = 1),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(color = guide_legend(nrow = 2, byrow = TRUE))
    }
    
    # Gráfico de sectores de errores por tipo
    {
      # Filtrar por convocatoria
      errores_ene2023 = erroresEcuacionesAlgebraicas %>%
        filter(Convocatoria == "Enero 2023") %>%
        group_by(Categoria) %>%
        summarise(Errores = sum(Frecuencia)) %>%
        ungroup()
      errores_feb2023 = erroresEcuacionesAlgebraicas %>%
        filter(Convocatoria == "Febrero 2023") %>%
        group_by(Categoria) %>%
        summarise(Errores = sum(Frecuencia)) %>%
        ungroup()
      errores_ene2024 = erroresEcuacionesAlgebraicas %>%
        filter(Convocatoria == "Enero 2024") %>%
        group_by(Categoria) %>%
        summarise(Errores = sum(Frecuencia)) %>%
        ungroup()
      errores_ene2025 = erroresEcuacionesAlgebraicas %>%
        filter(Convocatoria == "Enero 2025") %>%
        group_by(Categoria) %>%
        summarise(Errores = sum(Frecuencia)) %>%
        ungroup()
      
      # Calcular proporciones
      errores_ene2023 = errores_ene2023 %>%
        arrange(desc(Categoria)) %>%
        mutate(Prop = Errores / sum(Errores) * 100) %>%
        mutate(ypos = cumsum(Prop) - 0.5 * Prop)
      errores_feb2023 = errores_feb2023 %>%
        arrange(desc(Categoria)) %>%
        mutate(Prop = Errores / sum(Errores) * 100) %>%
        mutate(ypos = cumsum(Prop) - 0.5 * Prop)
      errores_ene2024 = errores_ene2024 %>%
        arrange(desc(Categoria)) %>%
        mutate(Prop = Errores / sum(Errores) * 100) %>%
        mutate(ypos = cumsum(Prop) - 0.5 * Prop)
      errores_ene2025 = errores_ene2025 %>%
        arrange(desc(Categoria)) %>%
        mutate(Prop = Errores / sum(Errores) * 100) %>%
        mutate(ypos = cumsum(Prop) - 0.5 * Prop)
      
      # Etiquetas para la gráfica
      errores_ene2023$label = paste(round(errores_ene2023$Prop, 1), "%")
      errores_feb2023$label = paste(round(errores_feb2023$Prop, 1), "%")
      errores_ene2024$label = paste(round(errores_ene2024$Prop, 1), "%")
      errores_ene2025$label = paste(round(errores_ene2025$Prop, 1), "%")
      
      # Gráfico de sectores
      GrafErroresPorCategoriaEne2023 = ggplot(errores_ene2023, aes(x = "", 
                                                                   y = Prop, 
                                                                   fill = Categoria)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(plot.title = element_text(size = 32, hjust = .5, face = "bold"),
              legend.title = element_text(size = 26, face = "bold"),
              legend.text = element_text(size = 24),
              legend.position = "bottom",
              legend.box = "horizontal") +
        geom_label(aes(y = ypos, label = label), fill = "#ffadad", size = 10, 
                   color = "black", fontface = "bold", hjust = 0.5, angle = 45,
                   label.size = 0) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        labs(title = "Porcentaje de errores por categoría\nEnero 2023", 
             face = "italic")
      
      GrafErroresPorCategoriaFeb2023 = ggplot(errores_feb2023, aes(x = "", 
                                                                   y = Prop, 
                                                                   fill = Categoria)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(plot.title = element_text(size = 32, hjust = .5, face = "bold"),
              legend.title = element_text(size = 26, face = "bold"),
              legend.text = element_text(size = 24),
              legend.position = "bottom",
              legend.box = "horizontal") +
        geom_label(aes(y = ypos, label = label), fill = "#ffadad", size = 10, 
                   color = "black", fontface = "bold", hjust = 0.5, angle = 45,
                   label.size = 0) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        labs(title = "Porcentaje de errores por categoría\nFebrero 2023", 
             face = "italic")
      
      GrafErroresPorCategoriaEne2024 = ggplot(errores_ene2024, aes(x = "", 
                                                                   y = Prop, 
                                                                   fill = Categoria)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(plot.title = element_text(size = 32, hjust = .5, face = "bold"),
              legend.title = element_text(size = 26, face = "bold"),
              legend.text = element_text(size = 24),
              legend.position = "bottom",
              legend.box = "horizontal") +
        geom_label(aes(y = ypos, label = label), fill = "#ffadad", size = 10, 
                   color = "black", fontface = "bold", hjust = 0.5, angle = 45,
                   label.size = 0) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        labs(title = "Porcentaje de errores por categoría\nEnero 2024", 
             face = "italic")
      
      GrafErroresPorCategoriaEne2025 = ggplot(errores_ene2025, aes(x = "", 
                                                                   y = Prop, 
                                                                   fill = Categoria)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(plot.title = element_text(size = 32, hjust = .5, face = "bold"),
              legend.title = element_text(size = 26, face = "bold"),
              legend.text = element_text(size = 24),
              legend.position = "bottom",
              legend.box = "horizontal") +
        geom_label(aes(y = ypos, label = label), fill = "#ffadad", size = 10, 
                   color = "black", fontface = "bold", hjust = 0.5, angle = 45,
                   label.size = 0) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        labs(title = "Porcentaje de errores por categoría\nEnero 2025", 
             face = "italic")
      
      # Tabla de gráficas con cowplot
      plot_grid(GrafErroresPorCategoriaEne2023, 
                GrafErroresPorCategoriaFeb2023, 
                GrafErroresPorCategoriaEne2024, 
                GrafErroresPorCategoriaEne2025, nrow = 2, ncol = 2)
      
    }
    
    # Evolución temporal por subtipos
    {
      # Filtrar solo la categoría correspondiente
      errores_temporalesSubtipo1 = subset(erroresEcuacionesAlgebraicas, 
                                          Categoria == 
                                            "Aplicación de resultados")
      errores_temporalesSubtipo2 = subset(erroresEcuacionesAlgebraicas, 
                                          Categoria == 
                                            "Enunciados de resultados")
      errores_temporalesSubtipo3 = subset(erroresEcuacionesAlgebraicas, 
                                          Categoria == 
                                            "Errores conceptuales")
      errores_temporalesSubtipo4 = subset(erroresEcuacionesAlgebraicas, 
                                          Categoria == 
                                            "Errores en demostraciones")
      errores_temporalesSubtipo5 = subset(erroresEcuacionesAlgebraicas, 
                                          Categoria == 
                                            "Nomenclatura y redacción")
      
      # Ordenar convocatorias si aún no lo están
      errores_temporalesSubtipo1$Convocatoria = factor(
        errores_temporalesSubtipo1$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025"))
      errores_temporalesSubtipo2$Convocatoria = factor(
        errores_temporalesSubtipo2$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025"))
      errores_temporalesSubtipo3$Convocatoria = factor(
        errores_temporalesSubtipo3$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025"))
      errores_temporalesSubtipo4$Convocatoria = factor(
        errores_temporalesSubtipo4$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025"))
      errores_temporalesSubtipo5$Convocatoria = factor(
        errores_temporalesSubtipo5$Convocatoria,
        levels = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025"))
      
      # Gráfico lineal por categoría
      GrafEvolucionErroresSubtipo1 = ggplot(errores_temporalesSubtipo1, 
                                            aes(x = Convocatoria, 
                                                y = Frecuencia, 
                                                color = Subtipo, 
                                                group = Subtipo)) +
        geom_line(size = 3) +
        geom_point(size = 2) +
        labs(title = "Evolución temporal de errores por subtipo
             Aplicación de resultados",
             x = "Convocatoria",
             y = "Frecuencia total de errores") +
        theme_minimal() +
        geom_text(aes(label = Frecuencia),
                  size = 10, color = "black", fontface = "bold") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", 
                                     angle = 45, hjust = 1),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      GrafEvolucionErroresSubtipo2 = ggplot(errores_temporalesSubtipo2, 
                                            aes(x = Convocatoria, 
                                                y = Frecuencia, 
                                                color = Subtipo, 
                                                group = Subtipo)) +
        geom_line(size = 3) +
        geom_point(size = 2) +
        labs(title = "Evolución temporal de errores por subtipo
             Enunciados de resultados",
             x = "Convocatoria",
             y = "Frecuencia total de errores") +
        theme_minimal() +
        geom_text(aes(label = Frecuencia),
                  size = 10, color = "black", fontface = "bold") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", 
                                     angle = 45, hjust = 1),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      GrafEvolucionErroresSubtipo3 = ggplot(errores_temporalesSubtipo3, 
                                            aes(x = Convocatoria, 
                                                y = Frecuencia, 
                                                color = Subtipo, 
                                                group = Subtipo)) +
        geom_line(size = 3) +
        geom_point(size = 2) +
        labs(title = "Evolución temporal de errores por subtipo
             Errores conceptuales",
             x = "Convocatoria",
             y = "Frecuencia total de errores") +
        theme_minimal() +
        geom_text(aes(label = Frecuencia),
                  size = 10, color = "black", fontface = "bold") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", 
                                     angle = 45, hjust = 1),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      GrafEvolucionErroresSubtipo4 = ggplot(errores_temporalesSubtipo4, 
                                            aes(x = Convocatoria, 
                                                y = Frecuencia, 
                                                color = Subtipo, 
                                                group = Subtipo)) +
        geom_line(size = 3) +
        geom_point(size = 2) +
        labs(title = "Evolución temporal de errores por subtipo
             Errores en demostraciones",
             x = "Convocatoria",
             y = "Frecuencia total de errores") +
        theme_minimal() +
        geom_text(aes(label = Frecuencia),
                  size = 10, color = "black", fontface = "bold") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", 
                                     angle = 45, hjust = 1),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      GrafEvolucionErroresSubtipo5 = ggplot(errores_temporalesSubtipo5, 
                                            aes(x = Convocatoria, 
                                                y = Frecuencia, 
                                                color = Subtipo, 
                                                group = Subtipo)) +
        geom_line(size = 3) +
        geom_point(size = 2) +
        labs(title = "Evolución temporal de errores por subtipo
             Nomenclatura y redacción",
             x = "Convocatoria",
             y = "Frecuencia total de errores") +
        theme_minimal() +
        geom_text(aes(label = Frecuencia),
                  size = 10, color = "black", fontface = "bold") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", 
                                     angle = 45, hjust = 1),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      # Tabla de gráficas con cowplot
      plot_grid(GrafEvolucionErroresSubtipo1, 
                GrafEvolucionErroresSubtipo2, ncol = 2)
      plot_grid(GrafEvolucionErroresSubtipo3, 
                GrafEvolucionErroresSubtipo4, ncol = 2)
      plot_grid(GrafEvolucionErroresSubtipo5, ncol = 2)
    }
    
    # Gráfico de sectores por subtipos
    {
      # Función para filtrar y agrupar errores por subtipo, 
      # dada una convocatoria y una categoría
      errores_por_subtipo = function(convocatoria, categoria) {
        erroresEcuacionesAlgebraicas %>%
          filter(Convocatoria == convocatoria,
                 Categoria == categoria) %>%
          group_by(Subtipo) %>%
          summarise(Errores = sum(Frecuencia)) %>%
          ungroup()
      }
      
      # Crear todos los objetos errores_SubtipoX_Convocatoria
      
      # Lista de convocatorias y categorías
      convocatorias = c("Enero 2023", "Febrero 2023", "Enero 2024", "Enero 2025")
      categorias = c("Aplicación de resultados", 
                     "Enunciados de resultados", 
                     "Errores conceptuales", 
                     "Errores en demostraciones", 
                     "Nomenclatura y redacción")
      
      # Asignar objetos dinámicamente para cada combinación
      for (conv in convocatorias) {
        for (cat in categorias) {
          nombre_objeto = paste0(
            "errores_",
            gsub(" ", "", cat), "_",
            gsub(" ", "", conv)
          )
          
          assign(nombre_objeto, errores_por_subtipo(conv, cat))
        }
      }
      
      # Función para calcular proporciones
      # dada una convocatoria y una categoría
      
      preparar_piechart_datos = function(df) {
        df = df %>%
          arrange(desc(Subtipo)) %>%
          mutate(
            Prop = Errores / sum(Errores) * 100,
            ypos = cumsum(Prop) - 0.5 * Prop,
            label = paste0(round(Prop, 1), "%")
          )
        return(df)
      }
      
      errores_Aplicacion_Enero2023_prop = preparar_piechart_datos(
        errores_Aplicaciónderesultados_Enero2023)
      errores_Aplicacion_Febrero2023_prop = preparar_piechart_datos(
        errores_Aplicaciónderesultados_Febrero2023)
      errores_Aplicacion_Enero2024_prop = preparar_piechart_datos(
        errores_Aplicaciónderesultados_Enero2024)
      errores_Aplicacion_Enero2025_prop = preparar_piechart_datos(
        errores_Aplicaciónderesultados_Enero2025)
      
      errores_Enunciados_Enero2023_prop = preparar_piechart_datos(
        errores_Enunciadosderesultados_Enero2023)
      errores_Enunciados_Febrero2023_prop = preparar_piechart_datos(
        errores_Enunciadosderesultados_Febrero2023)
      errores_Enunciados_Enero2024_prop = preparar_piechart_datos(
        errores_Enunciadosderesultados_Enero2024)
      errores_Enunciados_Enero2025_prop = preparar_piechart_datos(
        errores_Enunciadosderesultados_Enero2025)
      
      errores_Conceptuales_Enero2023_prop = preparar_piechart_datos(
        errores_Erroresconceptuales_Enero2023)
      errores_Conceptuales_Febrero2023_prop = preparar_piechart_datos(
        errores_Erroresconceptuales_Febrero2023)
      errores_Conceptuales_Enero2024_prop = preparar_piechart_datos(
        errores_Erroresconceptuales_Enero2024)
      errores_Conceptuales_Enero2025_prop = preparar_piechart_datos(
        errores_Erroresconceptuales_Enero2025)
      
      errores_Demostraciones_Enero2023_prop = preparar_piechart_datos(
        errores_Erroresendemostraciones_Enero2023)
      errores_Demostraciones_Febrero2023_prop = preparar_piechart_datos(
        errores_Erroresendemostraciones_Febrero2023)
      errores_Demostraciones_Enero2024_prop = preparar_piechart_datos(
        errores_Erroresendemostraciones_Enero2024)
      errores_Demostraciones_Enero2025_prop = preparar_piechart_datos(
        errores_Erroresendemostraciones_Enero2025)
      
      errores_Nomenclatura_Enero2023_prop = preparar_piechart_datos(
        errores_Nomenclaturayredacción_Enero2023)
      errores_Nomenclatura_Febrero2023_prop = preparar_piechart_datos(
        errores_Nomenclaturayredacción_Febrero2023)
      errores_Nomenclatura_Enero2024_prop = preparar_piechart_datos(
        errores_Nomenclaturayredacción_Enero2024)
      errores_Nomenclatura_Enero2025_prop = preparar_piechart_datos(
        errores_Nomenclaturayredacción_Enero2025)
      
      generar_piechart_errores = function(df_prop, titulo = "") {
        ggplot(df_prop, aes(x = "", y = Prop, fill = Subtipo)) +
          geom_bar(stat = "identity", width = 1, color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          geom_label(
            aes(y = ypos, label = label),
            fill = "#ffadad",
            size = 10,
            color = "black",
            fontface = "bold",
            hjust = 0.5,
            label.size = 0,
            angle = -45
          ) +
          labs(title = titulo) +
          theme(
            plot.title = element_text(size = 28, hjust = .5, face = "bold"),
            legend.title = element_text(size = 24, face = "bold"),
            legend.text = element_text(size = 22),
            legend.position = "bottom",
            legend.box = "horizontal"
          ) +
          guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
          scale_fill_manual(values = mis_colores)
      }
      
      categorias = c(
        "Aplicacion", 
        "Enunciados", 
        "Conceptuales", 
        "Demostraciones", 
        "Nomenclatura"
      )
      
      convocatorias = c("Enero2023", "Febrero2023", "Enero2024", "Enero2025")
      
      # Lista vacía para guardar los gráficos
      graficos_piecharts = list()
      
      # Bucle para generar y guardar gráficos
      for (cat in categorias) {
        for (conv in convocatorias) {
          
          nombre_objeto = paste0("errores_", cat, "_", conv, "_prop")
          
          # Obtener el objeto con get
          df_prop = get(nombre_objeto)
          
          # Crear título legible para gráfico
          categoria_titulo = gsub("([a-z])([A-Z])", "\\1 \\2", cat)
          titulo = paste0("Errores de ", categoria_titulo, "\n", 
                          gsub("([A-Za-z]+)([0-9]+)", "\\1 \\2", conv))
          
          # Generar gráfico y guardar
          graficos_piecharts[[paste0(cat, "_", 
                                     conv)]] = generar_piechart_errores(df_prop,
                                                                        titulo)
        }
        
      }
      
      # Tabla de gráficas con cowplot
      plot_grid(graficos_piecharts$Aplicacion_Enero2023,
                graficos_piecharts$Aplicacion_Febrero2023,
                graficos_piecharts$Aplicacion_Enero2024,
                graficos_piecharts$Aplicacion_Enero2025, ncol = 2)
      
      plot_grid(graficos_piecharts$Enunciados_Enero2023,
                graficos_piecharts$Enunciados_Febrero2023,
                graficos_piecharts$Enunciados_Enero2024,
                graficos_piecharts$Enunciados_Enero2025, ncol = 2)
      
      plot_grid(graficos_piecharts$Conceptuales_Enero2023,
                graficos_piecharts$Conceptuales_Febrero2023,
                graficos_piecharts$Conceptuales_Enero2024,
                graficos_piecharts$Conceptuales_Enero2025, ncol = 2)
      
      plot_grid(graficos_piecharts$Demostraciones_Enero2023,
                graficos_piecharts$Demostraciones_Febrero2023,
                graficos_piecharts$Demostraciones_Enero2024,
                graficos_piecharts$Demostraciones_Enero2025, ncol = 2)
      
      plot_grid(graficos_piecharts$Nomenclatura_Enero2023,
                graficos_piecharts$Nomenclatura_Febrero2023,
                graficos_piecharts$Nomenclatura_Enero2024,
                graficos_piecharts$Nomenclatura_Enero2025, ncol = 2)
      
      
    }
    
    # Estudio de correlación
    {
      # Crear tabla resumen en formato ancho
      tablaErroresConvocatorias = erroresEcuacionesAlgebraicas %>%
        group_by(Categoria, Convocatoria) %>%
        summarise(Frecuencia = sum(Frecuencia), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Categoria, values_from = Frecuencia)
      
      # Calcular matriz de correlación solo con columnas numéricas
      correlation_matrix = cor(
        tablaErroresConvocatorias[, sapply(tablaErroresConvocatorias, 
                                           is.numeric)])
      
      # Reorganizar para gráfica
      melted_cormat = melt(correlation_matrix)
      
      # Visualizar
      ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        labs(title = "Matriz de correlación entre categorías de errores", x = "", y = "") +
        scale_fill_gradientn(colors = brewer.pal(9, "RdPu")) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text.x = element_text(size = 24, face = "bold", angle = 90, 
                                     hjust = 1),
          axis.text.y = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(5, 'cm'),
          legend.text = element_text(size = 24),
          legend.position = "bottom"
        )
    }
    
    # Modelo de regresión lineal
    {
      # Creamos la tabla de errores por convocatoria (resumen total 
      # por categoría)
      tablaErrores = erroresEcuacionesAlgebraicas %>%
        group_by(Convocatoria, Categoria) %>%
        summarise(Frecuencia = sum(Frecuencia), .groups = "drop") %>%
        pivot_wider(names_from = Categoria, values_from = Frecuencia)
      
      # Modelo de regresión lineal
      modelo_conceptuales = lm(`Errores conceptuales` ~ 
                                 `Aplicación de resultados` + 
                                 `Errores en demostraciones`,
                               data = tablaErrores)
      
      # Ver resumen del modelo
      summary(modelo_conceptuales)
      
    }
  }
}

# Capítulo 5
{
  # Importación de los resultados de la encuesta
  {
    resultadosEncuesta = read.csv("EncuestaEcuacionesAlgebraicas.csv",
                                  header = T,
                                  sep = ",",
                                  dec = ".")
    # Limpiamos el csv y modificamos convenientemente las respuestas para
    # poder trabajar con ellas.
    
    # Eliminar la columna de marca temporal
    resultadosEncuesta$Marca.temporal = NULL
    
    # Eliminar la columna de sugerencias
    resultadosEncuesta$Sugerencias = NULL
    
    # Renombrar columnas con nombres simplificados
    colnames(resultadosEncuesta) = c(
      "Consentimiento",
      "DificultadExamen",
      "Preparacion",
      "TiempoExamen",
      "TemasDificiles",
      "DificultadEnunciados",
      "ErroresFrecuentes",
      "MetodosEstudio",
      "RecursosAdicionales",
      "ConocimientoAlgebraBasica",
      "ConocimientoAlgebraLineal",
      "ConocimientoMatDiscreta",
      "ImpactoConocimientosPrevios",
      "ConceptosPreviosDificiles",
      "Sugerencias"
    )
    
    # Eliminamos también la columna de consentimiento
    resultadosEncuesta$Consentimiento = NULL
    
    # Transformamos respuestas cualitativas en cuantitativas
    resultadosEncuesta$DificultadExamen = factor(
      resultadosEncuesta$DificultadExamen,
      levels = c("1", "2", "3", "4", "5"),
      ordered = TRUE)
    
    resultadosEncuesta$Preparacion = factor(resultadosEncuesta$Preparacion,
                                            levels = c("No", "Parcialmente",
                                                       "Sí"),
                                            ordered = TRUE)
    
    resultadosEncuesta$TiempoExamen = factor(
      resultadosEncuesta$TiempoExamen,
      levels = c("No", "En algunas ocasiones sí, en otras no", "Sí"),
      ordered = TRUE)
    
    resultadosEncuesta$DificultadEnunciados = factor(
      resultadosEncuesta$DificultadEnunciados,
      levels = c("1", "2", "3", "4", "5"),
      ordered = TRUE)
    
    # Lista de temas predefinidos
    temas = c(
      "Teoría de anillos: polinomios",
      "Repaso de Teoría de Grupos",
      "Grupos resolubles (si no se ha visto en Matemática Discreta)",
      "Automorfismos de cuerpos",
      "Teorema fundamental de la teoría de Galois",
      "Grupos de Galois de polinomios",
      "Teorema del elemento primitivo",
      "Extensiones radicales",
      "Extensiones de cuerpos separables",
      "Extensiones de cuerpos cíclicas",
      "Cuerpos finitos",
      "Cuerpos de escisión. Extensiones normales",
      "Complementos y aplicaciones"
    )
    
    # Crear columnas binarias por tema
    for (tema in temas) {
      resultadosEncuesta[[tema]] = grepl(tema, 
                                         resultadosEncuesta$TemasDificiles,
                                         fixed = TRUE)
    }
    
    resultadosEncuesta[temas] = lapply(resultadosEncuesta[temas], 
                                       as.integer)
    
    # Diccionario: nombre original -> nombre más representativo
    nombres_mas_claros = c(
      "Teoría de anillos: polinomios" = "Dificil_Polinomios",
      "Repaso de Teoría de Grupos" = "Dificil_Grupos",
      "Grupos resolubles (si no se ha visto en Matemática Discreta)" =
        "Dificil_GruposResolubles",
      "Automorfismos de cuerpos" = "Dificil_Automorfismos",
      "Teorema fundamental de la teoría de Galois" = "Dificil_TFGalois",
      "Grupos de Galois de polinomios" = "Dificil_GruposGalois",
      "Teorema del elemento primitivo" = "Dificil_ElementoPrimitivo",
      "Extensiones radicales" = "Dificil_ExtRadicales",
      "Extensiones de cuerpos separables" = "Dificil_ExtSeparables",
      "Extensiones de cuerpos cíclicas" = "Dificil_ExtCiclicas",
      "Cuerpos finitos" = "Dificil_CuerposFinitos",
      "Cuerpos de escisión. Extensiones normales" = "Dificil_EscisionNormales",
      "Complementos y aplicaciones" = "Dificil_Complementos"
    )
    
    # Renombrar columnas
    names(resultadosEncuesta)[names(resultadosEncuesta)
                              %in% names(nombres_mas_claros)] =
      nombres_mas_claros[names(resultadosEncuesta)[names(resultadosEncuesta) 
                                                   %in% names(
                                                     nombres_mas_claros)]]
    
    # Eliminamos la variable TemasDificiles porque ya la hemos traducido
    resultadosEncuesta$TemasDificiles = NULL
    
    # Repetimos proceso para ErroresFrecuentes
    errores = c(
      "Errores de procedimiento (por ejemplo, omitir pasos intermedios, usar el método incorrecto)",
      "Errores de concepto (por ejemplo, confusión entre propiedades, incorrecta aplicación de un teorema, etc.)",
      "Errores de interpretación del enunciado",
      "Errores de cálculo",
      "Errores por no tener tiempo suficiente (en el parcial)",
      "No estudiar lo suficiente"
    )
    
    # Crear columnas binarias para cada tipo de error
    for (error in errores) {
      nombre_columna = paste0("Error_", make.names(error))
      resultadosEncuesta[[nombre_columna]] = grepl(
        error, 
        resultadosEncuesta$ErroresFrecuentes, fixed = TRUE)
    }
    
    # Convertir a 0 y 1
    cols_error = grep("^Error_", names(resultadosEncuesta), value = TRUE)
    resultadosEncuesta[cols_error] = lapply(resultadosEncuesta[cols_error], 
                                            as.integer)
    
    # Diccionario de nombres nuevos
    nombres_nuevos = c(
      "Error_Errores.de.procedimiento..por.ejemplo..omitir.pasos.intermedios..usar.el.método.incorrecto." = "Error_Procedimiento",
      "Error_Errores.de.concepto..por.ejemplo..confusión.entre.propiedades..incorrecta.aplicación.de.un.teorema..etc.." = "Error_Concepto",
      "Error_Errores.de.interpretación.del.enunciado" = "Error_Enunciado",
      "Error_Errores.de.cálculo" = "Error_Calculo",
      "Error_Errores.por.no.tener.tiempo.suficiente..en.el.parcial." = "Error_Tiempo",
      "Error_No.estudiar.lo.suficiente" = "Error_EstudioInsuficiente"
    )
    
    # Renombrar
    names(resultadosEncuesta) = ifelse(
      names(resultadosEncuesta) %in% names(nombres_nuevos),
      nombres_nuevos[names(resultadosEncuesta)],
      names(resultadosEncuesta)
    )
    
    # Eliminamos ErroresFrecuentes porque ya la hemos traducido
    resultadosEncuesta$ErroresFrecuentes = NULL
    
    # Idem para métodos de estudio
    # Lista de métodos que aparecen en la encuesta
    metodos = c(
      "Clases magistrales",
      "Estudio individual",
      "Tutorías",
      "Ejercicios prácticos",
      "Vídeos educativos",
      "Libros y recursos en línea",
      "Trabajo cooperativo"
    )
    
    # Crear columnas binarias para cada método de estudio
    for (metodo in metodos) {
      nombre_columna = paste0("Metodo_", make.names(metodo))
      resultadosEncuesta[[nombre_columna]] = grepl(
        metodo, 
        resultadosEncuesta$MetodosEstudio, fixed = TRUE
      )
    }
    
    # Convertir a 0/1
    resultadosEncuesta[grep("^Metodo_", names(resultadosEncuesta))] = 
      lapply(resultadosEncuesta[grep("^Metodo_", names(resultadosEncuesta))], 
             as.integer)
    
    diccionario_metodos = c(
      "Metodo_Clases.magistrales"         = "Metodo_ClasesMagistrales",
      "Metodo_Estudio.individual"         = "Metodo_EstudioIndividual",
      "Metodo_Tutorías"                   = "Metodo_Tutorias",
      "Metodo_Ejercicios.prácticos"       = "Metodo_EjerciciosPracticos",
      "Metodo_Vídeos.educativos"          = "Metodo_VideosEducativos",
      "Metodo_Libros.y.recursos.en.línea" = "Metodo_LibrosRecursosLinea",
      "Metodo_Trabajo.cooperativo"        = "Metodo_TrabajoCooperativo"
    )
    
    # Renombrar columnas en el data frame
    names(resultadosEncuesta) = ifelse(
      names(resultadosEncuesta) %in% names(diccionario_metodos),
      diccionario_metodos[names(resultadosEncuesta)],
      names(resultadosEncuesta)
    )
    
    # Eliminamos MetodosEstudio porque ya la hemos traducido
    resultadosEncuesta$MetodosEstudio = NULL
    
    # Pasamos a RecursosAdicionales
    recursos = c(
      "Clases magistrales",
      "Estudio individual",
      "Tutorías",
      "Ejercicios prácticos",
      "Vídeos educativos",
      "Libros y recursos en línea",
      "Trabajo cooperativo",
      "chatGPT"
    )
    
    for (recurso in recursos) {
      nombre_columna = paste0("Recurso_", make.names(recurso))
      resultadosEncuesta[[nombre_columna]] = grepl(
        recurso,
        resultadosEncuesta$RecursosAdicionales,
        fixed = TRUE
      )
    }
    
    # Convertir a 0/1
    resultadosEncuesta[grep("^Recurso_", names(resultadosEncuesta))] = 
      lapply(resultadosEncuesta[grep("^Recurso_", names(resultadosEncuesta))], 
             as.integer)
    
    nombres_nuevos_recursos = c(
      "Recurso_Clases.magistrales"        = "Recurso_ClasesMagistrales",
      "Recurso_Estudio.individual"        = "Recurso_EstudioIndividual",
      "Recurso_Tutorías"                  = "Recurso_Tutorias",
      "Recurso_Ejercicios.prácticos"      = "Recurso_EjerciciosPracticos",
      "Recurso_Vídeos.educativos"         = "Recurso_VideosEducativos",
      "Recurso_Libros.y.recursos.en.línea"= "Recurso_LibrosRecursosLinea",
      "Recurso_Trabajo.cooperativo"       = "Recurso_TrabajoCooperativo",
      "Recurso_chatGPT"                   = "Recurso_ChatGPT"
    )
    
    names(resultadosEncuesta) = ifelse(
      names(resultadosEncuesta) %in% names(nombres_nuevos_recursos),
      nombres_nuevos_recursos[names(resultadosEncuesta)],
      names(resultadosEncuesta)
    )
    
    resultadosEncuesta$RecursosAdicionales = NULL
    
    # Ahora ordenamos las de conocimientos previos
    
    # Columnas de escala Likert (1-5)
    escalas_ordinales = c(
      "ConocimientoAlgebraBasica",
      "ConocimientoAlgebraLineal",
      "ConocimientoMatDiscreta",
      "ImpactoConocimientosPrevios"
    )
    
    # Aplicar transformación a factor ordenado
    resultadosEncuesta[escalas_ordinales] = lapply(
      resultadosEncuesta[escalas_ordinales],
      function(x) factor(x, levels = c("1", "2", "3", "4", "5"), 
                         ordered = TRUE)
    )
    
    # Finalizamos limpiando ConceptosPreviosDificiles
    
    # Lista de conceptos clave que aparecen con frecuencia
    conceptos = c(
      "Teoría de grupos",
      "Grupos resolubles",
      "Grupos cíclicos",
      "Clasificación de grupos",
      "Homomorfismos de grupos",
      "Anillos",
      "Teoría de anillos",
      "Series de composición",
      "Conceptos generales",
      "Matemática discreta",
      "Álgebra lineal",
      "Estructuras de grupos",
      "DFU",
      "Características de cuerpos"
    )
    
    # Crear columnas binarias
    for (concepto in conceptos) {
      nombre_columna = paste0("DificilPrevio_", make.names(concepto))
      resultadosEncuesta[[nombre_columna]] = grepl(
        concepto,
        resultadosEncuesta$ConceptosPreviosDificiles,
        ignore.case = TRUE
      )
    }
    
    # Convertir a 0/1
    resultadosEncuesta[grep("^DificilPrevio_", names(resultadosEncuesta))] = 
      lapply(resultadosEncuesta[grep("^DificilPrevio_", names(resultadosEncuesta))], 
             as.integer)
    
    # Unificar conceptos relacionados con GRUPOS
    resultadosEncuesta$DificilPrevio_Grupos = as.integer(
      rowSums(resultadosEncuesta[, c(
        "DificilPrevio_Teoría.de.grupos",
        "DificilPrevio_Grupos.resolubles",
        "DificilPrevio_Grupos.cíclicos",
        "DificilPrevio_Clasificación.de.grupos",
        "DificilPrevio_Homomorfismos.de.grupos",
        "DificilPrevio_Series.de.composición",
        "DificilPrevio_Estructuras.de.grupos"
      )], na.rm = TRUE) > 0
    )
    
    # Unificar conceptos relacionados con ANILLOS
    resultadosEncuesta$DificilPrevio_Anillos = as.integer(
      rowSums(resultadosEncuesta[, c(
        "DificilPrevio_Anillos",
        "DificilPrevio_Teoría.de.anillos",
        "DificilPrevio_DFU"
      )], na.rm = TRUE) > 0
    )
    
    # Eliminar columnas originales
    resultadosEncuesta = resultadosEncuesta[, !names(resultadosEncuesta) 
                                            %in% c(
                                              "DificilPrevio_Teoría.de.grupos",
                                              "DificilPrevio_Grupos.resolubles",
                                              "DificilPrevio_Grupos.cíclicos",
                                              "DificilPrevio_Clasificación.de.grupos",
                                              "DificilPrevio_Homomorfismos.de.grupos",
                                              "DificilPrevio_Series.de.composición",
                                              "DificilPrevio_Estructuras.de.grupos",
                                              "DificilPrevio_Anillos",
                                              "DificilPrevio_Teoría.de.anillos",
                                              "DificilPrevio_DFU"
                                            )]
    
    # Diccionario de nombres limpios para columnas restantes
    nombres_dificiles_limpios = c(
      "DificilPrevio_Conceptos.generales"        = 
        "DificilPrevio_ConceptosGenerales",
      "DificilPrevio_Matemática.discreta"        = 
        "DificilPrevio_MatematicaDiscreta",
      "DificilPrevio_Álgebra.lineal"             = 
        "DificilPrevio_AlgebraLineal",
      "DificilPrevio_Características.de.cuerpos" = 
        "DificilPrevio_CaracteristicasCuerpos"
    )
    
    # Aplicar el renombrado
    names(resultadosEncuesta) = ifelse(
      names(resultadosEncuesta) %in% names(nombres_dificiles_limpios),
      nombres_dificiles_limpios[names(resultadosEncuesta)],
      names(resultadosEncuesta)
    )
    
    resultadosEncuesta$ConceptosPreviosDificiles = NULL
    
    # Guardamos base de datos
    
    save(resultadosEncuesta, file = "resultadosEncuesta.RData")
    
  }
  
  # Análisis estadístico. Distribuciones generales
  {
    # Dificultad del examen
    {
      # Tabla de frecuencias
      df_dificultad = resultadosEncuesta %>%
        group_by(DificultadExamen) %>%
        summarise(Frecuencia = n()) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100,
               ypos = cumsum(Prop) - 0.5 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_dificultad, aes(x = "", y = Prop, fill = DificultadExamen)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 10, 
                   fontface = "bold",
                   angle = -70,
                   label.size = 0) +
        theme_void() +
        labs(title = "Nivel de dificultad de los exámenes
             según percepción del alumnado") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Dificultad de enunciados
    {
      # Tabla de frecuencias
      df_dificultadEnunciados = resultadosEncuesta %>%
        group_by(DificultadEnunciados) %>%
        summarise(Frecuencia = n()) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100) %>%
        arrange(desc(DificultadEnunciados)) %>%
        mutate(ypos = cumsum(Prop) - 0.5 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_dificultadEnunciados, aes(x = "", y = Prop, fill = DificultadEnunciados)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 10, 
                   angle = 45,
                   fontface = "bold",
                   label.size = 0) +
        theme_void() +
        labs(title = "Nivel de dificultad de los enunciados
             según percepción del alumnado") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Preparación del examen
    {
      # Tabla de frecuencias
      df_preparacion = resultadosEncuesta %>%
        group_by(Preparacion) %>%
        summarise(Frecuencia = n()) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100,
               ypos = cumsum(Prop) - 0.7 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_preparacion, aes(x = "", y = Prop, fill = Preparacion)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 8,
                   fontface = "bold",
                   label.size = 0) +
        theme_void() +
        labs(title = 
               "¿Te sentiste adecuadamente preparado para los exámenes?") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Tiempo de examen
    {
      # Tabla de frecuencias
      df_tiempo = resultadosEncuesta %>%
        group_by(TiempoExamen) %>%
        summarise(Frecuencia = n()) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100,
               ypos = cumsum(Prop) - 0.5 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_tiempo, aes(x = "", y = Prop, fill = TiempoExamen)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 10, 
                   fontface = "bold",
                   angle = -70,
                   label.size = 0) +
        theme_void() +
        labs(
          title = 
            "¿El tiempo proporcionado para resolver los exámenes fue suficiente?") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Dificultades de la asignatura
    {
      # Crear variables agrupadas binarizadas
      resultadosEncuesta$DificilAgrupado_PolinomiosAnillos = 
        resultadosEncuesta$Dificil_Polinomios
      
      resultadosEncuesta$DificilAgrupado_Grupos = as.integer(
        rowSums(resultadosEncuesta[, c(
          "Dificil_Grupos", "Dificil_GruposResolubles", "Dificil_GruposGalois"
        )], na.rm = TRUE) > 0
      )
      
      resultadosEncuesta$DificilAgrupado_AutomorfismosCuerpos = as.integer(
        rowSums(resultadosEncuesta[, c(
          "Dificil_Automorfismos", "Dificil_CuerposFinitos"
        )], na.rm = TRUE) > 0
      )
      
      resultadosEncuesta$DificilAgrupado_TFGalois = 
        resultadosEncuesta$Dificil_TFGalois
      
      resultadosEncuesta$DificilAgrupado_Extensiones = as.integer(
        rowSums(resultadosEncuesta[, c(
          "Dificil_ElementoPrimitivo", "Dificil_ExtRadicales",
          "Dificil_ExtSeparables", "Dificil_ExtCiclicas", 
          "Dificil_EscisionNormales"
        )], na.rm = TRUE) > 0
      )
      
      resultadosEncuesta$DificilAgrupado_Complementos = 
        resultadosEncuesta$Dificil_Complementos
      
      # Calcular proporciones de temas frecuentes
      df_temas_dificiles = 
        resultadosEncuesta[, c("DificilAgrupado_PolinomiosAnillos", 
                               "DificilAgrupado_Grupos", 
                               "DificilAgrupado_AutomorfismosCuerpos", 
                               "DificilAgrupado_TFGalois", 
                               "DificilAgrupado_Extensiones", 
                               "DificilAgrupado_Complementos")] %>%
        summarise_all(sum) %>%
        pivot_longer(cols = everything(), names_to = "Tema", 
                     values_to = "Frecuencia") %>%
        mutate(Prop = Frecuencia / 46 * 100,
               Etiqueta = paste0(Frecuencia, " (", round(Prop, 1), "%)")) %>%
        arrange(Frecuencia)
      
      nombres_dificiles_agrupados = c(
        "DificilAgrupado_PolinomiosAnillos"      = "Polinomios y anillos",
        "DificilAgrupado_Grupos"                 = "Teoría de grupos",
        "DificilAgrupado_AutomorfismosCuerpos"   = "Automorfismos y cuerpos",
        "DificilAgrupado_TFGalois"               = "Teorema Fund. de Galois",
        "DificilAgrupado_Extensiones"            = "Extensiones de cuerpos",
        "DificilAgrupado_Complementos"           = "Complementos y aplicaciones"
      )
      
      # Gráfico de barras horizontales
      ggplot(df_temas_dificiles, aes(x = reorder(Tema, Frecuencia), 
                                     y = Frecuencia, fill = Tema)) +
        geom_col(width = 0.7, color = "white") +
        coord_flip() +
        geom_text(aes(label = Etiqueta), hjust = -0.1, size = 6, fontface = "bold") +
        labs(title = "¿Qué tipo de errores cometiste con más frecuencia durante los exámenes?",
             subtitle = "(Puede seleccionar más de uno)", x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5, 
                                       margin = margin(b = 10)),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_blank(),
          legend.position = "none"
        ) +
        scale_x_discrete(labels = nombres_dificiles_agrupados) +
        scale_fill_manual(values = mis_colores) +
        expand_limits(y = max(df_temas_dificiles$Frecuencia) * 1.2)
    }
    
    # Errores cometidos con más frecuencia
    {
      # Calcular proporciones de errores frecuentes
      df_errores_frecuentes = 
        resultadosEncuesta[, c("Error_Procedimiento", 
                               "Error_Concepto", 
                               "Error_Enunciado", 
                               "Error_Calculo", 
                               "Error_Tiempo", 
                               "Error_EstudioInsuficiente")] %>%
        summarise_all(sum) %>%
        pivot_longer(cols = everything(), names_to = "Error", 
                     values_to = "Frecuencia") %>%
        mutate(Prop = Frecuencia / 46 * 100,
               Etiqueta = paste0(Frecuencia, " (", round(Prop, 1), "%)")) %>%
        arrange(Frecuencia)
      
      # Diccionario de nombres legibles
      nombres_errores_frecuentes = c(
        "Error_Procedimiento"        = "Errores de procedimiento",
        "Error_Concepto"             = "Errores de concepto",
        "Error_Enunciado"            = "Errores de interpretación del enunciado",
        "Error_Calculo"              = "Errores de cálculo",
        "Error_Tiempo"               = "Errores por no tener tiempo suficiente",
        "Error_EstudioInsuficiente" = "No estudiar lo suficiente"
      )
      
      # Gráfico de barras horizontales
      ggplot(df_errores_frecuentes, aes(x = reorder(Error, Frecuencia), y = Frecuencia, fill = Error)) +
        geom_col(width = 0.7, color = "white") +
        coord_flip() +
        geom_text(aes(label = Etiqueta), hjust = -0.1, size = 6, fontface = "bold") +
        labs(title = "¿Qué tipo de errores cometiste con más frecuencia durante los exámenes?",
             subtitle = "(Puede seleccionar más de uno)", x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5, 
                                       margin = margin(b = 10)),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_blank(),
          legend.position = "none"
        ) +
        scale_x_discrete(labels = nombres_errores_frecuentes) +
        scale_fill_manual(values = mis_colores) +
        expand_limits(y = max(df_errores_frecuentes$Frecuencia) * 1.2)
      
    }
    
    # Métodos más efectivos
    {
      # Calcular proporciones de métodos frecuentes
      df_metodos_estudio = 
        resultadosEncuesta[, c("Metodo_ClasesMagistrales",
                               "Metodo_EstudioIndividual",
                               "Metodo_Tutorias",
                               "Metodo_EjerciciosPracticos",
                               "Metodo_VideosEducativos",
                               "Metodo_LibrosRecursosLinea",
                               "Metodo_TrabajoCooperativo")] %>%
        summarise_all(sum) %>%
        pivot_longer(cols = everything(), names_to = "Metodo", 
                     values_to = "Frecuencia") %>%
        mutate(Prop = Frecuencia / 46 * 100,
               Etiqueta = paste0(Frecuencia, " (", round(Prop, 1), "%)")) %>%
        arrange(Frecuencia)
      
      # Diccionario de nombres legibles
      nombres_metodos_estudio = c(
        "Metodo_ClasesMagistrales"     = "Clases magistrales",
        "Metodo_EstudioIndividual"     = "Estudio individual",
        "Metodo_Tutorias"              = "Tutorías",
        "Metodo_EjerciciosPracticos"   = "Ejercicios prácticos",
        "Metodo_VideosEducativos"      = "Vídeos educativos",
        "Metodo_LibrosRecursosLinea"   = "Recursos en línea",
        "Metodo_TrabajoCooperativo"    = "Trabajo cooperativo"
      )
      
      # Gráfico de barras horizontales
      ggplot(df_metodos_estudio, aes(x = reorder(Metodo, Frecuencia), 
                                     y = Frecuencia, fill = Metodo)) +
        geom_col(width = 0.7, color = "white") +
        coord_flip() +
        geom_text(aes(label = Etiqueta), hjust = -0.1, size = 6, fontface = "bold") +
        labs(title = "¿Qué métodos de estudio consideras más efectivos para
             aprender los conceptos de la asignatura?",
             subtitle = "(Puedes seleccionar más de uno)", x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5, 
                                       margin = margin(b = 10)),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_blank(),
          legend.position = "none"
        ) +
        scale_x_discrete(labels = nombres_metodos_estudio) +
        scale_fill_manual(values = mis_colores) +
        expand_limits(y = max(df_metodos_estudio$Frecuencia) * 1.2)
    }
    
    # Recursos que habrían sido útiles
    {
      # Calcular proporciones de recursos frecuentes
      df_recursos_adicionales = 
        resultadosEncuesta[, c("Recurso_ClasesMagistrales",
                               "Recurso_EstudioIndividual",
                               "Recurso_Tutorias",
                               "Recurso_EjerciciosPracticos",
                               "Recurso_VideosEducativos",
                               "Recurso_LibrosRecursosLinea",
                               "Recurso_TrabajoCooperativo")] %>%
        summarise_all(sum) %>%
        pivot_longer(cols = everything(), names_to = "Recurso", 
                     values_to = "Frecuencia") %>%
        mutate(Prop = Frecuencia / 46 * 100,
               Etiqueta = paste0(Frecuencia, " (", round(Prop, 1), "%)")) %>%
        arrange(Frecuencia)
      
      # Diccionario de nombres legibles
      nombres_recursos_adicionales = c(
        "Recurso_ClasesMagistrales"     = "Clases magistrales",
        "Recurso_EstudioIndividual"     = "Estudio individual",
        "Recurso_Tutorias"              = "Tutorías",
        "Recurso_EjerciciosPracticos"   = "Ejercicios prácticos",
        "Recurso_VideosEducativos"      = "Vídeos educativos",
        "Recurso_LibrosRecursosLinea"   = "Recursos en línea",
        "Recurso_TrabajoCooperativo"    = "Trabajo cooperativo"
      )
      
      # Gráfico de barras horizontales
      ggplot(df_recursos_adicionales, aes(x = reorder(Recurso, Frecuencia), y = Frecuencia, fill = Recurso)) +
        geom_col(width = 0.7, color = "white") +
        coord_flip() +
        geom_text(aes(label = Etiqueta), hjust = -0.1, size = 6, fontface = "bold") +
        labs(title = "¿Qué recursos adicionales te habrían ayudado a mejorar tu rendimiento?",
             subtitle = "(Puedes seleccionar más de uno)", x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5, 
                                       margin = margin(b = 10)),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_blank(),
          legend.position = "none"
        ) +
        scale_x_discrete(labels = nombres_recursos_adicionales) +
        scale_fill_manual(values = mis_colores) +
        expand_limits(y = max(df_recursos_adicionales$Frecuencia) * 1.2)
    }
    
    # Conocimientos EBA previos
    {
      # Tabla de frecuencias
      df_EBA = resultadosEncuesta %>%
        group_by(ConocimientoAlgebraBasica) %>%
        summarise(Frecuencia = n()) %>%
        arrange(desc(ConocimientoAlgebraBasica)) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100,
               ypos = cumsum(Prop) - 0.5 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_EBA, aes(x = "", y = Prop,
                         fill = ConocimientoAlgebraBasica)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 10, 
                   fontface = "bold",
                   angle = -70,
                   label.size = 0) +
        theme_void() +
        labs(title = "Conocimientos de Estructuras Básicas del Álgebra
             según percepción del alumnado") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Conocimientos Álgebra lineal previos
    {
      # Tabla de frecuencias
      df_AL = resultadosEncuesta %>%
        group_by(ConocimientoAlgebraLineal) %>%
        summarise(Frecuencia = n()) %>%
        arrange(desc(ConocimientoAlgebraLineal)) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100,
               ypos = cumsum(Prop) - 0.5 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_AL, aes(x = "", y = Prop,
                        fill = ConocimientoAlgebraLineal)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 10, 
                   fontface = "bold",
                   angle = -70,
                   label.size = 0) +
        theme_void() +
        labs(title = "Conocimientos de Álgebra Lineal
             según percepción del alumnado") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Conocimientos Matemática Discreta previos
    {
      # Tabla de frecuencias
      df_MD = resultadosEncuesta %>%
        group_by(ConocimientoMatDiscreta) %>%
        summarise(Frecuencia = n()) %>%
        arrange(desc(ConocimientoMatDiscreta)) %>%
        mutate(Prop = Frecuencia / sum(Frecuencia) * 100,
               ypos = cumsum(Prop) - 0.5 * Prop,
               label = paste0(round(Prop, 1), "%"))
      
      # Gráfico de sectores
      ggplot(df_MD, aes(x = "", y = Prop,
                        fill = ConocimientoMatDiscreta)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        geom_label(aes(y = ypos, label = label),
                   fill = "#ffadad", color = "black", size = 10, 
                   fontface = "bold",
                   angle = 70,
                   label.size = 0) +
        theme_void() +
        labs(title = "Conocimientos de Matemática Discreta
             según percepción del alumnado") +
        theme(
          plot.title = element_text(size = 28, hjust = .5, face = "bold"),
          legend.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 28),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = mis_colores)
    }
    
    # Conocimientos previos difíciles
    {
      # Calcular proporciones de dificultades percibidas en conocimientos previos
      df_dificultades_previas = 
        resultadosEncuesta[, c("DificilPrevio_ConceptosGenerales",
                               "DificilPrevio_MatematicaDiscreta",
                               "DificilPrevio_AlgebraLineal",
                               "DificilPrevio_CaracteristicasCuerpos",
                               "DificilPrevio_Grupos")] %>%
        summarise_all(sum) %>%
        pivot_longer(cols = everything(), names_to = "Aspecto", 
                     values_to = "Frecuencia") %>%
        mutate(Prop = Frecuencia / 46 * 100,
               Etiqueta = paste0(Frecuencia, " (", round(Prop, 1), "%)")) %>%
        arrange(Frecuencia)
      
      # Diccionario de nombres legibles
      nombres_dificultades_previas = c(
        "DificilPrevio_ConceptosGenerales"       = "Conceptos generales de álgebra",
        "DificilPrevio_MatematicaDiscreta"       = "Matemática Discreta",
        "DificilPrevio_AlgebraLineal"            = "Álgebra Lineal",
        "DificilPrevio_CaracteristicasCuerpos"   = "Características de cuerpos",
        "DificilPrevio_Grupos"                   = "Teoría de grupos"
      )
      
      # Gráfico de barras horizontales
      ggplot(df_dificultades_previas, aes(x = reorder(Aspecto, Frecuencia), 
                                          y = Frecuencia, fill = Aspecto)) +
        geom_col(width = 0.7, color = "white") +
        coord_flip() +
        geom_text(aes(label = Etiqueta), hjust = -0.1, size = 6, fontface = "bold") +
        labs(title = "¿Qué conocimientos previos te resultaron más difíciles y afectaron tu comprensión?",
             subtitle = "(Puedes seleccionar más de uno)", x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5, 
                                       margin = margin(b = 10)),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_blank(),
          legend.position = "none"
        ) +
        scale_x_discrete(labels = nombres_dificultades_previas) +
        scale_fill_manual(values = mis_colores) +
        expand_limits(y = max(df_dificultades_previas$Frecuencia) * 1.2)
      
    }
    
  }
  
  # Análisis estadístico exploratorio
  {
    # Percepción de preparación frente a dificultad del examen
    {
      # Contar combinaciones de Preparacion y DificultadExamen
      df_perfil = resultadosEncuesta %>%
        count(Preparacion, DificultadExamen)
      
      # Visualización
      ggplot(df_perfil, aes(x = Preparacion, y = n, fill = DificultadExamen)) +
        geom_col(position = "dodge") +
        labs(title = "Percepción de preparación frente a dificultad del examen")
      
      ggplot(df_perfil, aes(x = Preparacion, y = n, 
                            fill = DificultadExamen)) +
        geom_col(position = "dodge") +
        labs(title = "Percepción de preparación frente a dificultad del examen",
             x = "Preparación", y = "Frecuencia", 
             fill = "Nivel de dificultad del examen") +
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
    }
    
    # Tiempo de examen y complejidad de examen
    {
      # Contar combinaciones de TiempoExamen y DificultadExamen
      df_tiempo_vs_dificultad = resultadosEncuesta %>%
        count(TiempoExamen, DificultadExamen)
      
      # Gráfico
      ggplot(df_tiempo_vs_dificultad, aes(x = TiempoExamen, y = n, 
                                          fill = DificultadExamen)) +
        geom_col(position = "dodge") +
        labs(title = "Percepción del tiempo frente a dificultad del examen",
             x = "¿Tiempo suficiente?", y = "Frecuencia", 
             fill = "Nivel de dificultad del examen") +
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
    }
    
    # Dificultad de enunciados y de examen
    {
      # Contar combinaciones entre dificultad de enunciados y dificultad del examen
      df_enunciado_vs_dificultad = resultadosEncuesta %>%
        count(DificultadEnunciados, DificultadExamen)
      
      # Gráfico con estilo personalizado
      ggplot(df_enunciado_vs_dificultad, 
             aes(x = as.factor(DificultadEnunciados), y = n, fill = DificultadExamen)) +
        geom_col(position = "dodge") +
        labs(title = "Percepción de dificultad de enunciados frente a dificultad del examen",
             x = "Dificultad de enunciados", y = "Frecuencia", 
             fill = "Nivel de dificultad del examen") +
        theme_minimal(base_size = 28) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.text = element_text(size = 24),
          legend.position = "bottom",
          legend.box = "horizontal"
        ) +
        scale_fill_manual(values = mis_colores) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
    }
    
    # Perfiles de pensamiento
    {
      # Crear columnas que indiquen el perfil de cada estudiante
      resultadosEncuesta = resultadosEncuesta %>%
        mutate(
          Perfil_Estructuralista = ifelse(
            Error_Concepto == 1 & 
              (Metodo_ClasesMagistrales == 1 | Metodo_LibrosRecursosLinea == 1), 
            1, 0
          ),
          Perfil_Procedimental = ifelse(
            (Error_Procedimiento == 1 | Error_Calculo == 1) &
              (Metodo_EjerciciosPracticos == 1 | 
                 Metodo_VideosEducativos == 1),
            1, 0
          ),
          Perfil_Mixto = ifelse(Perfil_Estructuralista == 1 & 
                                  Perfil_Procedimental == 1, 1, 0),
          Perfil_Ninguno = ifelse(Perfil_Estructuralista == 0 & 
                                    Perfil_Procedimental == 0, 1, 0)
        )
      
      # Resumen de frecuencias por perfil
      df_perfiles = resultadosEncuesta %>%
        summarise(
          Estructuralista = sum(Perfil_Estructuralista),
          Procedimental = sum(Perfil_Procedimental),
          Mixto = sum(Perfil_Mixto),
          Ninguno = sum(Perfil_Ninguno)
        ) %>%
        pivot_longer(cols = everything(), names_to = "Perfil", 
                     values_to = "Frecuencia")
      
      # Calcular porcentaje
      df_perfiles = df_perfiles %>%
        mutate(Porcentaje = paste0(round(Frecuencia / 46 * 100, 1), "%"))
      
      # Gráfico
      ggplot(df_perfiles, aes(x = Perfil, y = Frecuencia, fill = Perfil)) +
        geom_col(width = 0.6, color = "white") +
        geom_text(aes(label = Porcentaje), vjust = -0.5, size = 7, 
                  fontface = "bold") +
        theme_minimal(base_size = 24) +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.position = "none"
        ) +
        labs(title = "Perfiles de pensamiento en el alumnado",
             x = NULL, y = "Número de estudiantes") +
        scale_fill_manual(values = mis_colores)
    }
    
    # Errores cometidos vs. conocimientos previos
    {
      # Según álgebra basica
      ggplot(resultadosEncuesta, aes(x = as.factor(ConocimientoAlgebraBasica), 
                                     fill = as.factor(Error_Concepto))) +
        geom_bar(position = "fill") +
        labs(title = "Errores de concepto según nivel previo en Estructuras Básicas del Álgebra",
             x = "Nivel de conocimiento previo",
             y = "Proporción",
             fill = "¿Error de concepto?") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.position = "right"
        ) +
        scale_fill_manual(
          values = mis_colores,
          labels = c("0" = "No", "1" = "Sí"),
          name = "¿Error de concepto?"
        )
      
      # Según álgebra lineal
      ggplot(resultadosEncuesta, aes(x = as.factor(ConocimientoAlgebraLineal), 
                                     fill = as.factor(Error_Concepto))) +
        geom_bar(position = "fill") +
        labs(title = "Errores de concepto según nivel previo en Álgebra Lineal",
             x = "Nivel de conocimiento previo",
             y = "Proporción",
             fill = "¿Error de concepto?") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.position = "right"
        ) +
        scale_fill_manual(
          values = mis_colores,
          labels = c("0" = "No", "1" = "Sí"),
          name = "¿Error de concepto?"
        )
      
      # Según matemática discreta
      ggplot(resultadosEncuesta, aes(x = as.factor(ConocimientoMatDiscreta), 
                                     fill = as.factor(Error_Concepto))) +
        geom_bar(position = "fill") +
        labs(title = "Errores de concepto según nivel previo en Matemática Discreta",
             x = "Nivel de conocimiento previo",
             y = "Proporción",
             fill = "¿Error de concepto?") +
        theme(
          plot.title = element_text(size = 32, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 28, face = "italic"),
          axis.title.y = element_text(size = 28, face = "italic"),
          axis.text = element_text(size = 24, face = "bold"),
          legend.position = "right"
        ) +
        scale_fill_manual(
          values = mis_colores,
          labels = c("0" = "No", "1" = "Sí"),
          name = "¿Error de concepto?"
        )
      
      # Convertir todas las columnas a numéricas
      df_concepto_vs_conocimientos = resultadosEncuesta %>%
        select(Error_Concepto, 
               ConocimientoAlgebraLineal, 
               ConocimientoMatDiscreta, 
               ConocimientoAlgebraBasica) %>%
        mutate(across(everything(), as.numeric))
      
      # Calcular la matriz de correlación
      cor_matrix = cor(df_concepto_vs_conocimientos)
      
      
      # Convertir a formato largo para visualización
      melted_cormat = melt(cor_matrix)
      
      # Visualizar
      ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(value, 2)), size = 6, fontface = "bold") +
        scale_fill_gradientn(colors = brewer.pal(9, "RdPu"), limits = c(-1, 1)) +
        labs(title = "Correlación entre error de concepto y conocimientos previos",
             x = "", y = "", fill = "Correlación") +
        theme_minimal(base_size = 24) +
        theme(
          plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 20, face = "bold", angle = 45, hjust = 1),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.title = element_text(size = 26, face = "bold"),
          legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(5, 'cm'),
          legend.text = element_text(size = 24),
          legend.position = "bottom"
        )
      
      
      
    }
    
  }
}