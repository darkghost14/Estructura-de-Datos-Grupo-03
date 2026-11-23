// ============================================================================
//  SISTEMA DE GESTION DE PROCESOS - LABORATORIO CLINICO (Grupo 03)
//  Pseudocodigo para PSeInt
//
//  Simula:  lista enlazada (procesos), cola de prioridad y pila (memoria).
//  Datos ficticios: no se procesa informacion de pacientes.
//
//  COMO SE REPRESENTAN LAS ESTRUCTURAS
//  PSeInt no tiene punteros ni registros, por lo que cada estructura enlazada
//  se simula con arreglos: cada fila es un "nodo" y la columna "siguiente"
//  guarda el NUMERO DE FILA del nodo siguiente (0 equivale a NULO).
//
//   proc[n,1]=id (0 = fila libre)  proc[n,2]=prioridad  proc[n,3]=estado
//   proc[n,4]=memoria solicitada   proc[n,5]=siguiente
//   codigos[n], tareas[n]          = codigo de muestra y tipo de tarea
//   cola[n,1]=id del proceso (0 = fila libre) cola[n,2]=prioridad cola[n,3]=siguiente
//   pila[n,1]=id del proceso (0 = fila libre) pila[n,2]=memoria   pila[n,3]=siguiente
//   ctl[1]=primero  ctl[2]=ultimo  ctl[3]=siguienteId  ctl[4]=frente de la cola
//   ctl[5]=tope de la pila  ctl[6]=memoria usada  ctl[7]=filas disponibles (50)
//
//  Estados:     1=Listo  2=En ejecucion  3=Bloqueado  4=Finalizado
//  Prioridades: 1 (alta) a 3 (baja).   Memoria total: 1024 KB.
//
//  NOTA: se asume el perfil por defecto de PSeInt (arreglos y cadenas desde 1).
// ============================================================================


// ---------------------------------------------------------------------------
//  UTILIDADES DE LECTURA Y VALIDACION
// ---------------------------------------------------------------------------

// Verdadero si el texto esta vacio o solo tiene espacios.
Funcion vacio <- EstaVacio(txt)
    Definir vacio Como Logico
    Definir i Como Entero
    vacio <- Verdadero
    Para i <- 1 Hasta Longitud(txt) Con Paso 1 Hacer
        Si Subcadena(txt, i, i) <> " " Entonces
            vacio <- Falso
        FinSi
    FinPara
FinFuncion

// Verdadero si el texto es un numero entero (signo opcional, maximo 9 digitos).
Funcion ok <- EsEntero(txt)
    Definir ok Como Logico
    Definir n, i, inicio Como Entero
    Definir c Como Caracter
    n <- Longitud(txt)
    ok <- Verdadero
    inicio <- 1
    Si n = 0 Entonces
        ok <- Falso
    FinSi
    Si n > 9 Entonces
        ok <- Falso
    FinSi
    Si n > 0 Entonces
        Si Subcadena(txt, 1, 1) = "-" Entonces
            inicio <- 2
            Si n = 1 Entonces
                ok <- Falso
            FinSi
        FinSi
    FinSi
    Para i <- inicio Hasta n Con Paso 1 Hacer
        c <- Subcadena(txt, i, i)
        Si NO (c = "0" O c = "1" O c = "2" O c = "3" O c = "4" O c = "5" O c = "6" O c = "7" O c = "8" O c = "9") Entonces
            ok <- Falso
        FinSi
    FinPara
FinFuncion

// Lee un numero entero. Devuelve -1000000000 si el texto no es un entero valido.
Funcion valor <- LeerEntero(mensaje)
    Definir valor Como Entero
    Definir txt Como Caracter
    Escribir Sin Saltar mensaje
    Leer txt
    Si EsEntero(txt) Entonces
        valor <- ConvertirANumero(txt)
    SiNo
        valor <- -1000000000
    FinSi
FinFuncion

// Nombre legible de un estado.
Funcion nombre <- NombreEstado(e)
    Definir nombre Como Caracter
    Segun e Hacer
        1:
            nombre <- "Listo"
        2:
            nombre <- "En ejecucion"
        3:
            nombre <- "Bloqueado"
        4:
            nombre <- "Finalizado"
        De Otro Modo:
            nombre <- "Desconocido"
    FinSegun
FinFuncion

// Transiciones permitidas:
//   Listo -> En ejecucion; En ejecucion -> Bloqueado o Finalizado; Bloqueado -> Listo.
Funcion valida <- EsTransicionValida(actual, nuevo)
    Definir valida Como Logico
    valida <- Falso
    Si actual = 1 Y nuevo = 2 Entonces
        valida <- Verdadero
    FinSi
    Si actual = 2 Y nuevo = 3 Entonces
        valida <- Verdadero
    FinSi
    Si actual = 2 Y nuevo = 4 Entonces
        valida <- Verdadero
    FinSi
    Si actual = 3 Y nuevo = 1 Entonces
        valida <- Verdadero
    FinSi
FinFuncion


// ---------------------------------------------------------------------------
//  LISTA ENLAZADA DE PROCESOS  (GestorProcesos)
// ---------------------------------------------------------------------------

// Devuelve la fila del proceso con ese id, o 0 si no existe.
Funcion nodo <- BuscarProceso(proc, ctl, id)
    Definir nodo Como Entero
    Definir seguir Como Logico
    nodo <- ctl[1]
    seguir <- Verdadero
    Mientras seguir Hacer
        Si nodo = 0 Entonces
            seguir <- Falso
        SiNo
            Si proc[nodo, 1] = id Entonces
                seguir <- Falso
            SiNo
                nodo <- proc[nodo, 5]
            FinSi
        FinSi
    FinMientras
FinFuncion

// Algoritmo "AgregarProceso": valida, crea el nodo con
// estado Listo, lo agrega al final de la lista y lo encola.
// Devuelve el id asignado, o 0 si los datos no son validos.
Funcion nuevoId <- AgregarProceso(proc, codigos, tareas, cola, ctl, codigo, tarea, prio, mem)
    Definir nuevoId, nodo, i Como Entero
    nuevoId <- 0
    Si EstaVacio(tarea) Entonces
        Escribir "Error: El tipo de tarea no puede estar vacio."
    SiNo
        Si prio < 1 O prio > 3 Entonces
            Escribir "Error: La prioridad debe estar entre 1 y 3."
        SiNo
            Si mem <= 0 Entonces
                Escribir "Error: La memoria solicitada debe ser mayor que cero."
            SiNo
                // buscar una fila libre en la lista
                nodo <- 0
                Para i <- ctl[7] Hasta 1 Con Paso -1 Hacer
                    Si proc[i, 1] = 0 Entonces
                        nodo <- i
                    FinSi
                FinPara
                Si nodo = 0 Entonces
                    Escribir "Error: No hay espacio para mas procesos."
                SiNo
                    nuevoId <- ctl[3]
                    ctl[3] <- ctl[3] + 1
                    proc[nodo, 1] <- nuevoId
                    proc[nodo, 2] <- prio
                    proc[nodo, 3] <- 1
                    proc[nodo, 4] <- mem
                    proc[nodo, 5] <- 0
                    codigos[nodo] <- codigo
                    tareas[nodo] <- tarea
                    // insertar al final de la lista
                    Si ctl[1] = 0 Entonces
                        ctl[1] <- nodo
                    SiNo
                        proc[ctl[2], 5] <- nodo
                    FinSi
                    ctl[2] <- nodo
                    // incorporar a la cola de prioridad
                    Encolar(cola, ctl, nuevoId, prio)
                    Escribir "Proceso registrado con el identificador ", nuevoId, " (estado: Listo)."
                FinSi
            FinSi
        FinSi
    FinSi
FinFuncion

// Muestra todos los procesos.
SubProceso MostrarProcesos(proc, codigos, tareas, ctl)
    Definir nodo, total Como Entero
    Escribir "--- Lista de procesos registrados ---"
    nodo <- ctl[1]
    total <- 0
    Si nodo = 0 Entonces
        Escribir "No hay procesos registrados."
    SiNo
        Escribir "ID | Codigo | Prioridad | Estado | Memoria (KB) | Tarea"
        Mientras nodo <> 0 Hacer
            Escribir proc[nodo, 1], " | ", codigos[nodo], " | ", proc[nodo, 2], " | ", NombreEstado(proc[nodo, 3]), " | ", proc[nodo, 4], " | ", tareas[nodo]
            total <- total + 1
            nodo <- proc[nodo, 5]
        FinMientras
        Escribir "Total de procesos: ", total
    FinSi
    Escribir "Memoria usada: ", ctl[6], " de 1024 KB."
FinSubProceso

// Retira el nodo de la lista y libera su fila. Las condiciones
// (finalizado y sin memoria) se verifican antes, en OpcionEliminar.
SubProceso EliminarNodo(proc, ctl, id)
    Definir actual, anterior Como Entero
    Definir seguir Como Logico
    anterior <- 0
    actual <- ctl[1]
    seguir <- Verdadero
    Mientras seguir Hacer
        Si actual = 0 Entonces
            seguir <- Falso
        SiNo
            Si proc[actual, 1] = id Entonces
                seguir <- Falso
            SiNo
                anterior <- actual
                actual <- proc[actual, 5]
            FinSi
        FinSi
    FinMientras
    Si actual <> 0 Entonces
        Si anterior = 0 Entonces
            ctl[1] <- proc[actual, 5]
        SiNo
            proc[anterior, 5] <- proc[actual, 5]
        FinSi
        Si ctl[2] = actual Entonces
            ctl[2] <- anterior
        FinSi
        proc[actual, 1] <- 0
        proc[actual, 5] <- 0
    FinSi
FinSubProceso


// ---------------------------------------------------------------------------
//  COLA DE PRIORIDAD  (PlanificadorCPU)
//  Orden: menor prioridad numerica primero; si empatan, menor id primero.
// ---------------------------------------------------------------------------

// Algoritmo "Encolar": inserta en la posicion que le corresponde.
SubProceso Encolar(cola, ctl, id, prio)
    Definir nuevo, actual, sig, i Como Entero
    Definir seguir, precede Como Logico
    nuevo <- 0
    Para i <- ctl[7] Hasta 1 Con Paso -1 Hacer
        Si cola[i, 1] = 0 Entonces
            nuevo <- i
        FinSi
    FinPara
    Si nuevo = 0 Entonces
        Escribir "Error: la cola de prioridad esta llena."
    SiNo
        cola[nuevo, 1] <- id
        cola[nuevo, 2] <- prio
        cola[nuevo, 3] <- 0
        // el nuevo debe ir al frente?
        precede <- Falso
        Si ctl[4] = 0 Entonces
            precede <- Verdadero
        SiNo
            Si prio < cola[ctl[4], 2] Entonces
                precede <- Verdadero
            FinSi
            Si prio = cola[ctl[4], 2] Y id < cola[ctl[4], 1] Entonces
                precede <- Verdadero
            FinSi
        FinSi
        Si precede Entonces
            cola[nuevo, 3] <- ctl[4]
            ctl[4] <- nuevo
        SiNo
            // avanzar hasta el ultimo nodo que debe ir antes del nuevo
            actual <- ctl[4]
            seguir <- Verdadero
            Mientras seguir Hacer
                sig <- cola[actual, 3]
                Si sig = 0 Entonces
                    seguir <- Falso
                SiNo
                    precede <- Falso
                    Si prio < cola[sig, 2] Entonces
                        precede <- Verdadero
                    FinSi
                    Si prio = cola[sig, 2] Y id < cola[sig, 1] Entonces
                        precede <- Verdadero
                    FinSi
                    Si precede Entonces
                        seguir <- Falso
                    SiNo
                        actual <- sig
                    FinSi
                FinSi
            FinMientras
            cola[nuevo, 3] <- cola[actual, 3]
            cola[actual, 3] <- nuevo
        FinSi
    FinSi
FinSubProceso

// Retira el frente de la cola y devuelve el id del proceso (0 si esta vacia).
Funcion id <- Desencolar(cola, ctl)
    Definir id, nodo Como Entero
    id <- 0
    Si ctl[4] <> 0 Entonces
        nodo <- ctl[4]
        id <- cola[nodo, 1]
        ctl[4] <- cola[nodo, 3]
        cola[nodo, 1] <- 0
        cola[nodo, 3] <- 0
    FinSi
FinFuncion

// Algoritmo "ExtraerDeCola": retira de la cola el nodo de un proceso concreto.
SubProceso ExtraerDeCola(cola, ctl, id)
    Definir actual, sig Como Entero
    Definir seguir Como Logico
    Si ctl[4] <> 0 Entonces
        Si cola[ctl[4], 1] = id Entonces
            sig <- ctl[4]
            ctl[4] <- cola[sig, 3]
            cola[sig, 1] <- 0
            cola[sig, 3] <- 0
        SiNo
            actual <- ctl[4]
            seguir <- Verdadero
            Mientras seguir Hacer
                sig <- cola[actual, 3]
                Si sig = 0 Entonces
                    seguir <- Falso
                SiNo
                    Si cola[sig, 1] = id Entonces
                        cola[actual, 3] <- cola[sig, 3]
                        cola[sig, 1] <- 0
                        cola[sig, 3] <- 0
                        seguir <- Falso
                    SiNo
                        actual <- sig
                    FinSi
                FinSi
            FinMientras
        FinSi
    FinSi
FinSubProceso

// Algoritmo "EjecutarSiguiente".
SubProceso EjecutarSiguiente(proc, tareas, cola, ctl)
    Definir id, nodo Como Entero
    id <- Desencolar(cola, ctl)
    Si id = 0 Entonces
        Escribir "No hay procesos listos para ejecutar."
    SiNo
        nodo <- BuscarProceso(proc, ctl, id)
        proc[nodo, 3] <- 2
        Escribir "Proceso seleccionado: ", id, " | ", tareas[nodo], " | prioridad ", proc[nodo, 2]
        Escribir "Su estado ahora es: ", NombreEstado(proc[nodo, 3]), "."
    FinSi
FinSubProceso


// ---------------------------------------------------------------------------
//  PILA DE MEMORIA  (GestorMemoria)
// ---------------------------------------------------------------------------

// Verdadero si el proceso ya tiene un bloque en la pila.
Funcion tiene <- TieneBloque(pila, ctl, id)
    Definir tiene Como Logico
    Definir nodo Como Entero
    tiene <- Falso
    nodo <- ctl[5]
    Mientras nodo <> 0 Hacer
        Si pila[nodo, 1] = id Entonces
            tiene <- Verdadero
        FinSi
        nodo <- pila[nodo, 3]
    FinMientras
FinFuncion

// Algoritmo "AsignarMemoria" (push).
SubProceso AsignarMemoria(proc, pila, ctl, id)
    Definir nodo, bloque, i Como Entero
    nodo <- BuscarProceso(proc, ctl, id)
    Si nodo = 0 Entonces
        Escribir "Error: No existe un proceso con ese identificador."
    SiNo
        Si proc[nodo, 3] <> 2 Entonces
            Escribir "Error: El proceso debe estar en ejecucion para recibir memoria."
        SiNo
            Si TieneBloque(pila, ctl, id) Entonces
                Escribir "Error: El proceso ya tiene un bloque de memoria asignado."
            SiNo
                Si ctl[6] + proc[nodo, 4] > 1024 Entonces
                    Escribir "Error: Memoria insuficiente. Solicitado: ", proc[nodo, 4], " KB, disponible: ", 1024 - ctl[6], " KB."
                SiNo
                    bloque <- 0
                    Para i <- ctl[7] Hasta 1 Con Paso -1 Hacer
                        Si pila[i, 1] = 0 Entonces
                            bloque <- i
                        FinSi
                    FinPara
                    pila[bloque, 1] <- id
                    pila[bloque, 2] <- proc[nodo, 4]
                    pila[bloque, 3] <- ctl[5]
                    ctl[5] <- bloque
                    ctl[6] <- ctl[6] + proc[nodo, 4]
                    Escribir "Memoria asignada al proceso ", id, ": ", proc[nodo, 4], " KB."
                    Escribir "Memoria usada: ", ctl[6], " de 1024 KB (disponible: ", 1024 - ctl[6], " KB)."
                FinSi
            FinSi
        FinSi
    FinSi
FinSubProceso

// Algoritmo "LiberarMemoria" (pop): retira siempre el ultimo bloque.
SubProceso LiberarMemoria(pila, ctl)
    Definir bloque Como Entero
    Si ctl[5] = 0 Entonces
        Escribir "Error: La pila esta vacia: no hay memoria para liberar."
    SiNo
        bloque <- ctl[5]
        ctl[5] <- pila[bloque, 3]
        ctl[6] <- ctl[6] - pila[bloque, 2]
        Escribir "Se libero el bloque del proceso ", pila[bloque, 1], " (", pila[bloque, 2], " KB)."
        Escribir "Memoria usada: ", ctl[6], " de 1024 KB (disponible: ", 1024 - ctl[6], " KB)."
        pila[bloque, 1] <- 0
        pila[bloque, 2] <- 0
        pila[bloque, 3] <- 0
    FinSi
FinSubProceso


// ---------------------------------------------------------------------------
//  OPCIONES DEL MENU (el menu coordina los modulos; ellos no se llaman entre si)
// ---------------------------------------------------------------------------

SubProceso MostrarMenu()
    Escribir ""
    Escribir "========== SIMULADOR DE PROCESOS - LABORATORIO CLINICO =========="
    Escribir " 1. Registrar proceso"
    Escribir " 2. Mostrar procesos"
    Escribir " 3. Buscar proceso por identificador"
    Escribir " 4. Eliminar proceso"
    Escribir " 5. Ejecutar siguiente proceso"
    Escribir " 6. Cambiar estado de un proceso"
    Escribir " 7. Asignar memoria"
    Escribir " 8. Liberar memoria"
    Escribir " 0. Salir"
FinSubProceso

// Opcion 1
SubProceso OpcionRegistrar(proc, codigos, tareas, cola, ctl)
    Definir codigo, tarea, sPrio, sMem Como Caracter
    Definir prio, mem, nuevoId Como Entero
    Escribir "--- Registrar proceso ---"
    Escribir Sin Saltar "Codigo de muestra (ficticio, ej. MUE-0001): "
    Leer codigo
    Escribir Sin Saltar "Tipo de tarea: "
    Leer tarea
    Escribir Sin Saltar "Prioridad (1 = alta, 2 = media, 3 = baja): "
    Leer sPrio
    Escribir Sin Saltar "Memoria solicitada (KB): "
    Leer sMem
    Si EstaVacio(codigo) Entonces
        codigo <- "(sin codigo)"
    FinSi
    Si EstaVacio(tarea) Entonces
        Escribir "Error: El tipo de tarea no puede estar vacio."
    SiNo
        Si NO EsEntero(sPrio) Entonces
            Escribir "Error: Dato invalido. La prioridad debe ser un numero entero entre 1 y 3."
        SiNo
            Si NO EsEntero(sMem) Entonces
                Escribir "Error: Dato invalido. La memoria debe ser un numero entero mayor que cero."
            SiNo
                prio <- ConvertirANumero(sPrio)
                mem <- ConvertirANumero(sMem)
                nuevoId <- AgregarProceso(proc, codigos, tareas, cola, ctl, codigo, tarea, prio, mem)
            FinSi
        FinSi
    FinSi
FinSubProceso

// Opcion 3
SubProceso OpcionBuscar(proc, codigos, tareas, ctl)
    Definir id, nodo Como Entero
    Escribir "--- Buscar proceso ---"
    id <- LeerEntero("Identificador del proceso: ")
    Si id = -1000000000 Entonces
        Escribir "Error: Dato invalido. El identificador debe ser un numero entero."
    SiNo
        nodo <- BuscarProceso(proc, ctl, id)
        Si nodo = 0 Entonces
            Escribir "Error: No existe un proceso con ese identificador."
        SiNo
            Escribir "Proceso encontrado:"
            Escribir "  Identificador : ", proc[nodo, 1]
            Escribir "  Codigo muestra: ", codigos[nodo]
            Escribir "  Tarea         : ", tareas[nodo]
            Escribir "  Prioridad     : ", proc[nodo, 2]
            Escribir "  Estado        : ", NombreEstado(proc[nodo, 3])
            Escribir "  Memoria       : ", proc[nodo, 4], " KB"
        FinSi
    FinSi
FinSubProceso

// Opcion 4: buscar -> consultar pila -> eliminar
SubProceso OpcionEliminar(proc, pila, ctl)
    Definir id, nodo Como Entero
    Escribir "--- Eliminar proceso ---"
    id <- LeerEntero("Identificador del proceso: ")
    Si id = -1000000000 Entonces
        Escribir "Error: Dato invalido. El identificador debe ser un numero entero."
    SiNo
        nodo <- BuscarProceso(proc, ctl, id)
        Si nodo = 0 Entonces
            Escribir "Error: No existe un proceso con ese identificador."
        SiNo
            Si proc[nodo, 3] <> 4 Entonces
                Escribir "Error: Solo se pueden eliminar procesos finalizados."
            SiNo
                Si TieneBloque(pila, ctl, id) Entonces
                    Escribir "Error: El proceso tiene memoria asignada; liberela antes de eliminarlo."
                SiNo
                    EliminarNodo(proc, ctl, id)
                    Escribir "Proceso ", id, " eliminado."
                FinSi
            FinSi
        FinSi
    FinSi
FinSubProceso

// Opcion 6: buscar -> validar transicion -> ajustar cola -> actualizar estado
SubProceso OpcionCambiarEstado(proc, cola, ctl)
    Definir id, nodo, nuevo, anterior Como Entero
    Escribir "--- Cambiar estado de un proceso ---"
    id <- LeerEntero("Identificador del proceso: ")
    Si id = -1000000000 Entonces
        Escribir "Error: Dato invalido. El identificador debe ser un numero entero."
    SiNo
        nodo <- BuscarProceso(proc, ctl, id)
        Si nodo = 0 Entonces
            Escribir "Error: No existe un proceso con ese identificador."
        SiNo
            Escribir "Estado actual: ", NombreEstado(proc[nodo, 3])
            Escribir "Nuevo estado: 1. Listo  2. En ejecucion  3. Bloqueado  4. Finalizado"
            nuevo <- LeerEntero("Seleccione el nuevo estado: ")
            Si nuevo < 1 O nuevo > 4 Entonces
                Escribir "Error: Estado no valido. Ingrese un numero del 1 al 4."
            SiNo
                Si NO EsTransicionValida(proc[nodo, 3], nuevo) Entonces
                    Escribir "Error: Transicion no permitida: de ", NombreEstado(proc[nodo, 3]), " a ", NombreEstado(nuevo), "."
                SiNo
                    Si proc[nodo, 3] = 1 Y nuevo = 2 Entonces
                        ExtraerDeCola(cola, ctl, id)
                    FinSi
                    Si proc[nodo, 3] = 3 Y nuevo = 1 Entonces
                        Encolar(cola, ctl, id, proc[nodo, 2])
                    FinSi
                    anterior <- proc[nodo, 3]
                    proc[nodo, 3] <- nuevo
                    Escribir "El estado del proceso ", id, " cambio de ", NombreEstado(anterior), " a ", NombreEstado(nuevo), "."
                FinSi
            FinSi
        FinSi
    FinSi
FinSubProceso

// Opcion 7
SubProceso OpcionAsignarMemoria(proc, pila, ctl)
    Definir id Como Entero
    Escribir "--- Asignar memoria ---"
    id <- LeerEntero("Identificador del proceso: ")
    Si id = -1000000000 Entonces
        Escribir "Error: Dato invalido. El identificador debe ser un numero entero."
    SiNo
        AsignarMemoria(proc, pila, ctl, id)
    FinSi
FinSubProceso


// ---------------------------------------------------------------------------
//  PROGRAMA PRINCIPAL
// ---------------------------------------------------------------------------
Algoritmo SistemaGestionG03
    Definir proc, cola, pila, ctl Como Entero
    Definir codigos, tareas Como Caracter
    Definir txt Como Caracter
    Definir op, i Como Entero
    Dimension proc[50, 5]
    Dimension cola[50, 3]
    Dimension pila[50, 3]
    Dimension ctl[7]
    Dimension codigos[50]
    Dimension tareas[50]

    // Inicializacion de las estructuras (todas las filas libres)
    Para i <- 1 Hasta 50 Con Paso 1 Hacer
        proc[i, 1] <- 0
        proc[i, 2] <- 0
        proc[i, 3] <- 0
        proc[i, 4] <- 0
        proc[i, 5] <- 0
        cola[i, 1] <- 0
        cola[i, 2] <- 0
        cola[i, 3] <- 0
        pila[i, 1] <- 0
        pila[i, 2] <- 0
        pila[i, 3] <- 0
        codigos[i] <- ""
        tareas[i] <- ""
    FinPara
    ctl[1] <- 0
    ctl[2] <- 0
    ctl[3] <- 1
    ctl[4] <- 0
    ctl[5] <- 0
    ctl[6] <- 0
    ctl[7] <- 50

    // Bucle del menu principal
    Repetir
        MostrarMenu()
        Escribir Sin Saltar "Seleccione una opcion: "
        Leer txt
        Si EsEntero(txt) Entonces
            op <- ConvertirANumero(txt)
        SiNo
            op <- -1
        FinSi
        Segun op Hacer
            1:
                OpcionRegistrar(proc, codigos, tareas, cola, ctl)
            2:
                MostrarProcesos(proc, codigos, tareas, ctl)
            3:
                OpcionBuscar(proc, codigos, tareas, ctl)
            4:
                OpcionEliminar(proc, pila, ctl)
            5:
                Escribir "--- Ejecutar siguiente proceso ---"
                EjecutarSiguiente(proc, tareas, cola, ctl)
            6:
                OpcionCambiarEstado(proc, cola, ctl)
            7:
                OpcionAsignarMemoria(proc, pila, ctl)
            8:
                Escribir "--- Liberar memoria ---"
                LiberarMemoria(pila, ctl)
            0:
                Escribir "Saliendo del programa..."
            De Otro Modo:
                Escribir "Error: Opcion no valida. Ingrese un numero del 0 al 8."
        FinSegun
    Hasta Que op = 0
FinAlgoritmo
