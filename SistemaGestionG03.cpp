// ===========================================================================
//  SISTEMA DE GESTION DE PROCESOS - LABORATORIO CLINICO (Grupo 03)
//  Curso: Estructura de datos
//
//  Simulador en consola que usa 3 estructuras de datos hechas a mano:
//    1) LISTA ENLAZADA    -> guarda los procesos registrados
//    2) COLA DE PRIORIDAD -> ordena los procesos que estan listos
//    3) PILA              -> guarda los bloques de memoria (el ultimo en entrar sale primero)
//  Todos los datos son ficticios (no se usan datos de pacientes reales).
// ===========================================================================
#include <iostream>     // Libreria para entrada y salida (cin, cout)
#include <string>       // Libreria para usar la clase string
#include <cstdlib>      // Libreria para usar system("cls"), system("pause") y exit()

using namespace std;    // Permite usar nombres estandar sin anteponer "std::"

const int CAPACIDAD_TOTAL = 1024;   // Memoria total simulada (en KB)

// Estructura para representar un proceso (nodo de la lista enlazada)
struct Proceso {
    int id;                     // Identificador unico (1, 2, 3...)
    string codigoMuestra;       // Codigo ficticio de la muestra (ej. MUE-0001)
    string tipoTarea;           // Tarea de laboratorio que representa
    int prioridad;              // 1 (alta), 2 (media) o 3 (baja)
    string estado;              // "Listo", "En ejecucion", "Bloqueado" o "Finalizado"
    int memoria;                // Memoria que solicita el proceso (KB)
    Proceso* siguiente;         // Puntero al siguiente proceso (lista enlazada)
};

// Puntero global al inicio de la lista de procesos
Proceso* cabezaProcesos = NULL;

// Estructura para nodo de la cola de planificacion
struct NodoCola {
    Proceso* proceso;           // Puntero al proceso contenido
    NodoCola* siguiente;        // Puntero al siguiente nodo en la cola
};

// Puntero global al inicio de la cola (el que sale primero)
NodoCola* cabezaCola = NULL;

// Estructura para representar un bloque de memoria asignado a un proceso
struct BloqueMemoria {
    Proceso* proceso;           // Proceso dueno del bloque
    int tamanio;                // Tamano del bloque (KB)
    BloqueMemoria* siguiente;   // Bloque que se asigno antes (el de abajo en la pila)
};

// Puntero al tope de la pila de memoria (ultimo bloque asignado)
BloqueMemoria* topeMemoria = NULL;

int siguienteId = 1;            // Proximo identificador por asignar
int memoriaUsada = 0;           // Memoria ocupada por los bloques de la pila (KB)

//---------------- FUNCIONES AUXILIARES ----------------//

// Muestra un mensaje y lee un numero entero. Si el usuario escribe algo que
// no es un numero (por ejemplo "abc"), avisa y vuelve a pedirlo.
int leerEntero(string mensaje) {
    int numero;
    cout << mensaje;
    while (!(cin >> numero)) {          // Mientras no se haya escrito un numero
        if (cin.eof()) exit(0);         // Si se acabo la entrada de datos, termina el programa
        cin.clear();                    // Limpia el estado de error de cin
        cin.ignore(1000, '\n');         // Descarta lo que se escribio mal
        cout << "Error: Dato invalido. Debe ingresar un numero entero.\n";
        cout << mensaje;
    }
    cin.ignore(1000, '\n');             // Descarta el salto de linea (para que getline funcione despues)
    return numero;
}

// Busca un proceso por su ID en la lista enlazada
Proceso* buscarProcesoPorID(int id) {
    Proceso* actual = cabezaProcesos;   // Comienza desde la cabeza de la lista
    while (actual != NULL) {
        if (actual->id == id)           // Si se encuentra el ID
            return actual;              // Retorna el puntero al proceso
        actual = actual->siguiente;     // Avanza al siguiente nodo
    }
    return NULL; // No se encontro el proceso
}

// Verifica si un proceso ya tiene un bloque de memoria en la pila
bool tieneBloque(int id) {
    BloqueMemoria* actual = topeMemoria;    // Comienza desde el tope de la pila
    while (actual != NULL) {
        if (actual->proceso->id == id)      // Si encuentra el ID en algun bloque
            return true;
        actual = actual->siguiente;         // Avanza al siguiente bloque
    }
    return false; // El proceso no tiene memoria asignada
}

// Indica si el proceso "a" debe salir de la cola antes que el proceso "b".
// Va primero el de menor numero de prioridad; si empatan, el de menor ID.
bool tieneMasPrioridad(Proceso* a, Proceso* b) {
    if (a->prioridad != b->prioridad)
        return a->prioridad < b->prioridad;
    return a->id < b->id;
}

// Inserta un proceso en la cola, ordenado por prioridad
void encolarProceso(Proceso* p) {
    NodoCola* nuevo = new NodoCola;     // Crea un nuevo nodo
    nuevo->proceso = p;                 // Apunta al proceso
    nuevo->siguiente = NULL;

    // Si la cola esta vacia o el nuevo tiene mas prioridad que el primero
    if (cabezaCola == NULL || tieneMasPrioridad(p, cabezaCola->proceso)) {
        nuevo->siguiente = cabezaCola;
        cabezaCola = nuevo;             // Se convierte en el nuevo primero
    } else {
        NodoCola* actual = cabezaCola;
        // Avanza mientras el siguiente tenga mas prioridad que el nuevo
        while (actual->siguiente != NULL && !tieneMasPrioridad(p, actual->siguiente->proceso)) {
            actual = actual->siguiente;
        }
        // Inserta el nuevo nodo en la posicion adecuada
        nuevo->siguiente = actual->siguiente;
        actual->siguiente = nuevo;
    }
}

// Saca de la cola un proceso concreto (cuando deja de estar "Listo")
void sacarDeCola(int id) {
    if (cabezaCola == NULL) return;     // Cola vacia: no hay nada que sacar

    if (cabezaCola->proceso->id == id) {    // El proceso esta al frente
        NodoCola* temp = cabezaCola;
        cabezaCola = cabezaCola->siguiente;
        delete temp;                    // Libera el nodo
        return;
    }

    NodoCola* actual = cabezaCola;
    while (actual->siguiente != NULL && actual->siguiente->proceso->id != id)
        actual = actual->siguiente;

    if (actual->siguiente != NULL) {    // Se encontro el proceso
        NodoCola* temp = actual->siguiente;
        actual->siguiente = temp->siguiente;    // Lo saca de la cola
        delete temp;
    }
}

//---------------- GESTOR DE PROCESOS ----------------//

// Registra un proceso: lo agrega a la lista enlazada y lo mete a la cola
void insertarProceso() {
    cout << "--- Registrar proceso ---\n";

    string codigo;
    do {
        cout << "Codigo de muestra (ficticio, ej. MUE-0001): ";
        getline(cin, codigo);               // Lee el texto completo con espacios
        if (codigo.empty()) {
            cout << "Error: El codigo no puede estar vacio.\n";
        }
    } while (codigo.empty());               // Repite hasta que no este vacio

    string tarea;
    do {
        cout << "Tipo de tarea: ";
        getline(cin, tarea);
        if (tarea.empty()) {
            cout << "Error: El tipo de tarea no puede estar vacio.\n";
        }
    } while (tarea.empty());

    int prioridad;
    do {
        prioridad = leerEntero("Prioridad (1 = alta, 2 = media, 3 = baja): ");
        if (prioridad < 1 || prioridad > 3) {
            cout << "Error: La prioridad debe estar entre 1 y 3.\n";
        }
    } while (prioridad < 1 || prioridad > 3);

    int memoria;
    do {
        memoria = leerEntero("Memoria solicitada (KB): ");
        if (memoria <= 0) {
            cout << "Error: La memoria solicitada debe ser mayor que cero.\n";
        }
    } while (memoria <= 0);

    Proceso* nuevo = new Proceso;           // Crea un nuevo proceso dinamicamente
    nuevo->id = siguienteId;                // Asigna los valores
    siguienteId++;                          // El siguiente proceso tendra otro ID
    nuevo->codigoMuestra = codigo;
    nuevo->tipoTarea = tarea;
    nuevo->prioridad = prioridad;
    nuevo->estado = "Listo";                // Todo proceso nuevo empieza "Listo"
    nuevo->memoria = memoria;
    nuevo->siguiente = NULL;

    if (cabezaProcesos == NULL) {           // Si la lista esta vacia
        cabezaProcesos = nuevo;             // El nuevo proceso es el primero
    } else {
        Proceso* actual = cabezaProcesos;
        while (actual->siguiente != NULL) actual = actual->siguiente;
        actual->siguiente = nuevo;          // Agrega el proceso al final de la lista
    }

    encolarProceso(nuevo);                  // Entra a la cola de prioridad
    cout << "Proceso registrado con el identificador " << nuevo->id << " (estado: Listo).\n";
}

// Muestra todos los procesos registrados
void mostrarProcesos() {
    cout << "--- Lista de procesos registrados ---\n";

    // Verifica si no hay procesos en la lista
    if (cabezaProcesos == NULL) {
        cout << "No hay procesos registrados.\n";
        return;
    }

    // Recorre la lista de procesos e imprime sus datos
    Proceso* actual = cabezaProcesos;
    while (actual != NULL) {
        cout << "ID: " << actual->id << ", Codigo: " << actual->codigoMuestra
             << ", Tarea: " << actual->tipoTarea << ", Prioridad: " << actual->prioridad
             << ", Estado: " << actual->estado << ", Memoria: " << actual->memoria << " KB\n";
        actual = actual->siguiente;         // Avanza al siguiente nodo
    }
}

// Busca un proceso por su ID y muestra sus datos
void buscarProceso() {
    cout << "--- Buscar proceso ---\n";
    int id = leerEntero("Ingrese el ID del proceso: ");

    Proceso* p = buscarProcesoPorID(id);
    if (p == NULL) {
        cout << "Error: No existe un proceso con ese ID.\n";
        return;
    }
    cout << "Proceso encontrado:\n";
    cout << "  ID            : " << p->id << "\n";
    cout << "  Codigo muestra: " << p->codigoMuestra << "\n";
    cout << "  Tarea         : " << p->tipoTarea << "\n";
    cout << "  Prioridad     : " << p->prioridad << "\n";
    cout << "  Estado        : " << p->estado << "\n";
    cout << "  Memoria       : " << p->memoria << " KB\n";
}

// Elimina un proceso de la lista (solo si esta Finalizado y sin memoria asignada)
void eliminarProceso() {
    cout << "--- Eliminar proceso ---\n";
    int id = leerEntero("Ingrese el ID del proceso a eliminar: ");

    Proceso* p = buscarProcesoPorID(id);
    if (p == NULL) {
        cout << "Error: No existe un proceso con ese ID.\n";
        return;
    }
    if (p->estado != "Finalizado") {
        cout << "Error: Solo se pueden eliminar procesos finalizados.\n";
        return;
    }
    if (tieneBloque(id)) {
        cout << "Error: El proceso tiene memoria asignada; liberela antes de eliminarlo.\n";
        return;
    }

    if (cabezaProcesos == p) {              // El proceso a eliminar es el primero
        cabezaProcesos = p->siguiente;
    } else {
        Proceso* anterior = cabezaProcesos;
        while (anterior->siguiente != p)
            anterior = anterior->siguiente;
        anterior->siguiente = p->siguiente; // Lo saca de la lista
    }
    delete p;                               // Libera la memoria del nodo
    cout << "Proceso " << id << " eliminado.\n";
}

// Cambia el estado de un proceso (solo con los cambios permitidos)
void cambiarEstado() {
    cout << "--- Cambiar estado de un proceso ---\n";
    int id = leerEntero("Ingrese el ID del proceso: ");

    Proceso* p = buscarProcesoPorID(id);
    if (p == NULL) {
        cout << "Error: No existe un proceso con ese ID.\n";
        return;
    }
    cout << "Estado actual: " << p->estado << "\n";
    cout << "1. Listo   2. En ejecucion   3. Bloqueado   4. Finalizado\n";

    int opcion;
    do {
        opcion = leerEntero("Seleccione el nuevo estado (1-4): ");
        if (opcion < 1 || opcion > 4) {
            cout << "Error: Estado no valido. Ingrese un numero del 1 al 4.\n";
        }
    } while (opcion < 1 || opcion > 4);

    string nuevo;
    if (opcion == 1) nuevo = "Listo";
    else if (opcion == 2) nuevo = "En ejecucion";
    else if (opcion == 3) nuevo = "Bloqueado";
    else nuevo = "Finalizado";

    // Cambios permitidos: Listo -> En ejecucion, En ejecucion -> Bloqueado o
    // Finalizado, y Bloqueado -> Listo
    bool permitido = (p->estado == "Listo" && nuevo == "En ejecucion") ||
                     (p->estado == "En ejecucion" && (nuevo == "Bloqueado" || nuevo == "Finalizado")) ||
                     (p->estado == "Bloqueado" && nuevo == "Listo");
    if (!permitido) {
        cout << "Error: Transicion no permitida: de " << p->estado << " a " << nuevo << ".\n";
        return;
    }

    // La cola solo guarda procesos "Listo": se saca o se vuelve a meter
    if (p->estado == "Listo") sacarDeCola(id);
    if (nuevo == "Listo") encolarProceso(p);

    cout << "El estado del proceso " << id << " cambio de " << p->estado << " a " << nuevo << ".\n";
    p->estado = nuevo;
}

//---------------- PLANIFICADOR DE CPU ----------------//

// Saca el primer proceso de la cola (mayor prioridad) y lo "ejecuta"
void desencolarYEjecutarProceso() {
    cout << "--- Desencolar y ejecutar proceso ---\n";

    // Verifica si la cola esta vacia
    if (cabezaCola == NULL) {
        cout << "No hay procesos listos para ejecutar.\n";
        return;
    }

    NodoCola* temp = cabezaCola;            // Toma el primer nodo (mayor prioridad)
    cabezaCola = cabezaCola->siguiente;     // Avanza el inicio de la cola
    Proceso* p = temp->proceso;
    delete temp;                            // Libera el nodo ejecutado

    p->estado = "En ejecucion";
    cout << "Ejecutando proceso:\n";
    cout << "ID: " << p->id << ", Tarea: " << p->tipoTarea << ", Prioridad: " << p->prioridad << "\n";
    cout << "Su estado ahora es: " << p->estado << ".\n";
}

// Muestra la cola de planificacion
void mostrarColaPlanificador() {
    cout << "--- Cola de planificacion (ordenada por prioridad) ---\n";

    if (cabezaCola == NULL) {
        cout << "La cola de planificacion esta vacia.\n";
        return;
    }

    // Recorre la cola e imprime cada proceso
    NodoCola* actual = cabezaCola;
    while (actual != NULL) {
        cout << "ID: " << actual->proceso->id << ", Tarea: " << actual->proceso->tipoTarea
             << ", Prioridad: " << actual->proceso->prioridad << "\n";
        actual = actual->siguiente;
    }
}

//---------------- GESTOR DE MEMORIA ----------------//

// Asigna memoria a un proceso (push a la pila de memoria)
void asignarMemoria() {
    cout << "--- Asignar memoria a proceso ---\n";
    int id = leerEntero("Ingrese el ID del proceso: ");

    Proceso* p = buscarProcesoPorID(id);
    if (p == NULL) {
        cout << "Error: No existe un proceso con ese ID.\n";
        return;
    }
    if (p->estado != "En ejecucion") {
        cout << "Error: El proceso debe estar en ejecucion para recibir memoria.\n";
        return;
    }
    if (tieneBloque(id)) {
        cout << "Error: El proceso ya tiene un bloque de memoria asignado.\n";
        return;
    }
    if (memoriaUsada + p->memoria > CAPACIDAD_TOTAL) {
        cout << "Error: Memoria insuficiente. Solicitado: " << p->memoria
             << " KB, disponible: " << CAPACIDAD_TOTAL - memoriaUsada << " KB.\n";
        return;
    }

    // Crea el bloque y lo pone en el tope de la pila
    BloqueMemoria* nuevo = new BloqueMemoria;
    nuevo->proceso = p;                     // Asocia el bloque con el proceso
    nuevo->tamanio = p->memoria;            // Tamano del bloque
    nuevo->siguiente = topeMemoria;         // Queda encima del bloque anterior
    topeMemoria = nuevo;                    // El nuevo bloque es ahora el tope
    memoriaUsada = memoriaUsada + p->memoria;

    cout << "Memoria asignada al proceso " << id << ": " << p->memoria << " KB.\n";
    cout << "Memoria usada: " << memoriaUsada << " de " << CAPACIDAD_TOTAL << " KB.\n";
}

// Libera el bloque superior de la pila de memoria (el ultimo asignado)
void liberarMemoria() {
    cout << "--- Liberar memoria ---\n";

    // Verifica si la pila esta vacia
    if (topeMemoria == NULL) {
        cout << "Error: La pila esta vacia, no hay memoria para liberar.\n";
        return;
    }

    BloqueMemoria* temp = topeMemoria;      // Guarda el bloque superior
    topeMemoria = topeMemoria->siguiente;   // El tope baja al bloque anterior
    memoriaUsada = memoriaUsada - temp->tamanio;

    cout << "Se libero el bloque del proceso " << temp->proceso->id
         << " (" << temp->tamanio << " KB).\n";
    cout << "Memoria usada: " << memoriaUsada << " de " << CAPACIDAD_TOTAL << " KB.\n";
    delete temp;                            // Libera el nodo del bloque
}

// Muestra el estado actual de la memoria
void estadoMemoria() {
    cout << "--- Estado actual de la memoria ---\n";

    if (topeMemoria == NULL) {
        cout << "La memoria esta completamente libre.\n";
        return;
    }

    // Recorre la pila desde el tope hacia abajo
    BloqueMemoria* actual = topeMemoria;
    while (actual != NULL) {
        cout << "Proceso " << actual->proceso->id << " - Tarea: " << actual->proceso->tipoTarea
             << " - Tamano: " << actual->tamanio << " KB\n";
        actual = actual->siguiente;
    }
    cout << "Memoria usada: " << memoriaUsada << " de " << CAPACIDAD_TOTAL
         << " KB (disponible: " << CAPACIDAD_TOTAL - memoriaUsada << " KB).\n";
}

//---------------- MENU PRINCIPAL ----------------//

// Libera todos los nodos antes de terminar el programa
void liberarTodo() {
    while (cabezaProcesos != NULL) {
        Proceso* temp = cabezaProcesos;
        cabezaProcesos = cabezaProcesos->siguiente;
        delete temp;
    }
    while (cabezaCola != NULL) {
        NodoCola* temp = cabezaCola;
        cabezaCola = cabezaCola->siguiente;
        delete temp;
    }
    while (topeMemoria != NULL) {
        BloqueMemoria* temp = topeMemoria;
        topeMemoria = topeMemoria->siguiente;
        delete temp;
    }
}

void menu() {
    int opcionPrincipal;

    do {
        do {
            cout << "\n=== Simulador de procesos - Laboratorio clinico ===\n";
            cout << "1. Gestor de Procesos\n";
            cout << "2. Planificador de CPU (Cola de Prioridad)\n";
            cout << "3. Gestor de Memoria\n";
            cout << "4. Salir\n";
            opcionPrincipal = leerEntero("Seleccione una opcion (1-4): ");

            if (opcionPrincipal < 1 || opcionPrincipal > 4) {
                cout << "Opcion invalida. Por favor ingrese un numero entre 1 y 4.\n";
            }
        } while (opcionPrincipal < 1 || opcionPrincipal > 4);

        switch (opcionPrincipal) {
            case 1: {
                int opcionProceso;
                do {
                    cout << "\n--- Gestor de Procesos ---\n";
                    cout << "1. Registrar proceso\n";
                    cout << "2. Mostrar todos los procesos\n";
                    cout << "3. Buscar proceso por ID\n";
                    cout << "4. Cambiar estado de un proceso\n";
                    cout << "5. Eliminar proceso\n";
                    cout << "6. Volver al menu principal\n";
                    opcionProceso = leerEntero("Seleccione una opcion (1-6): ");

                    switch (opcionProceso) {
                        case 1: insertarProceso(); break;
                        case 2: mostrarProcesos(); break;
                        case 3: buscarProceso(); break;
                        case 4: cambiarEstado(); break;
                        case 5: eliminarProceso(); break;
                        case 6: cout << "Volviendo al menu principal...\n"; break;
                        default: cout << "Opcion invalida.\n"; break;
                    }

                    system("pause"); // Espera una tecla
                    system("cls");   // Limpia pantalla
                } while (opcionProceso != 6);
                break;
            }
            case 2: {
                int opcionPlanificador;
                do {
                    cout << "\n=== Planificador de CPU (Cola de Prioridad) ===\n";
                    cout << "1. Desencolar y ejecutar proceso\n";
                    cout << "2. Mostrar cola actual\n";
                    cout << "3. Volver al menu principal\n";
                    opcionPlanificador = leerEntero("Seleccione una opcion (1-3): ");

                    switch (opcionPlanificador) {
                        case 1: desencolarYEjecutarProceso(); break;
                        case 2: mostrarColaPlanificador(); break;
                        case 3: cout << "Volviendo al menu principal...\n"; break;
                        default: cout << "Opcion invalida.\n"; break;
                    }

                    system("pause");
                    system("cls");
                } while (opcionPlanificador != 3);
                break;
            }
            case 3: {
                int opcionMemoria;
                do {
                    cout << "\n--- Gestor de Memoria ---\n";
                    cout << "1. Asignar memoria a proceso (push)\n";
                    cout << "2. Liberar memoria (pop)\n";
                    cout << "3. Ver estado actual de la memoria\n";
                    cout << "4. Volver al menu principal\n";
                    opcionMemoria = leerEntero("Seleccione una opcion (1-4): ");

                    switch (opcionMemoria) {
                        case 1: asignarMemoria(); break;
                        case 2: liberarMemoria(); break;
                        case 3: estadoMemoria(); break;
                        case 4: cout << "Volviendo al menu principal...\n"; break;
                        default: cout << "Opcion invalida.\n"; break;
                    }

                    system("pause");
                    system("cls");
                } while (opcionMemoria != 4);
                break;
            }
            case 4:
                cout << "Saliendo del programa...\n";
                break;
        }

    } while (opcionPrincipal != 4);
}

// Funcion principal del programa
int main() {
    menu();         // Llama al menu principal
    liberarTodo();  // Libera la memoria de todos los nodos
    return 0;
}
