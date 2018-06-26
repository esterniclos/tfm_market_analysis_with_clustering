# Idae marcas
import sys


class buscarModelos:
    
    def __init__(self, marca):
        self.l_modelos = []
        self.marca = marca

    def __buscar_etiqueta (self, fp, etiqueta):
         # busca la línea adecuada e imprime el valor.
        line = fp.readline()
        while line : 
            line = line.lstrip() # Quitar blancos por delante.
            if (etiqueta in line):
                return line
            line = fp.readline()
        return None
    
    def __leer_modelo (self, fp):
        line = fp.readline()
        while line: 
            if ("</select>" in line):
                return None
            if ("option value=" in line):
                modelo = line.split(">")[1].split("<")[0]
                print (marca + "," + modelo)
                self.l_modelos.append("modelo")
            line = fp.readline()

        return None


    def analizarHTM (self, filename):
        fp = open(filename,"r", encoding="utf-8")
        self.__buscar_etiqueta(fp, "ctl00_ContenidoPagina_ddlModelo")
        while (self.__leer_modelo(fp) != None):
            print ("Ubicados " + l_modelos.Count() + " modelos.")
        fp.close


# Ver si los argumentos son correctos
if (len(sys.argv) < 2 ) :
	print ("Error: Falta archivo de entrada")
	print ("Sintáxis de windows para las barras de directorios")
	print ("Uso  :", str(sys.argv[0]), "mod_DFDF.xml [mod_DFDF1.xml]")


# Todos los archivos que pasen serán validados
i = 1
print ("Marca,Modelo")
while (i < len(sys.argv)) :
    filename = str(sys.argv[i])
    marca = filename.split("_")[3].replace(".htm", "")
    C = buscarModelos(marca)
    C.analizarHTM(filename)
    i+=1
