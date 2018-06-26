import sys
import os
import fileinput 

# Ver si los argumentos son correctos
if (len(sys.argv) < 2 ) :
	print ("Error: Falta directorio de datos")
else:
    with open('VPIC/TECH_SPEC_2013-2017.csv', 'w') as fo:
        ficheros = [name for name in os.listdir(sys.argv[1]) if name.endswith(".csv")]        
        for fi in ficheros:
            fo.writelines (fileinput.input(sys.argv[1] +"/" + fi))
        fileinput.close()
        fo.close()