import requests,csv;

f = open('TraduccionMarcas.csv', 'r', newline='')
marcas = csv.reader(f)

#La tabla de traducción tiene la forma:
# Marca IDAE , Marca VPIC
for l in marcas:
    marca = l.split(",")[1]

    for y in range (2013, 2017):
        outputfile = marca + "-" + str(y) + ".csv"
        url = "https://vpic.nhtsa.dot.gov/api/vehicles/GetCanadianVehicleSpecifications/?year=" + str(y) + "&make=" + marca + "&Model=&units=&format=csv"
        # Almacenar el resultado de la query
        f2 = open (outputfile, "w")
        r = requests.get(url);
        f2.write(r.text)        
        f2.close

f.close()