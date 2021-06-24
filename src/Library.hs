module Library where
import PdePreludat

-- Enunciado general
-- El fondo monetario internacional nos solicitó que modelemos su negocio, basado en realizar préstamos a países en apuros financieros. 
-- Sabemos de cada país el "ingreso per cápita" que es el promedio de lo que cada habitante necesita para subsistir, 
-- también conocemos la población activa en el sector público y la activa en el sector privado, la lista de recursos naturales 
-- (ej: "Minería", "Petróleo", "Industria pesada") y la deuda que mantiene con el FMI.
-- Se pide implementar en Haskell los siguientes requerimientos explicitando el tipo de cada función:
type Recurso = String
data Pais = Pais {
  ingresoXCapita   :: Number,
  poblacionPublica :: Number,
  poblacionPrivada :: Number,
  recursos         :: [Recurso],
  deuda            :: Number
} deriving (Show, Eq, Ord)

-- El FMI es especialista en dar recetas. Cada receta combina una o más estrategias que se describen a continuación:
-- (4 puntos) Implementar las estrategias que forman parte de las recetas del FMI.
--    * Prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
type Estrategia = Pais -> Pais

prestarMillones :: Number -> Estrategia
prestarMillones millones = modificarDeuda (millones * 1.50)

modificarDeuda :: Number -> Estrategia
modificarDeuda deudaADisminuir pais = pais {
  deuda = deuda pais + deudaADisminuir
}

--    * reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público 
--      y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario
reducirPuestosPublicos :: Number -> Estrategia
reducirPuestosPublicos cantidadPuestos pais = pais {
  poblacionPublica = poblacionPublica pais - cantidadPuestos,
  ingresoXCapita = disminuirIngresoXCapita cantidadPuestos (ingresoXCapita pais)
}

disminuirIngresoXCapita :: Number -> Number -> Number
disminuirIngresoXCapita cantidadPuestos ingresoXCapita 
  | cantidadPuestos > 100     = ingresoXCapita * 0.80
  | otherwise                 = ingresoXCapita * 0.85

--    * darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que 
--      el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.
explotarRecursosNaturales :: Recurso -> Estrategia
explotarRecursosNaturales recurso pais = modificarDeuda (-2) pais {
  recursos = filter (/=recurso) (recursos pais)
}

--    * establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso 
--      per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. 
--      Evitar la repetición de código.
blindaje :: Estrategia
blindaje pais = modificarDeuda (div 2 (productoBrutoInterno pais)) pais {
  poblacionPublica = poblacionPublica pais - 500
}

productoBrutoInterno :: Pais -> Number
productoBrutoInterno pais = ingresoXCapita pais * poblacionActiva pais

poblacionActiva :: Pais -> Number
poblacionActiva pais = poblacionPublica pais + poblacionPrivada pais

type Receta = [Estrategia]

-- (2 puntos)
-- Representar el TAD País.
-- Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, la población activa del sector público es de 400.000, la 
-- población activa del sector privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.
namibia = Pais {
  ingresoXCapita    = 4140,
  poblacionPublica  = 400000,
  poblacionPrivada  = 650000,
  recursos          = ["Mineria","Ecoturismo"],
  deuda             = 50
}

-- (2 puntos)
-- Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.
-- Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.

recetaEspecial :: Receta
recetaEspecial = [prestarMillones 200,explotarRecursosNaturales "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldl (\pais estrategia -> estrategia pais) pais receta
-- opción con foldr + $
--aplicarReceta receta pais = foldr ($) pais receta

-- (3 puntos) Resolver todo el punto con orden superior, composición y aplicación parcial, no puede utilizar funciones auxiliares.
-- Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.
zafarPaises :: [Pais] -> [Pais]
zafarPaises = filter $ elem "Petróleo" . recursos

-- Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
totalDeuda :: [Pais] -> Number
totalDeuda = foldr ((+).deuda) 0
-- Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.


-- (2 puntos) Debe resolver este punto con recursividad: dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”,
--  en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge 
--  de multiplicar el ingreso per cápita por la población activa (privada y pública).
recetaOrdenada :: Pais -> [Receta] -> Bool
recetaOrdenada _ [receta] = True
recetaOrdenada pais (recetaPrimera:recetaSegunda:recetas)
  | productoBrutoInterno (aplicarReceta recetaPrimera pais) <= productoBrutoInterno (aplicarReceta recetaSegunda pais) = recetaOrdenada pais (recetaSegunda:recetas)
  | otherwise = False

-- (1 punto) Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
-- ¿qué sucede evaluamos la función 4a con ese país? 
-- ¿y con la 4b?
-- Justifique ambos puntos relacionándolos con algún concepto.


-- Recordatorio: Solo puede utilizar recursividad en el punto 5.b. Cualquier otra solución donde quiera aplicar recursividad no será tomada en cuenta.