import Circuitos.{Chip,crearChipUnario,crearChipBinario,half_adder,full_adder,adder}
import scala.math.pow
// Pruebas para crearChipUnario(f : Int => Int): Chip; Compuerta lógica NOT

val chip_not=crearChipUnario((x:Int)=>(1-x))
chip_not(List(0))
chip_not(List(1))


// Pruebas para crearChipBinario(f : (Int,Int) => Int) : Chip; Compuerta lógica AND

val chip_and=crearChipBinario((x:Int,y:Int)=>(x*y))
chip_and(List(0,0))
chip_and(List(0,1))
chip_and(List(1,0))
chip_and(List(1,1))


// Pruebas para crearChipBinario(f : (Int,Int) => Int) : Chip; Compuerta lógica OR

val chip_or=crearChipBinario((x:Int,y:Int)=>(x+y-x*y))
chip_or(List(0,0))
chip_or(List(1,0))
chip_or(List(0,1))
chip_or(List(1,1))

// Pruebas para half_adder : Chip

half_adder(List(0,0))
half_adder(List(0,1))
half_adder(List(1,0))
half_adder(List(1,1))

// Pruebas para full_adder : Chip

full_adder(List(1,1,0))
full_adder(List(1,1,1))
full_adder(List(0,1,0))
full_adder(List(0,1,1))
full_adder(List(1,0,1))

// Pruebas para adder(n : Int) : Chip

val add_1=adder(1)
val add_2=adder(2)
val add_3=adder(3)
val add_4=adder(4)
/*
add_1(List(1)++List(1))
add_1(List(0)++List(0))
add_1(List(1)++List(0))
add_1(List(0)++List(1))
*/
// Estos 5 de abajo son los casos de prueba presentados en el informe.
add_2(List(1,0)++List(0,1))
add_2(List(1,1)++List(0,1))

add_3(List(1,0,1)++List(0,0,0))
add_3(List(1,0,1)++List(1,0,1))

add_4(List(1,0,1,1)++List(1,0,1,0))



/*
* FIN DE LOS CASOS DE PRUEBA
* /////////////////////////////////////////////////////////////////////////////////////////////
* */
// Se da la posibilidad de optimizar las pruebas del adder usando el siguiente código :

// Esta función retorna la transformación de un número entero positivo a binario en n bits,
// es decir, recibe la cantidad de bits con la que se quiere transformar el número y que número.

//bin : Cantidad de bits ; n : número a transformar
def toBinary(bin : Int, n: Int): List[Int] = {
  if (n < 0) throw new Error("El número debe ser entero positivo")
  else if (n >= math.pow(2,bin).toInt) throw new Error("El número debe poder transformarse con bin bits")

  def convert(binaryParameter : Int, n: Int, li: List[Int]): List[Int] = {
    if (binaryParameter == 0) {li}
    else {
      if (n == 0) {
        convert(binaryParameter - 1,0, 0 :: li)
      } else {
        convert(binaryParameter - 1, n / 2, (n % 2) :: li)
      }
    }
  }
  convert(bin, n, List())
}

// Esta función recibe un unico parametro n entero positivo, y retorna una
// matriz de tamaño (2^n,n) donde esta contendra todas las posibles combinaciones de n bits.

// por ejemplo : generate_allCombinations_of_n_bits(2) =
// List(List(00),List(01),List(10),List(11))

def generate_allCombinations_of_n_bits(n : Int) : List[List[Int]] = {

  val limitN = math.pow(2,n).toInt

  def iterative(limit : Int, li : List[List[Int]]) : List[List[Int]] = {

    if(limit < 0){
      li
    } else {
      iterative(limit-1,toBinary(n,limit) :: li)
    }
  }

  iterative(limitN - 1, List())

}

// Esta es la función final, la cual es una función de alto orden, dado que retorna
// una función. sum_in_range toma o recibe una entero positivo n, y calcula todas las posibles,
// combinaciones de bits de n longitud, y retorna una función la cual recibe un número entero,
// positivo number, que se pueda escribir con la cantidad n de bits y retorna una matriz de
// tamañno (2^n,n), donde cada sublista es la suma de ese entero positivo number a (se recibe
// una lista de tamaño 1 o con el valor del entero postivo a sumar)
// cada sublista de la matriz con todas las posibles combinaciones de n bits.

// Además la función le permite controlar el uso de si misma, en cuyo caso que quiera poner el
// número number directamente en binario (lista de 1,0), entonces debe ajustar binaryOrInt = true,
// en caso de que lo quiera en entero, debe ajustar binaryOrInt = false

// Por ejemplo sum_in_range(2,false)(List(0)), transformara 0 a un numero binario de 2 bits y, lo sumara
// a cada binario de la lista List(List(0,0),List(0,1),List(1,0),List(1,1)), lo cual dará:
// List(List(0,1,1),List(0,1,0),List(0,0,1),List(0,0,0)) -> Lista ordenada descendentemente

// Otro ejemplo sum_in_range(2,true)(List(0,0)) será
// List(List(0,1,1),List(0,1,0),List(0,0,1),List(0,0,0))



def sum_in_range(n : Int,binaryOrInt : Boolean) : List[Int] => List[List[Int]] = {

  def sumAux(number : List[Int], bits_of_n_length : List[List[Int]],
             actual : Int, result : List[List[Int]]) : List[List[Int]] = {

    if (actual == math.pow(2,n)){
      result
    }
    else{
      sumAux(number,bits_of_n_length.tail,actual + 1,
        adder(n)(number ++ bits_of_n_length.head)::result)
    }

  }

  def tamList(li : List[Int], counter : Int) : Int = {

    if (li.isEmpty) counter
    else tamList(li.tail,counter + 1)

  }

  def evalInt(number : List[Int]) : List[List[Int]] = {

    if (number.head >= math.pow(2, n)) throw new Error("Number overflow")
    else {
      sumAux(toBinary(n,number.head),generate_allCombinations_of_n_bits(n),0,List())
    }

  }

  def evalBinary(number: List[Int]): List[List[Int]] = {

    if (tamList(number, 0) > n) throw new Error("Number overflow")
    else {
      sumAux(number, generate_allCombinations_of_n_bits(n), 0, List())
    }

  }

  if (binaryOrInt){
    evalBinary
  }
  else {
    evalInt
  }

}

