package object Circuitos {

  type Chip=List[Int]=>List[Int]

  def crearChipUnario (funcion:Int=>Int): Chip={
    (input : List[Int])=>List(funcion(input.head));
  }


  def crearChipBinario (funcion:(Int,Int)=>Int): Chip={
    (input : List[Int])=>List(funcion(input.head,input.tail.head));
  }

  val chip_not: Chip = crearChipUnario((x: Int) => (1 - x))

  val chip_and: Chip = crearChipBinario((x: Int, y: Int) => (x * y))

  val chip_or: Chip = crearChipBinario((x: Int, y: Int) => (x + y - x * y))

  def half_adder: Chip = {
    // let input as List[Int] -> length 2
    (input : List[Int]) =>
      val A = input.head
      val B = input.tail.head

      val C = chip_and(List(A, B)).head

      val S = chip_and(List(chip_or(List(A, B)).head, chip_not(List(C)).head)).head

      List(C, S)
  }

  def full_adder: Chip = {

    (li: List[Int]) => {

      val half = half_adder

      val sum_B_Cin = half(List(li.tail.head, li.tail.tail.head))

      val sum_B_Cin_A = half(List(li.head, sum_B_Cin.tail.head))

      val SUM = sum_B_Cin_A.tail.head

      val or = crearChipBinario((x: Int, y: Int) => x + y - (x * y))(List(sum_B_Cin_A.head, sum_B_Cin.head))

      val Cout = or.head

      List(Cout, SUM)

    }

  }

  def llevarSuma(cut : Int, attemps : Int, listaBinariosA : List[Int],
                 listaBinariosB : List[Int], carry : Int,
                 resultado : List[Int]) : List[Int] = {

    if(attemps == 0){
      carry :: resultado
    } else {
      val suma : List[Int] = full_adder(List(listaBinariosA.drop(cut).head,
        listaBinariosB.drop(cut).head,carry))
      llevarSuma(cut - 1, attemps - 1, listaBinariosA.take(cut),
        listaBinariosB.take(cut),suma.head, suma.tail.head :: resultado)

    }

  }

  def adder(n : Int) : Chip = {

    (li : List[Int]) => {

      val BinaryNumberA = li.take(n) // Toma los primeros n elementos
      val BinaryNumberB = li.drop(n) // Toma los ultimos n elementos

      llevarSuma(n - 1, n, BinaryNumberA, BinaryNumberB, 0, List())


    }


  }
  
}
