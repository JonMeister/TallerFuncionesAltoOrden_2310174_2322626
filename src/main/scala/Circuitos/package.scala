package object Circuitos {

  type Chip=List[Int]=>List[Int]

  def crearChipUnario (funcion:Int=>Int): Chip={
    input=>List(funcion(input.head));
  }


  def crearChipBinario (funcion:(Int,Int)=>Int): Chip={
    input=>List(funcion(input.head,input.tail.head));
  }

  val chip_not: Chip = crearChipUnario((x: Int) => (1 - x))

  val chip_and: Chip = crearChipBinario((x: Int, y: Int) => (x * y))

  val chip_or: Chip = crearChipBinario((x: Int, y: Int) => (x + y - x * y))

  def half_adder: Chip = {

    input =>
      val A = input.head
      val B = input.tail.head

      val C = chip_and(List(A, B)).head

      val S = chip_and(List(chip_or(List(A, B)).head, chip_not(List(C)).head)).head

      List(C, S)
  }

  def full_adder: Chip ={
    input=>
      val A = input.head
      val B = input.tail.head
      val Cin = input.tail.tail.head

      val first_half_adder = half_adder(List(B,Cin))
      val C1 = first_half_adder.head
      val S1 = first_half_adder.tail.head

      val second_half_adder = half_adder(List(A, S1))
      val S = second_half_adder.tail.head
      val C2 = second_half_adder.head

      val Cout=chip_or(List(C1,C2)).head

      List(Cout,S)
  }

  def adder(n:Int): Chip= { // NOT WORKING, #SENDHELP

    input =>
      val first_number = input.take(n)
      val second_number = input.drop(n)

      def recursive_adder(A:List[Int],B:List[Int],Cr:Int): List[Int]={
        if (A.isEmpty)
          List(Cr)
        else
          val results=full_adder(List(A.head,B.head,Cr))
          val Cout=results.head
          val S=results.tail.head

          recursive_adder(A.tail,B.tail,Cout)++List(S) //We're not suposed to use ++ to concatenate lists

      }
      recursive_adder(first_number,second_number,0)
  }
}
