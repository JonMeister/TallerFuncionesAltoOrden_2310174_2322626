import Circuitos.{Chip,crearChipUnario,crearChipBinario,half_adder,full_adder,adder}

val chip_not=crearChipUnario((x:Int)=>(1-x))
chip_not(List(0))
chip_not(List(1))


val chip_and=crearChipBinario((x:Int,y:Int)=>(x*y))
chip_and(List(0,0))
chip_and(List(0,1))
chip_and(List(1,0))
chip_and(List(1,1))


val chip_or=crearChipBinario((x:Int,y:Int)=>(x+y-x*y))
chip_or(List(0,0))
chip_or(List(1,0))
chip_or(List(0,1))
chip_or(List(1,1))

half_adder(List(1,1))

full_adder(List(1,1,0))
val add_1=adder(1)
val add_2=adder(2)
val add_3=adder(3)
val add_4=adder(4)

add_1(List(1)++List(1))//Works
add_1(List(0)++List(0))//Works
add_1(List(1)++List(0))//Works
add_1(List(0)++List(1))//Works


add_2(List(1,0)++List(0,1))//Works
add_2(List(1,1)++List(0,1))//Doesn't work, something's wrong :(

add_3(List(1,0,1)++List(0,0,0))//Works
add_3(List(1,0,1)++List(1,0,1))//Works

add_4(List(1,0,1,1)++List(1,0,1,0))//Doesn't work, I gave up :'(