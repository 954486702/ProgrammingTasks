// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================


// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (6) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 
/*def is_op(op: String) : Boolean = {
	if (ops.contains(op)) true
	else false  
}*/

def is_op(op: String) : Boolean = {
	op match {
	      case "+" => true
	      case "-" => true
	      case "*" => true
	      case "/" => true
	      case "^" => true
	      case _ => false
	    }
}


def prec(op1: String, op2: String) : Boolean = {
	if (precs(op1) <= precs(op2)) true
	else false
}

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
	toks match {
		case Nil =>
					st match {
								case Nil => out
								case _ => syard(toks, st.tail, out :+ st.head)
					}
		case _ =>
			if (is_op(toks.head)) {
				if (!st.isEmpty && st.head != "(" && prec(toks.head, st.head)) {
					val op1 = st.takeWhile(o => prec(toks.head, o))
					syard(toks.tail, toks.head :: st.diff(op1), out ++ op1)
				}
				else {
					syard(toks.tail, toks.head :: st, out)
					}
			}
		else {
			toks.head match {
				case "(" => syard(toks.tail, toks.head +: st, out)
				case ")" => syard(toks.tail, st.dropWhile(_ != "(").tail, out ++ st.takeWhile(_ != "("))
				case _ => syard(toks.tail, st, out :+ toks.head)
				}
			} 		
	}		
}

// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
/*
toks = 3 4 2 1 - * +
st = Nil
..
toks = 4 2 1 - * +
st = 3
..
toks = 2 1 - * +
st = 4 3
..
toks = 1 - * +
st = 2 4 3
..
toks = - * +
st = 1 2 4 3
..
toks = * +
st = 1 4 3
..
toks = +
st = 4 3
..
toks = Nil
st = 12

*/
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (7) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.  xs  

def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
case Nil => st.head
case x::xs => x match{
	case "*" => compute(xs, (st(1)*st(0)) :: st.drop(2))
	case "+" => compute(xs, (st(1)+st(0)) :: st.drop(2))
	case "-" => compute(xs, (st(1)-st(0)) :: st.drop(2))
	case "/" => compute(xs, (st(1)/st(0)) :: st.drop(2))
	case num => compute(xs, x.toInt :: st)
	}
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15




