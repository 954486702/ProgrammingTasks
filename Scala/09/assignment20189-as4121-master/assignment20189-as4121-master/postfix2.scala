// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// let- and right-associativity
abstract class Assoc
case object RA extends Assoc
case object LA extends Assoc

// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (8) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.
def prec(op1: String, op2: String) : Boolean = {
	if (assoc(op1) == LA && precs(op1) <= precs(op2)) true
	else if (assoc(op1) == LA && precs(op1) < precs(op2)) true
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
			if (ops.contains(toks.head)) {
				if (!st.isEmpty && st.head != "(" && prec(toks.head, st.head)) {
					//ReTurns longest prefix whose elements satisfy prec
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

//correct to move powers along 


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +



// (9) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Long] = Nil) : Long = toks match {
case Nil => st.head
case x::xs => x match{
	case "*" => compute(xs, (st(1)*st(0)) :: st.drop(2))
	case "+" => compute(xs, (st(1)+st(0)) :: st.drop(2))
	case "-" => compute(xs, (st(1)-st(0)) :: st.drop(2))
	case "/" => compute(xs, (st(1)/st(0)) :: st.drop(2))
	case "^" => compute(xs, (Math.pow(st(1), st(0)).toLong) :: st.drop(2))
	case num => compute(xs, x.toLong :: st)
	}
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

