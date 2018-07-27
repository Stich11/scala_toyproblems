
def duplicate(l: List[Char]): List[Char] = {
  def iter(l: List[Char],acc: List[Char]): List[Char] = {
    if(l.isEmpty) return acc
    iter(l.tail,acc ::: List(l.head,l.head))
  }
  iter(l,List())
}

def duplicateN(nth: Int,l: List[Char]): List[Char] = {
  def iter(l: List[Char],acc: List[Char]): List[Char] = {
    if(l.isEmpty) return acc
    def iter2(acc: List[Char],nth: Int,cc: Char): List[Char] = {
      if(nth == 0) return acc
      iter2(acc ::: List(cc),nth - 1,cc)
    }
    val l2 = iter2(List(),nth,l.head)
    iter(l.tail,acc ::: l2)
  }
  iter(l,List())
}

def encode(l: List[Char]) = {
  val p = pack(l)
  def iter(p: List[List[Char]],acc: List[(Int,Char)]): List[(Int,Char)] = {
    if(p.isEmpty) return acc
    val length = p.head.length
    val char   = p.head.head
    iter(p.tail,acc ::: List((length,char)))
  }
  iter(p,List())
}
def encodemodified(e: List[Char]) = {
  val l = encodedriect(e)
  def iter(l: List[(Int,Char)],acc: List[Any]): List[Any] = {
    if(l.isEmpty) return acc
    val(c1,c2) = l.head
    if(c1 == 1) iter(l.tail,acc ::: List(c2))
    else iter(l.tail,acc ::: List((c1,c2)))
  }
  iter(l,List())
}
def decode(l: List[(Int,Char)]) = {
  def iter(l: List[(Int,Char)],acc: List[Char]): List[Char] = {
    if(l.isEmpty) return acc
    def reiter(acc: List[Char],n: Int,c: Char): List[Char] = {
      if(n == 0) return acc
      reiter(acc ::: List(c),n - 1,c)
    }
    val(c1,c2) = l.head
    val nl = reiter(List(),c1,c2)
    iter(l.tail,acc ::: nl)
  }
  iter(l,List())
}
def encodedriect(l:List[Char]) = {
  def iter(l: List[Char],cc: Char, acc: List[(Int,Char)],cl: (Int,Char)): List[(Int,Char)] = {
    if(l.isEmpty) return acc :+ cl
    if(cc == l.head) {
      val(c1,c2) = cl
      iter(l.tail,cc,acc,(c1 + 1,c2))
    }
    else {
      iter(l.tail,l.head,acc :+ cl,(1,l.head))
    }
  }
  iter(l.tail,l.head,List(),(1,'e'))
}
def last(list: List[Int]): Int = {
  if(list.tail.isEmpty) return list.head
  last(list.tail)
}
def penultimate(list: List[Int]): Int = {
  if(list.tail.tail.isEmpty) return list.head
  penultimate(list.tail)
}
def nth(n: Int,list: List[Int]): Int = {
  if(n == 0) return list.head
  nth(n - 1,list.tail)
}
def length(list: List[Int]): Int = {
  def loop(acc: Int,list: List[Int]): Int = {
    if(list.isEmpty) return acc
    loop(acc + 1,list.tail)
  }
  loop(-1,list)
}
def reverse(list: List[Int]): List[Int] = {
  list.reverse
}
def ispalindrom(list: List[Int]): Boolean = {
  val l = list.length
  val (left,rp) = list.splitAt(l / 2)
  val right = rp.reverse
  def loop(n: Int, sofar: Boolean): Boolean = {
    if(n == 0) sofar
    if(nth(n - 1,left) != (n - 1,right)) return false
    true
  }
  loop(l / 2 - 1,sofar = true)
}
def flatten(ls: List[Any]): List[Any] = {
  def flatten2(ls: List[Any],acc: List[Any]): List[Any] = {
    if(ls.isEmpty) return acc
    var r = List(List(1,2),1)
    ls.head match {
      case a: List[Int] => r = acc ::: a
      case a: Int => r = acc ::: List(a)
    }
    flatten2(ls.tail,r)
  }
  flatten2(ls,List())
}
def compress(l:List[Char]) = {
  def iter(l: List[Char],cl: Char, acc: List[Char]): List[Char] = {
    if(l.isEmpty) return acc
    if(cl == l.head) {iter(l.tail,cl,acc)}
    else {iter(l.tail,l.head,acc ::: List(l.head))}
  }
  iter(l.tail,l.head,List(l.head))
}
def pack(l:List[Char]) = {
  def iter(l: List[Char],cc: Char, acc: List[List[Char]],cl: List[Char]): List[List[Char]] = {
    if(l.isEmpty) return acc :+ cl
    if(cc == l.head) {iter(l.tail,cc,acc,cl ::: List(l.head))}
    else {iter(l.tail,l.head,acc :+ cl,List(l.head))}
  }
  iter(l.tail,l.head,List(),List('e'))
}
def split(n: Int,L: List[Any]) = {
  def iter(n: Int,L: List[Any],acc: List[Any]): (List[Any],List[Any]) = {
    if(L.isEmpty) throw sys.error("index is larger than the list")
    if(n <= 0) return (acc.reverse,L)
    iter(n - 1,L.tail,L.head :: acc)
  }
  iter(n,L,List())
}

def slice(n: Int,i: Int,L: List[Any]) = {
  def iter(accnum: Int,n: Int,i: Int,L: List[Any],acc: List[Any]): List[Any] = {
    if(L.isEmpty) return acc.reverse
   if(accnum >= n && accnum < i) return iter(accnum + 1,n,i,L.tail,L.head :: acc)
    iter(accnum + 1,n,i,L.tail,acc)
  }
  iter(0,n,i,L,List())
}

def RemoveAt(n: Int,l: List[Char]):(List[Char],Char) = {
  def iter(acn: Int,n: Int,l: List[Char], acc: List[Char]): (List[Char],Char) = {
    if(acn >=  n) return (acc ::: l.tail,l.head)
    iter(acn + 1,n,l.tail,l.head :: acc)
  }
  iter(0,n,l,List())
}


val codel = List((3,'e'), (2,'o'), (1,'a'), (1,'e'), (1,'r'))
val cl = List('e','e','e','o','o','a','e','r')
val sl = List('e','a','b','l')
val lol = List(List(7,5),1,1,2,3,4,5,8)
val tl = List(1,1,2,3,4,5,8)
val ntl = List(1,2,2,1)
val ctl = ntl ::: tl
val e = tl.head

assert(RemoveAt(1,sl) == (List('e','b','l'),'a'))
assert(encode(cl) == List((3,'e'), (2,'o'), (1,'a'), (1,'e'), (1,'r')))
assert(encodemodified(cl) == List((3,'e'), (2,'o'), 'a', 'e', 'r'))
assert(decode(codel) == List('e', 'e', 'e', 'o', 'o', 'a', 'e', 'r'))
assert(encodedriect(cl) == List((3,'e'), (2,'o'), (1,'a'), (1,'e'), (1,'r')))
assert(compress(cl) == List('e', 'o', 'a', 'e', 'r'))
assert(pack(cl) == List(List('e', 'e', 'e'), List('o', 'o'), List('a'), List('e'), List('r')))
assert(flatten(lol) == List(7, 5, 1, 1, 2, 3, 4, 5, 8))
assert(last(tl) == 8)
assert(penultimate(tl) == 5)
assert(nth(2,tl) == 2)
assert(length(tl) == 6)
assert(reverse(tl)== List(8, 5, 4, 3, 2, 1, 1))
assert(ispalindrom(tl) == false)
assert(duplicate(sl) == List('e','e','a','a','b','b','l','l'))
assert(duplicateN(3,sl) == List('e','e','e','a','a','a','b','b','b','l','l','l'))
assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))














