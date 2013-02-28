package s99

import scala.annotation.tailrec
import Solutions._
import scala.util.Random

trait ListsSolutions {

  def last[T](list: List[T]): T = {
  
    @tailrec
    def last0[T](list: List[T]): T = 
      list match {
        case last :: Nil => last
        case head :: tail => last0(tail)
      }
    
    list match { 
      case Nil => null.asInstanceOf[T]
      case _ => last0(list) 
    }
  }
  
  def penultimate[T](list: List[T]): T = {

    @tailrec
    def penultimate0[T](list: List[T], prev: T): T = 
      list match {
        case last :: Nil => prev
        case head :: tail => penultimate0(tail, head)
      }
    
    list match { 
      case Nil => null.asInstanceOf[T]      
      case last :: Nil => null.asInstanceOf[T]
      case _ => penultimate0(list.tail, list.head) 
    }  
  }
  
  def nth[T](n: Int, list: List[T]): T = {

    @tailrec
    def nth0[T](n: Int, list: List[T]): T =
      list match {
        case Nil => null.asInstanceOf[T]
        case head :: tail if (n > 0) => nth0(n - 1, tail)
        case head :: _ if (n == 0) => head          
      }
    
    if (n < 0) throw new IllegalArgumentException("n=" + n)
    else nth0(n, list)
  }
  
  def length[T](list: List[T]): Int = {

    @tailrec
    def length0[T](n: Int, list: List[T]): Int =
      list match {
        case Nil => n
        case head :: tail => length0(n + 1, tail)
      }
    
    length0(0, list)
  }
  
  def reverse[T](list: List[T]): List[T] = {

    @tailrec
    def reverse0[T](list: List[T], acc: List[T]): List[T] =
      list match {
        case Nil => acc
        case head :: tail => reverse0(tail, head :: acc)
      }
    
    reverse0(list, Nil)    
  }
  
  def isPalindrome[T](list: List[T]): Boolean = {
    
    @tailrec
    def isPalindrome0[T](list: List[T], acc: List[T]): Boolean = {
      var tail: List[T] = null
      if (list == acc) true
      else if ({tail = list.tail; tail} == acc) true
      else isPalindrome0(tail, list.head :: acc)
    }
    
    isPalindrome0(list, Nil)
  }
  
  def flatten(list: List[Any]): List[Any] = {

    @tailrec
	def flatten0(list: Any, acc: List[Any], listTail: List[Any]): List[Any] = 
	  list match {
		case Nil => 
		  if (listTail == Nil) acc else flatten0(listTail, acc, Nil);
		case (headList: List[Any]) :: Nil => 
		  flatten0(headList.head, acc, if (headList.tail != Nil) headList.tail :: listTail else listTail)
		case head :: Nil => 
		  flatten0(Nil, head :: acc, listTail)         
		case Nil :: tail => 
		  flatten0(tail, acc, listTail)               
		case (headList: List[Any]) :: tail => 
		  flatten0(headList.head, acc, headList.tail :: tail :: listTail)
		case head :: tail => 
		  flatten0(tail, head :: acc, listTail)  
		case el =>
		  flatten0(Nil, el :: acc, listTail)        	            
	  }

	val result = reverse(flatten0(list, Nil, Nil))
	result
  }
  
  def compress[T](list: List[T]): List[T] = {
    
    @tailrec
    def compress0(list: List[T], acc: List[T], lastHead: T): List[T] = 
      list match {
        case Nil => acc
        case h :: t =>
          if (h == lastHead) compress0(t, acc, lastHead)
          else compress0(t, h :: acc, h)
      }
       
    if (list != Nil) reverse(compress0(list.tail, List(list.head), list.head))
    else list
  }
   
  def pack[T](list: List[T]): List[List[T]] = {
    
    @tailrec
    def pack0(list: List[T], acc: List[List[T]], lastAcc: List[T], lastHead: T): List[List[T]] = 
      list match {
        case Nil => 
          if (lastAcc != Nil) lastAcc :: acc
          else acc
        case h :: t =>
          if (h == lastHead) pack0(t, acc, h :: lastAcc, lastHead)
          else pack0(t, lastAcc :: acc, List(h), h)
      }
       
    if (list != Nil) reverse(pack0(list.tail, Nil, List(list.head), list.head))
    else List(Nil)
  }
  
  def encode[T](list: List[T]): List[(Int, T)] = {
    
    @tailrec
    def encode0(list: List[T], acc: List[(Int, T)], lastHead: T, lastNum: Int): List[(Int, T)] = 
      list match {
        case Nil => 
          (lastNum, lastHead) :: acc        
        case h :: t =>
          if (h == lastHead) encode0(t, acc, lastHead, lastNum + 1)
          else encode0(t, (lastNum, lastHead) :: acc, h, 1)
      }
       
    if (list != Nil) reverse(encode0(list.tail, Nil, list.head, 1))
    else Nil 
  }
  
  def encodeModified[T](list: List[T]): List[Any] = {
        
    @tailrec
    def encodeModified0(list: List[T], acc: List[Any], lastHead: T, lastNum: Int): List[Any] = {
      def modify(lastNum: Int, lastHead: T) =
        if (lastNum == 1) lastHead
        else (lastNum, lastHead)
      list match {
        case Nil => 
          modify(lastNum, lastHead) :: acc        
        case h :: t =>
          if (h == lastHead) encodeModified0(t, acc, lastHead, lastNum + 1)
          else encodeModified0(t, modify(lastNum, lastHead) :: acc, h, 1)
      }
    }
       
    if (list != Nil) reverse(encodeModified0(list.tail, Nil, list.head, 1))
    else Nil     
  }
  
  def decode[T](list: List[(Int, T)]): List[T] = {
    
    @tailrec
    def decode0(list: List[(Int, T)], acc: List[T]): List[T] = {
      @tailrec
      def fill(num: Int, el: T, acc: List[T]): List[T] = {
        if (num > 0) fill(num - 1, el, el :: acc)
        else acc
      }
      list match {
        case Nil => acc        
        case h :: t => decode0(t, fill(h._1, h._2, Nil) ::: acc)
      }
    }
       
    if (list != Nil) reverse(decode0(list, Nil))
    else Nil     
  }
  
  /**
   * Opposite - I used <code>pack()</code> because in previous solutions I didn't.
   */
  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    
    @tailrec
    def encodeDirect0(list: List[List[T]], acc: List[(Int, T)], scat: List[T], accNum: Int, accEl: T): List[(Int, T)] = 
      (list, scat) match {
        case (Nil, Nil) => if (accNum > 0) (accNum, accEl) :: acc else acc
        case (l, Nil) => encodeDirect0(l.tail, if (accNum > 0) (accNum, accEl) :: acc else acc, l.head, 0, null.asInstanceOf[T])
        case (l, a) => encodeDirect0(l, acc, a.tail, accNum + 1, a.head)        
      }
    reverse(encodeDirect0(pack(list), Nil, Nil, 0, null.asInstanceOf[T]))    
  }
  
  def duplicate[T](list: List[T]): List[T] = {
    
    @tailrec
    def duplicate0(list: List[T], acc: List[T]): List[T] = 
      list match {
        case Nil => acc
        case h :: t => duplicate0(t, h :: h :: acc)
      }
    
    reverse(duplicate0(list, Nil))
  }
  
  def duplicateN[T](n: Int, list: List[T]): List[T] = {
    
    @tailrec
    def duplicateN0(list: List[T], acc: List[T], n: Int): List[T] = {
      
      @tailrec
      def fill(num: Int, el: T, acc: List[T]): List[T] = {
        if (num > 0) fill(num - 1, el, el :: acc)
        else acc
      }
      
      list match {
        case Nil => acc
        case h :: t => duplicateN0(t, fill(n, h, Nil) ::: acc, n)
      }
    }
    
    reverse(duplicateN0(list, Nil, n))    
  }
  
  def drop[T](n: Int, list: List[T]): List[T] = {
    
    @tailrec
    def drop0(list: List[T], acc: List[T], currN: Int, n: Int): List[T] = 
      list match {
        case Nil => acc
        case h :: t => drop0(t, if (currN > 0) h :: acc else acc, if (currN > 0) currN - 1 else n - 1, n)
      }

    reverse(drop0(list, Nil, n - 1, n))
  }
  
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {

    @tailrec
    def split0(list: List[T], acc0: List[T], acc1: List[T], n: Int): (List[T], List[T]) = 
      list match {
        case Nil => (acc0, acc1)
        case h :: t => 
          if (n > 0) split0(t, h :: acc0, acc1, n - 1)
          else split0(t, acc0, h :: acc1, n - 1)
      }

    val (l, r)  = split0(list, Nil, Nil, n)
    (reverse(l), reverse(r))  
  }
  
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = {

    @tailrec
    def slice0(list: List[T], acc: List[T], n: Int, i: Int, k: Int): List[T] = 
      list match {
        case Nil => acc
        case h :: t => 
          if (n >= i && n < k) slice0(t, h :: acc, n + 1, i, k)
          else slice0(t, acc, n + 1, i, k)
      }

    reverse(slice0(list, Nil, 0, i, j))
  }
  
  def rotate[T](n: Int, list: List[T]): List[T] = {
    
    @tailrec 
    def len0(list: List[T], acc: Int): Int = 
      list match {
        case Nil => acc
        case h :: t => len0(t, acc + 1)
      }
    
    @tailrec
    def rotate0(list: List[T], acc: List[T], n: Int): List[T] = 
      list match {
        case Nil => 
          if (n > 0) rotate0(reverse(acc), Nil, n)  
          else list ::: reverse(acc)
        case h :: t =>
          if (n > 0) rotate0(t, h :: acc, n - 1)
          else list ::: reverse(acc) 
      }  
    rotate0(list, Nil, if (n > 0) n else len0(list, 0) + n)
  }
  
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = {
    
    @tailrec
    def removeAt0(list: List[T], acc: List[T], n: Int, el: T): (List[T], T) = 
      list match {
        case Nil => (acc, el)
        case h :: t => 
          if (n != 0) removeAt0(t, h :: acc, n - 1, el)
          else removeAt0(t, acc, n - 1, h) 
      }
    
    val (l, el) = removeAt0(list, Nil, i, null.asInstanceOf[T])
    (reverse(l), el)
  }
  
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = {

    @tailrec
    def insertAt0(list: List[T], acc: List[T], n: Int, el: T): List[T] = 
      list match {
        case Nil => acc
        case h :: t => 
          if (n != 0) insertAt0(t, h :: acc, n - 1, el)
          else insertAt0(t, h :: el :: acc, n - 1, el)
      }
    
    reverse(insertAt0(list, Nil, i, t))   
  }
  
  def range[T](i: Int, j: Int): List[Int] = {
    
    @tailrec
    def range0(i: Int, j: Int, acc: List[Int]): List[Int] = 
      if (i <= j) range0(i + 1, j, i :: acc)
      else acc
      
    reverse(range0(i, j, Nil))
  }
  
  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    
    @tailrec 
    def len0(list: List[T], acc: Int): Int = 
      list match {
        case Nil => acc
        case h :: t => len0(t, acc + 1)
      }
    
    val len = len0(list, 0)
    
    val rand = new Random

    @tailrec
    def randomSelect0(list: List[T], i: Int): List[T] = {
      if (i > 0) randomSelect0(removeAt(rand.nextInt(len - (len - n - i)), list)._1, i - 1)
      else list
    }
 
    randomSelect0(list, len - n)      
  }
  
  def lotto[T](i: Int, j: Int): List[Int] = {
    
    val rand = new Random
    
    @tailrec
    def contains0(list: List[Int], n: Int): Boolean = 
      list match {
        case Nil => false
        case h :: t => if (h != n) contains0(t, n) else true
      }
    
    @tailrec
    def nextRand(list: List[Int], next: Unit => Int): List[Int] = {
      val i = next()
      if (!contains0(list, i)) i :: list
      else nextRand(list, next)
    }
    
    @tailrec 
    def lotto0(i: Int, j: Int, acc: List[Int]): List[Int] = {
      if (i > 0) { 
        lotto0(i - 1, j, nextRand(acc, Unit => rand.nextInt(j) + 1))
      } else acc
    }
    
    lotto0(i, j, Nil)
  }
  
  def randomPermute[T](list: List[T]): List[T] = {
    
    @tailrec 
    def len0(list: List[T], acc: Int): Int = 
      list match {
        case Nil => acc
        case h :: t => len0(t, acc + 1)
      }
    
    val len = len0(list, 0)
    
    randomSelect(len, list)
  }
  
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    
    def combinations0(n: Int, orig: List[T], list: List[T], acc: List[List[T]]): List[List[T]] = {
      if (n > 1) {
        list match {
          case Nil => acc
          case h :: t => pack0(h, combinations0(n - 1, orig, t, Nil), combinations0(n, orig, t, Nil))
        }        
      } else {
        list match {
          case Nil => acc
          case h :: t => combinations0(n, orig, t, List(h) :: acc)
        }
      }
    }
    
    @tailrec
    def pack0(el: T, to: List[List[T]], acc: List[List[T]]): List[List[T]] = 
      to match {
        case Nil => acc
        case h :: t => pack0(el, t, (el :: h) :: acc)
      }
    
    require(n >= 0)
    if (n >= 1) combinations0(n, list, list, List(Nil))
    else List(Nil)
  }
  
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

