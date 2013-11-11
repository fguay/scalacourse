package forcomp

import forcomp.Anagrams._
  

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
 
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> lard  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
    
    val r = List(('b',1), ('r', 1))               //> r  : List[(Char, Int)] = List((b,1), (r,1))
    
   for {
   		(a, b) <- r
   		(x, y) <- lard
   		if( x != a || y - b > 0)
   } yield (if(x != a) (x,y) else (x, y-b))       //> res0: List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1), (a,1), (d,1), (l,
                                                  //| 1))
   
 
 		
  	lard.foldLeft(Map.empty[Char, Int]){
 			(m, x) => {
 				if(x._2 - (r.toMap).getOrElse(x._1, 0) <= 0) m
 				else m + ((x._1, x._2 - (r.toMap).getOrElse(x._1, 0)))
 			}
 		}                                 //> res1: scala.collection.immutable.Map[Char,Int] = Map(a -> 1, d -> 1, l -> 1)
                                                  //| 

   val sentence = List("Linux", "rulez")          //> sentence  : List[String] = List(Linux, rulez)
   val dic = dictionaryByOccurrences              //> dic  : Map[forcomp.Anagrams.Occurrences,List[forcomp.Anagrams.Word]] = Map(L
                                                  //| ist((e,1), (i,1), (l,1), (r,1), (t,2)) -> List(litter), List((a,1), (d,1), (
                                                  //| e,1), (g,2), (l,1), (r,1)) -> List(gargled), List((a,1), (e,1), (h,1), (i,1)
                                                  //| , (k,1), (n,1), (s,3)) -> List(shakiness), List((e,2), (g,1), (n,1)) -> List
                                                  //| (gene), List((a,2), (n,1), (t,1), (y,1)) -> List(Tanya), List((a,1), (d,1), 
                                                  //| (e,2), (h,1), (m,1), (n,2), (o,1), (s,3)) -> List(handsomeness), List((a,2),
                                                  //|  (c,1), (e,2), (k,1), (l,1), (m,1), (p,1), (r,1), (t,1)) -> List(marketplace
                                                  //| ), List((a,1), (i,1), (l,2), (s,1), (v,1)) -> List(villas), List((d,2), (e,1
                                                  //| ), (h,2), (n,1), (r,1), (t,1), (u,1)) -> List(hundredth), List((a,3), (b,1),
                                                  //|  (c,1), (h,1), (i,2), (l,1), (o,1), (p,2), (r,1), (t,1), (y,1)) -> List(appr
                                                  //| oachability), List((d,1), (e,2), (l,1), (s,1), (t,2)) -> List(settled), List
                                                  //| ((a,1), (g,1), (i,3), (l,1), (n,2), (t,1), (z,1)) -> List(Latinizing), List(
                                                  //| (a,1), (m,1), (n,1), (o,
                                                  //| Output exceeds cutoff limit.

  
    
    
 def analoop(occ : Occurrences) : List[Sentence] = {

			if(occ.isEmpty) {
				List(Nil)
			} else {
			    for {
				  c <- combinations(occ)
				  w <- dic.getOrElse(c, List())
				  o <-  {
				        analoop(subtract(occ, c))}
			    } yield {
				  w :: o
				}
			}
	   }                                      //> analoop: (occ: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Sentence]
	   analoop(sentenceOccurrences(sentence)) //> res2: List[forcomp.Anagrams.Sentence] = List(List(Zulu, nil, Rex), List(Zulu
                                                  //| , Lin, Rex), List(Zulu, Rex, nil), List(Zulu, Rex, Lin), List(null, Uzi, Rex
                                                  //| ), List(null, Rex, Uzi), List(Uzi, null, Rex), List(Uzi, Rex, null), List(ni
                                                  //| l, Zulu, Rex), List(nil, Rex, Zulu), List(Lin, Zulu, Rex), List(Lin, Rex, Zu
                                                  //| lu), List(Linux, rulez), List(Rex, Zulu, nil), List(Rex, Zulu, Lin), List(Re
                                                  //| x, null, Uzi), List(Rex, Uzi, null), List(Rex, nil, Zulu), List(Rex, Lin, Zu
                                                  //| lu), List(rulez, Linux))
	   
	   
    
		
	
	
}