import scala.util.Try
 

// A abstract base class for Page & query resposible for tokenizing the content and weight the tokens.
 abstract class Scorable(content: String,maxKeywords:Int) {
	var tokens: Array[String] = Array[String]()
	var tokenWeight: List[(String,Int)] = List[(String,Int)]()

	def tokenize() : Array[String] =   {
		tokens = content.split(' ')
		tokens
	}

	def weight() : List[(String,Int)] =  {
		weightTokens(tokenize())
	}

	def weight(tokenList: Array[String]) : List[(String,Int)] =  {
		tokens = tokenList
		weightTokens(tokens)
	}

	// This method assigns weights to individual tokens based on the order it came
	private def weightTokens(tokens: Array[String]) : List[(String,Int)] = {
		tokenWeight = tokens.foldLeft(List[(String,Int)]()) { (acc,item) =>
			acc.size match {
				case 0 => List(item->maxKeywords)
				case _ =>
					val (k,v) = acc.last
					acc :+ (item -> (v-1))
			}
		}
		tokenWeight
	} 

	


}

// Base class for Page entity. It does 2 things
// - Index the given content
// - Search the given query against the page
// We can easily extend this to support nested pages 
class Page(content: String,maxKeywords:Int)  extends Scorable(content,maxKeywords) {
	// var nestedPages: List[Scorable] = List[Scorable]()
	// It searches the given query on the page by tokenizing the query.
	// Once it knows about the weight of the tokens, it ll calculate the score by using the query weight and its own weight
	def scoreFor(q:String) : Int = {
		val query = new Query(q,maxKeywords)
		query.weight()
		calculateScore(query.tokenWeight)
	}

	// def addPage(page:Scorable) = {
	// 	nestedPages :+= page
	// }

	// def addPages(pages:Array[Scorable]) = {
	// 	nestedPages = pages
	// }

	// This method triggers the score calcultion and sums up all the hits on the page
	private def calculateScore(queryWeight:List[(String,Int)]) : Int = {
		var r = queryWeight.map (qw => {
			calculateHit(qw)
		})
		r.sum

	}


	// This method find the weights of the hit queries and calculates the individual 
	// score of the token by multiplying its own weight with pages weight
	private def calculateHit(qw:(String,Int)) : Int = {
		val qWeight = qw._2
		val qKey  = qw._1
		val hitToken = tokenWeight.find(k => k._1 == qKey)
		if (hitToken.size > 0){
			val (pKey,pWeight) = hitToken.last
			pWeight*qWeight
		}else
			0

	}


}

// Base class for Query entity. Right now it doesnt have any specific actions. But we can implement
// any query specific calculation here. For an example, during tokenizing, we can remove the stopwords here(commented part)
class Query(content: String,maxKeywords:Int)  extends Scorable(content,maxKeywords)  {
	// override def tokenize() : Array [String] = {
	// 	super.tokenize()
	// 	tokens = cleanupStopWords(Array("i","the","is","am","an"))
	// 	tokens
	// }

	// private def cleanupStopWords(stoppers: Array[String]) : Array[String]  =  {
	// 	tokens.filter (ta=> {
	// 		!stoppers.exists (stopper=> stopper==ta)
	// 	})
	// }
}





class Crawler {
	// val pages = Array[String]("Ford Car Review","Review Car","Review Ford","Toyota Car","Honda Car","Car")
	// val queries = Array[String]("Ford","Car","Review","Ford Review","Ford Car","cooking French")
	var maxKeywords:Int = 8
	var maxResults:Int = 5

	// This method runs the given queries all the available pages and returns teh accumulated score
	// of each page against the query.
	def run() : Array[Array[Int]] = {
		
		// Get the user inputs 
		val (pages,queries) = getUserInputs()
		
		// Loop through each query against all the pages to find which page has max no of hits 
		queries.map (q=> { 
			pages.map (p => {   
				val page = new Page(p,maxKeywords)
				page.weight
				page.scoreFor(q) 
			})
		})
	}

	// Read the user input from the stdin
	def getUserInputs() : (Array[String],Array[String]) = {
		println("Please enter your page/query data. ")
		println("Pages follwed by a keyword P and Space")
		println("Queries follwed by a keyword Q and Space")
		println("Exit the input by an empty line break")
		
		var inputArr:Array[String] = Array[String]()
		
		// Get the user input for pages & queries until user enters empty line break
		Iterator.continually(Console.readLine).takeWhile(_.nonEmpty).foreach(line=> {
			// println(line)
			inputArr :+= line
		})

		println("Please enter the max keywords per page: Default is 8")
		val readKeys = Try(Console.readInt)
		if (!readKeys.isFailure)
			maxKeywords = readKeys.get

		println("Please enter the max results, default is 5")
		val readResults = Try(Console.readInt)
		if (!readResults.isFailure)
			maxResults = readResults.get

		// Partiotion the given result to pages & queries
		val (pageStr,queryStr) = inputArr.partition(in=>in.take(2)=="P ")
		val pages = pageStr.map(p=>p.tail.trim)
		// Take only the valid queries.
		val queries = queryStr.filter(q=>q.take(2)=="Q ").map(q=>q.tail.trim) 
		
		(pages,queries)
	}

	// It pretty prints the above calculated result. 
	def formatResults(results:Array[Array[Int]])  = {

		// Map the actual page ids (P1,P2,..) to the page score
		val pageMapped=results.map (x=> {
			var res = List[(String,Int)]()
			x.indices.foreach { i => res :+=("P"+(i+1)->x(i))}
			res
		})

		// Sort the mapped pages by score first(descending), page no next (ascending). 
		// This will orders the items in such a way that top score pages will come first. 
		// Also if multiple pages has the same results. It will order them by the page order
		val sorted =  pageMapped.map ( pq=> {
			pq.sortBy (y => (-y._2,y._1)).take(maxResults)
		})

		println("The results are")
		// printing the results with question id mapped
		sorted.indices.foreach (idx=> {
			print("Q" + (idx+1) + ": ")
			sorted(idx).foreach(item=> {
				if (item._2>0){
					print(item._1)
					print(" ")
				}
			})
			println("")
		})


	}
}

val crawler  = new Crawler
val out = crawler.run
crawler.formatResults(out)



