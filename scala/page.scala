class Page(val content: String,val maxKeywords:Int)  extends Scorable(content,maxKeywords) {
	def search(q:String) = Int {
		val query = new Query(q,maxKeywords)
		query.classify()
		calculateHit(query.getWeight())
	}

	private def calculateHit(queryWeight:Map[String,Int]) = {
		queryWeight.map (qw => {
			calculateScore(qw)
		})

	}

	def calculateScore(qw:(String,Int)) : Int = {
		val pWeight = qw._2
		val pWeight = keyWeight.get(qToken)
		if (pWeight!=null)
			pWeight*pWeight

	}


}