class Query(val content: String,val maxKeywords:Int)  extends Scorable(content,maxKeywords)  {
	override def tokenize : Array [String] = {
		super.tokenize()
		cleanupStopWords(Array("i","the","is","am","an"))
	}

	private def cleanupStopWords(stoppers: Array[String]) =  {
		tokens.filter(t=>!stoppers.exists(stopper=>stopper==t))
	}
}