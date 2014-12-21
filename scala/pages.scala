class Pages(val maxKeywords:Int,val pages:Array[String]) {
	def classifyAll(){
		pages.map (p=> 
				val page = new Page(p,maxKeywords)
				page.classify()
			)
	}
}