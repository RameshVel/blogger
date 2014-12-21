# Written by Ramesh Vel 
# Nov 1, 2014


#This method responsible for assigning weights to the keywords in the pages as well the keywords in the question as per the given order
#Output weight of the each keywords in page & question is the key for calcualting the total strength of the hits in the page
def calculate_weight(pages_or_queries)
	pages_or_queries.map { |page|
		n=8
		page.map {|key|
			res = [key,n]
			n-=1
			res
		}
	}.map {|x| Hash[*x.flatten] }
end

#This method calculates strength of the each keywords against the page & the given order
#This is done by multiplying the page weight & keyword weight in the already calculated order
#Each questions strength against the page is calculated by summing the total weight of all keywords(those hit) in the page
def calculate_key_strength(page_weights,query_weights)
	page_weights.map {|page|
		query_weights.map {|query|
			query.map {|k,v|
				[page[k]*v] unless page[k].nil?
			}
		}.map(&:flatten).map(&:compact).map {|x| x.reduce(0, :+)}
	}.transpose
end

def format_result(result)
	p_mapped = result.map {|arr|
		n=0
		arr.map {|item|
			n+=1
			next if item==0
			["P#{n}",item]
		}
	}.map(&:compact).map {|x| Hash[*x.flatten]}
	p_mapped = p_mapped.map {|x| x.sort_by {|k,v| [-v,k]}}.map(&:transpose).map(&:first)
	puts "Mapped pages : #{p_mapped}" 
	q=0
	pq_mapped = p_mapped.map {|pages|
		q+=1
		"Q#{q}: #{pages.join ' ' if pages}"
	}

	puts "\nThe results are \n\n" 
	puts pq_mapped.join " \n"


end

pages = []
pages << %w(Ford Car Review)
pages << %w(Review Car)
pages << %w(Review Ford)
pages << %w(Toyota Car)
pages << %w(Honda Car)
pages << %w(Car)

queries = []
queries << %w(Ford)
queries << %w(Car)
queries << %w(Review)
queries << %w(Ford Review)
queries << %w(Ford Car)
queries << %w(cooking French)


page_weight = calculate_weight(pages)
puts "Page key weight : #{page_weight}"
query_weight = calculate_weight(queries)
puts "Queries key weight : #{query_weight}"
final_strength  = calculate_key_strength(page_weight,query_weight)
puts "Calculated strength for given keys : #{final_strength}"
format_result final_strength

