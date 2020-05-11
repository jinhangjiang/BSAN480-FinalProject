create table TAG as select m.movieid, t.tag, s.relevance
from movie m 
join ml_genome_scores s on m.movieid = s.movieid 
join ml_genome_tags t on s.tagid = t.tagid;