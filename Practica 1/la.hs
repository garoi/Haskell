data Rule ru = Rule ru ru | EmptyRule

data RewriteSystem rs = RewriteSystem [Rule rs]